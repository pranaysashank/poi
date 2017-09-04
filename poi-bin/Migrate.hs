{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Migrate where

import Control.Exception (try)
import Control.Monad (join, filterM, void)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Int (Int64)
import System.Directory (removeFile, getCurrentDirectory, createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import Text.RawString.QQ (r)

data Mode = Up | Down | Redo | Prepare | New String

data MigrateOpts = MigrateOpts
  { migration :: Mode
  , environment :: ConnectInfo
  }

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "vl"
                                    , connectPassword = "password"
                                    , connectUser = "vacationlabs"
                                    }

type Migration = (String, (Query, Query))

migrate :: MigrateOpts -> [Migration] -> IO ()
migrate opts migrations= do
  conn <- connect $ environment opts
  case migration opts of
    Prepare -> prepareMigration conn
    New migName -> createNewMigration migName
    Up -> upMigration conn migrations
    Down -> downMigration conn migrations
    Redo -> redoMigration conn migrations
  close conn

prepareMigration :: Connection -> IO ()
prepareMigration conn = do
  void $ execute_ conn [r|
CREATE TABLE IF NOT EXISTS schema_migrations (name text
NOT NULL PRIMARY KEY, updated_at timestamp with time zone
NOT NULL DEFAULT now())
|]
  cd <- getCurrentDirectory
  let migrationsD = cd </> "migrations"
  withCurrentDirectory cd (createDirectoryIfMissing False migrationsD)
  writeFile (cd </> "Migrations.hs") [r|#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF migrate-autoimporter #-}

module Migrations where

import Migrate (Mode(..), MigrateOpts(..), migrate, connectionInfo)
import System.Directory
import System.Environment
import System.FilePath

runMigrations :: Mode -> IO ()
runMigrations mode = migrate opts migrations
  where
    opts = MigrateOpts mode connectionInfo

main :: IO ()
main = do
  (e:es) <- getArgs
  case e of
    "prepare" -> runMigrations Prepare
    "up" -> runMigrations Up
    "down" -> runMigrations Down
    "new" -> runMigrations (New $ head es)
|]

createNewMigration :: String -> IO ()
createNewMigration migName = do
  cd <- getCurrentDirectory
  let migrationsD = cd </> "migrations"
  time <- getCurrentTime
  let moduleName = "M" ++ formatTime defaultTimeLocale timeFormat time ++ "_" ++ pascal migName
      migFile = migrationsD </> (moduleName ++ ".hs")
  putStrLn migFile
  writeFile migFile (([r|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
|] ++ "module " ++ moduleName ++ " where \n") ++ fillerText)

upMigration :: Connection -> [Migration] -> IO ()
upMigration conn migrations = do
  migs <- filterM didMigrationRun migrations
  myForM_ migs $ \(name, (up, _)) -> do
    putStrLn ("Running Migration up " ++ name)
    result <- try $ execute_ conn up :: IO (Either SqlError Int64)
    either (\a -> do
               pgDump name
               error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
      (\b -> do
          void $ execute conn [r|INSERT INTO schema_migrations VALUES (?) |] [name]
          putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
      result
  where
    myForM_ :: [Migration] -> (Migration -> IO ()) -> IO ()
    myForM_ [] _ = return ()
    myForM_ [x@ (name, _)] f = do
      f x
      pgDump name
    myForM_ (x:xs) f = do
      f x
      myForM_ xs f

    didMigrationRun :: Migration -> IO Bool
    didMigrationRun (name, _) = do
      xs :: [[String]] <- query conn [sql|SELECT name FROM schema_migrations WHERE name = ? |] [name]
      return (null xs)

downMigration :: Connection -> [Migration] -> IO ()
downMigration conn migrations = do
  migName <- fmap (maybe (error "There was no last run migration to roll down.")
                          (id))
                  (lastRunMigration conn)
  let mig = findMig migName migrations
  case mig of
    Nothing -> putStrLn $ "Last run migration not found. Aborting."
    Just (name, (_, down)) -> do
      putStrLn ("Running Migration down " ++ name)
      result <- try $ execute_ conn down :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 void $ execute conn [r|DELETE FROM schema_migrations WHERE name=? |] [name]
                 join $ fmap (maybe (removeFile "schema.sql") pgDump) (lastRunMigration conn)
                 putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
             result
  where
    findMig :: String -> [Migration] -> Maybe Migration
    findMig _ [] = Nothing
    findMig migname (m@(name, _) : migs) | migname == name = Just m
                                         | otherwise = findMig migname migs

redoMigration :: Connection -> [Migration] -> IO ()
redoMigration conn migrations = do
  downMigration conn migrations
  upMigration conn migrations

fillerText :: String
fillerText = [r|import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import System.Environment

migrate :: (Query, Query)
migrate = (up, down)

up :: Query
up = undefined

down :: Query
down = undefined
|]

lastRunMigration :: Connection -> IO (Maybe String)
lastRunMigration conn = do
  xs ::[(String,UTCTime)] <- query_ conn ("SELECT name, updated_at FROM schema_migrations ORDER BY updated_at DESC LIMIT 1")
  if null xs
     then return Nothing
     else (\[(name, _)] -> return $ Just name) xs

timeFormat :: String
timeFormat = "%Y%m%d%H%M%S"

pascal :: String -> String
pascal [] = []
pascal (x:xs) = toUpper x : pascal' xs
  where
    pascal' [] = []
    pascal' [y] = [y]
    pascal' (y:ys) | y == '_' = y : toUpper (head ys) : pascal' (tail ys)
               | otherwise = y : pascal' ys

pgDump :: String -> IO ()
pgDump name = callCommand $ "pg_dump -d 'vl' --file schema.sql && sed -i '1i" ++ name ++ "' schema.sql"
