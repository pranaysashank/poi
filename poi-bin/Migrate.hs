{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Migrate where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, void)
import Data.Char
import Data.Time
import Data.ByteString.Char8 (unpack)
import Database.PostgreSQL.Simple
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import Text.RawString.QQ (r)
import GHC.Int

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
    _ -> undefined
  close conn

prepareMigration :: Connection -> IO ()
prepareMigration conn = do
  execute_ conn [r|
CREATE TABLE IF NOT EXISTS schema_migrations (name text
NOT NULL PRIMARY KEY, updated_at timestamp with time zone
NOT NULL DEFAULT now())
|]
  cd <- getCurrentDirectory
  let migrationsD = cd </> "migrations"
  withCurrentDirectory cd (createDirectoryIfMissing False migrationsD)
  writeFile (cd </> "Migrations.hs") [r|{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF migrate-autoimporter #-}

module Migrations where

import Migrate (Mode, MigrateOpts, migrate, connectionInfo)

runMigrations :: Mode -> IO ()
runMigrations mode = migrate opts migrations
  where
    opts = MigrateOpts mode connectionInfo
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
  maybeMigName <- lastRunMigration conn
  let migs = case maybeMigName of
               Nothing -> migrations
               Just migName -> takeWhile (\(name, _) -> name /= migName) migrations
  forM_ migs $ \(name, (up, _)) -> do
    putStrLn ("Running Migration up " ++ name)
    result <- try $ execute_ conn up :: IO (Either SqlError Int64)
    either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
           (\b -> do
               execute conn [r|INSERT INTO schema_migrations VALUES (?) |] [name]
               putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
           result

downMigration :: Connection -> [Migration] -> IO ()
downMigration conn migrations = do
  maybeMigName <- lastRunMigration conn
  let migName = case maybeMigName of
                  Nothing -> error "There was no last run migration to roll down."
                  Just gb -> gb
  let mig = findMig migName migrations
  case mig of
    Nothing -> putStrLn $ "Last run migration not found. Aborting."
    Just (name, (_, down)) -> do
      putStrLn ("Running Migration down " ++ name)
      result <- try $ execute_ conn down :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 execute conn [r|DELETE FROM schema_migrations WHERE name=? |] [name]
                 putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
             result
  where
    findMig :: String -> [Migration] -> Maybe Migration
    findMig _ [] = Nothing
    findMig migname (m@(name, _) : migs) | migname == name = Just m
                                         | otherwise = findMig migname migs

fillerText :: String
fillerText = [r|
import Database.PostgreSQL.Simple
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
