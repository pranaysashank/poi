{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Migrate where

import Control.Exception (finally, try, bracket)
import Control.Monad (when, join, filterM, void)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.List (intercalate)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Int (Int64)
import System.Directory (doesFileExist, removeFile, getCurrentDirectory, createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import Text.RawString.QQ (r)

import Utils (timeStampFromFileName, fuzzyFilter)
import Types

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "postgres"
                                    , connectPassword = "password"
                                    , connectUser = "postgres"
                                    }


migrate :: MigrateOpts -> [Migration] -> Maybe String -> IO ()
migrate opts migrations ver = do
  conn <- connect $ environment opts
  finally (maybe (case migration opts of
                     Prepare -> prepareMigration conn
                     New migName -> createNewMigration migName
                     Up -> upMigration conn migrations
                     Down -> downMigration conn migrations
                     Redo -> redoMigration conn migrations)
                 (\v -> runSpecificMigration conn (migration opts) migrations v) ver)
          (do fileExists <- doesFileExist "schema.sql"
              case migration opts of
                Up -> join $ fmap (maybe (when fileExists (removeFile "schema.sql")) (pgDump ((connectDatabase . environment) opts))) (lastRunMigration conn)
                Down -> join $ fmap (maybe (when fileExists (removeFile "schema.sql")) (pgDump ((connectDatabase . environment) opts))) (lastRunMigration conn)
                _ -> return ()
              close conn)

prepareMigration :: Connection -> IO ()
prepareMigration conn = do
  void $ execute_ conn [r|
CREATE OR REPLACE FUNCTION trg_update_modified_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language 'plpgsql';
CREATE TABLE IF NOT EXISTS schema_migrations (version character varying(255)
NOT NULL PRIMARY KEY);
|]
  cd <- getCurrentDirectory
  let migrationsD = cd </> "Migrations"
  withCurrentDirectory cd (createDirectoryIfMissing False migrationsD)
  writeFile (cd </> "Migrations.hs") [r|#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF migrate-autoimporter #-}

module Migrations where

import Types
import Migrate (migrate, connectionInfo)
import System.Directory
import System.Environment
import System.FilePath
import Utils (migArgs, readConfigForEnv, dbConfig)

runMigrations :: MigrateArgs -> IO ()
runMigrations (MigrateArgs mode env ver) = do
   config <- readConfigForEnv env
   let opts = MigrateOpts mode (dbConfig config)
   migrate opts migrations ver

main :: IO ()
main = migArgs runMigrations
|]

createNewMigration :: String -> IO ()
createNewMigration migName = do
  cd <- getCurrentDirectory
  let migrationsD = cd </> "Migrations"
  time <- getCurrentTime
  let moduleName = "M" ++ formatTime defaultTimeLocale timeFormat time ++ "_" ++ pascal migName
      migFile = migrationsD </> (moduleName ++ ".hs")
  putStrLn migFile
  writeFile migFile (([r|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
|] ++ "module Migrations." ++ moduleName ++ " where \n") ++ fillerText)

upMigration :: Connection -> [Migration] -> IO ()
upMigration conn migrations = do
  migs <- filterM didMigrationRun migrations
  myForM_ migs (\mig -> withTransaction conn (runMigration conn Up mig))
  where
    myForM_ :: [Migration] -> (Migration -> IO ()) -> IO ()
    myForM_ [] _ = return ()
    myForM_ [x@ (_, _)] f = do
      f x
    myForM_ (x:xs) f = do
      f x
      myForM_ xs f

    didMigrationRun :: Migration -> IO Bool
    didMigrationRun (name, _) = do
      xs :: [Only String] <- query conn [sql|SELECT version FROM schema_migrations WHERE version = ? |] [timeStampFromFileName name]
      return (null xs)

downMigration :: Connection -> [Migration] -> IO ()
downMigration conn migrations = do
  migName <- fmap (maybe (error "There was no last run migration to roll down.")
                          (id))
                  (lastRunMigration conn)
  let mig = findMig migName migrations
  case mig of
    Nothing -> putStrLn $ "Last run migration not found. Aborting."
    Just migRun -> withTransaction conn (runMigration conn Down migRun)

runMigration :: Connection -> Mode -> Migration -> IO ()
runMigration conn mode m@(name,(up, down)) =
  case mode of
    Up -> do
      putStrLn ("Running Migration up " ++ name)
      result <- try $ execute_ conn up :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 void $ execute conn [r|INSERT INTO schema_migrations SELECT ? WHERE NOT EXISTS (SELECT version FROM schema_migrations WHERE version=?)|] (timeStampFromFileName name, timeStampFromFileName name)
                 putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
             result
    Down -> do
      putStrLn ("Running Migration down " ++ name)
      result <- try $ execute_ conn down :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 void $ execute conn [r|DELETE FROM schema_migrations WHERE version=? |] [timeStampFromFileName name]
                 putStrLn $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
             result
    Redo -> do
      runMigration conn Down m
      runMigration conn Up m
    _ -> do putStrLn "You should have never reached this far"

runSpecificMigration :: Connection -> Mode -> [Migration] -> String -> IO ()
runSpecificMigration conn mode migrations mig = do
  let matches = fuzzyFilter mig migrations
  case matches of
    []  -> putStrLn $ "Given " ++ mig ++ " didn't match any existing migrations."
    [x] -> do
     putStr $ "Run migration " ++ (fst x) ++ " (y or n)? "
     reply <- getLine
     if reply == "y"
        then withTransaction conn (runMigration conn mode x)
        else putStrLn "Migration aborted."
    xs  -> putStrLn $ "Found these migrations, be more specific. \n" ++ intercalate "\n" (map (show . fst) xs)


redoMigration :: Connection -> [Migration] -> IO ()
redoMigration conn migrations = do
  migName <- fmap (maybe (error "There was no last run migration to redo.")
                          (id))
                  (lastRunMigration conn)
  let mig = findMig migName migrations
  case mig of
    Nothing -> putStrLn $ "Last run migration not found. Aborting."
    Just migRun -> withTransaction conn $ do
      runMigration conn Down migRun
      runMigration conn Up migRun

fillerText :: String
fillerText = [r|import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Text.InterpolatedString.Perl6 (qc)
import System.Environment

migrate :: (Query, Query)
migrate = (up, down)

-- TODO: Insert your raw SQL just before the closing `| ])` tag
up :: Query
up = [qc| |] ++ "|]" ++ [r|

-- TODO: Insert your raw SQL just before the closing `| ])` tag
down :: Query
down = [qc| |] ++ "|]\n"

lastRunMigration :: Connection -> IO (Maybe String)
lastRunMigration conn = do
  xs ::[Only String] <- query_ conn ("SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1")
  if null xs
     then return Nothing
     else return $ Just ((\(Only n) -> n) $ head xs)

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

pgDump :: String -> String -> IO ()
pgDump db name = callCommand $ "pg_dump -d '" ++ db ++ "' --file schema.sql && sed -i '1i" ++ name ++ "' schema.sql"

findMig :: String -> [Migration] -> Maybe Migration
findMig _ [] = Nothing
findMig migname (m@(name, _) : migs) | migname == (timeStampFromFileName name) = Just m
                                     | otherwise = findMig migname migs

prepareMigrationWithConfig :: ConnectInfo -> IO ()
prepareMigrationWithConfig connection = do
  bracket (connect connection)
          (close)
          (prepareMigration)
