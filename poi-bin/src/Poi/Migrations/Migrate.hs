{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Poi.Migrations.Migrate where

import Control.Exception (finally, try, bracket)
import Control.Monad (forM_, join, filterM, void)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.List (intercalate, isPrefixOf)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Int (Int64)
import System.Console.ANSI
import System.Directory (getDirectoryContents, getCurrentDirectory, createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)
import Text.InterpolatedString.Perl6 (qc)
import Text.RawString.QQ (r)

import Poi.Migrations.Utils (fileNameFromTimeStamp, timeStampFromFileName, fuzzyFilter, colorPutStrLn)
import Poi.Migrations.Types

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectDatabase = "postgres"
                                    , connectPassword = "password"
                                    , connectUser = "postgres"
                                    }


migrate :: MigrateOpts -> [Migration] -> Maybe String -> IO ()
migrate opts migrations ver = do
  conn <- connect $ connectInfo opts
  finally (maybe (case migration opts of
                     Prepare -> prepareMigration conn
                     New migName fileType -> createNewMigration migName fileType
                     Up -> upMigration conn migrations
                     Down -> downMigration conn migrations
                     Redo -> redoMigration conn migrations)
                 (\v -> runSpecificMigration conn (migration opts) migrations v) ver)
          (do --fileExists <- doesFileExist "Migrations/schema.sql"
              case migration opts of
                Up -> generatePGDump conn
                Down -> generatePGDump conn
                Redo -> generatePGDump conn
                _ -> return ()
              close conn)
  where
    generatePGDump conn = join $ fmap (maybe (pgDump (opts) "No migrations applied") (pgDump (opts))) (lastRunMigration conn)

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

import Poi.Migrations.Types
import Poi.Migrations.Migrate (migrate, connectionInfo)
import Poi.Migrations.Utils (migArgs, readConfigWithDefault, dbConfig)

runMigrations :: MigrateArgs -> IO ()
runMigrations (MigrateArgs mode env ver) = do
   config <- readConfigWithDefault env
   let opts = MigrateOpts mode (dbConfig config) env
   migrate opts migrations ver

defaultMigrationArgs = MigrateArgs Up "lambda" Nothing

main :: IO ()
main = migArgs runMigrations
|]

createNewMigration :: String -> FileType -> IO ()
createNewMigration migName fileType = do
  cd <- getCurrentDirectory
  let migrationsD = cd </> "Migrations"
  time <- getCurrentTime
  let moduleName = "M" ++ formatTime defaultTimeLocale timeFormat time ++ "_" ++ pascal migName
  case fileType of
    Hs -> do
      let migFile = migrationsD </> (moduleName ++ ".hs")
      putStrLn migFile
      writeFile migFile (([r|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
|] ++ "module Migrations." ++ moduleName ++ " where \n") ++ fillerText)
    Sql -> do
      let migFile = migrationsD </> (moduleName ++ ".sql")
      putStrLn migFile
      writeFile migFile [r|-- Your migration statements go here |]
    Yaml -> do
      let migFile = migrationsD </> (moduleName ++ ".yml")
      putStrLn migFile
      writeFile migFile [r|# TODO: Insert your raw SQL in the corresponding blocks
up:

down:
  |]

upMigration :: Connection -> [Migration] -> IO ()
upMigration conn migrations = do
  migs <- filterM (\(name, _) -> didMigrationRun name conn) migrations
  myForM_ migs (\mig -> withTransaction conn (runMigration conn Up mig))
  where
    myForM_ :: [Migration] -> (Migration -> IO ()) -> IO ()
    myForM_ [] _ = return ()
    myForM_ [x@ (_, _)] f = do
      f x
    myForM_ (x:xs) f = do
      f x
      myForM_ xs f

didMigrationRun :: String -> Connection -> IO Bool
didMigrationRun name conn= do
    xs :: [Only String] <- query conn [sql|SELECT version FROM schema_migrations WHERE version = ? |] [timeStampFromFileName name]
    return (null xs)

downMigration :: Connection -> [Migration] -> IO ()
downMigration conn migrations = do
  migName <- fmap (maybe (error "There was no last run migration to roll down.")
                          (id))
                  (lastRunMigration conn)
  let mig = findMig migName migrations
  case mig of
    Nothing -> colorPutStrLn Red $ "Last run migration not found. Aborting."
    Just migRun -> withTransaction conn (runMigration conn Down migRun)

runMigration :: Connection -> Mode -> Migration -> IO ()
runMigration conn mode m@(name,(up, down)) =
  case mode of
    Up -> do
      colorPutStrLn Yellow ("Running Migration up " ++ name)
      result <- try $ execute_ conn up :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 void $ execute conn [r|INSERT INTO schema_migrations SELECT ? WHERE NOT EXISTS (SELECT version FROM schema_migrations WHERE version=?)|] (timeStampFromFileName name, timeStampFromFileName name)
                 colorPutStrLn Green $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
             result
    Down -> do
      colorPutStrLn Yellow ("Running Migration down " ++ name)
      result <- try $ execute_ conn down :: IO (Either SqlError Int64)
      either (\a -> error $ "The Migration " ++ name ++ " could not be run. Aborting.\n" ++ (unpack $ sqlErrorMsg a))
             (\b -> do
                 void $ execute conn [r|DELETE FROM schema_migrations WHERE version=? |] [timeStampFromFileName name]
                 colorPutStrLn Green $ "The Migration " ++ name ++ " successfully ran. " ++ show b ++ " rows affected.")
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
    [x] -> withTransaction conn (runMigration conn mode x)
    xs  -> putStrLn $ "Found these migrations, be more specific. \n" ++ intercalate "\n" (map (show . fst) xs)


redoMigration :: Connection -> [Migration] -> IO ()
redoMigration conn migrations = do
  migName <- fmap (maybe (error "There was no last run migration to redo.")
                          (id))
                  (lastRunMigration conn)
  let mig = findMig migName migrations
  case mig of
    Nothing -> colorPutStrLn Red $ "Last run migration not found. Aborting."
    Just migRun -> withTransaction conn $ do
      runMigration conn Down migRun
      runMigration conn Up migRun

migrationStatus :: Connection -> IO ()
migrationStatus conn = do
  migName <- fmap (maybe (error "There was no last run migration to redo.")
                         (id))
                  (lastRunMigration conn)
  mig <- fileNameFromTimeStamp migName
  migs <- migrationsToRun conn
  case mig of
    Nothing -> colorPutStrLn Yellow $ "No migrations run yet :|"
    Just name -> do
      colorPutStrLn Green $ [qc|Last applied migration is {name} :) |]
      if (null migs)
      then colorPutStrLn Green $ "All migrations are up-to-date"
      else do
          putStrLn $ "These migrations are yet to be run. \n"
          forM_ migs (colorPutStrLn Red)

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

migrationsToRun :: Connection -> IO ([String])
migrationsToRun conn = do
  migrationFiles <- (filter (isPrefixOf "M")) <$> (getDirectoryContents "./Migrations")
  migs <- filterM (flip didMigrationRun conn) migrationFiles
  return migs

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

pgDump :: MigrateOpts -> String -> IO ()
pgDump opts name = if env == "development"
                   then callCommand ([qc|
\{ echo "-- SCHEMA VERSION {name}" ; pg_dump -d '{db}' -s; pg_dump --data-only --table schema_migrations -d '{db}'; } > Migrations/schema.sql
|])
                  else return ()
  where
    env = environment opts
    db = (connectDatabase . connectInfo) opts


findMig :: String -> [Migration] -> Maybe Migration
findMig _ [] = Nothing
findMig migname (m@(name, _) : migs) | migname == (timeStampFromFileName name) = Just m
                                     | otherwise = findMig migname migs

prepareMigrationWithConfig :: ConnectInfo -> IO ()
prepareMigrationWithConfig connection = do
  bracket (connect connection)
          (close)
          (prepareMigration)

migrationStatusWithConfig :: ConnectInfo -> IO ()
migrationStatusWithConfig connection = do
  bracket (connect connection)
          (close)
          (migrationStatus)
