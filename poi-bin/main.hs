module Main where

import System.Process (callCommand)

import Poi.Migrations.Migrate (prepareMigrationWithConfig, migrationStatusWithConfig, createNewMigration)
import Poi.Migrations.Types (Options(..), MigrateArgs(..), Mode(..))
import Poi.Migrations.Utils (poiArgs, readConfigForEnv, dbConfig)

main :: IO ()
main = do
  poiArgs (migs)

migs :: Options -> IO ()
migs (Options (MigrateArgs mode env ver)) = do
  config <- readConfigForEnv env
  case mode of
    Prepare -> prepareMigrationWithConfig (dbConfig config)
    Up -> callCommand $ makeCommand "up"
    Down -> callCommand $ makeCommand "down"
    New xs ft -> createNewMigration xs ft
    Redo -> callCommand $ makeCommand "redo"
    Status -> migrationStatusWithConfig (dbConfig config)
  where
    makeCommand :: String -> String
    makeCommand s  = maybe ("stack Migrations.hs " ++ s ++ " --env " ++ env)
                           (\v -> "stack Migrations.hs " ++ s ++ " --env " ++ env ++ " --version " ++ v)
                           ver
