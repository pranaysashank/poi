module Main where

import System.Process (callCommand)

import Migrate (prepareMigrationWithConfig)
import Types (Options(..), MigrateArgs(..), Mode(..))
import Utils (poiArgs, readConfigForEnv, dbConfig)

main :: IO ()
main = do
  poiArgs (migs)

migs :: Options -> IO ()
migs (Options (MigrateArgs mode env ver)) = do
  config <- readConfigForEnv env
  case mode of
    Prepare -> prepareMigrationWithConfig (dbConfig config)
    Up -> callCommand $ makeCommand "up" ver
    Down -> callCommand $ makeCommand "down" ver
    New xs -> callCommand $ makeCommand ("new " ++ xs) ver
    Redo -> callCommand $ makeCommand "redo" ver
  where
    makeCommand :: String -> Maybe String -> String
    makeCommand s ver = maybe ("stack Migrations.hs " ++ s ++ " --env " ++ env)
                              (\v -> "stack Migrations.hs " ++ s ++ " --env " ++ env ++ " --version " ++ v)
                              ver
