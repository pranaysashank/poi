module Main where

import System.Process

import Migrate
import Utils (Options(..), poiArgs, readConfigForEnv, dbConfig, MigrateArgs(..))

main :: IO ()
main = do
  poiArgs (migs)

migs :: Options -> IO ()
migs (Options (MigrateArgs mode env)) = do
  config <- readConfigForEnv env
  let opts = MigrateOpts Prepare (dbConfig config)
  case mode of
    Prepare -> migrate opts []
    Up -> callCommand "stack Migrations.hs up"
    Down -> callCommand "stack Migrations.hs down"
    New xs -> callCommand ("stack Migrations.hs new " ++ xs)
    Redo -> callCommand ("stack Migrations.hs redo")
