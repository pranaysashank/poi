module Main where

import System.Environment
import System.Process

import Migrate
import Utils (Options(..), poiArgs)

main :: IO ()
main = do
  poiArgs (migs opts)
  where
    opts = MigrateOpts Prepare connectionInfo

migs :: MigrateOpts -> Options -> IO ()
migs opts (Options mode) = do
  case mode of
    Prepare -> migrate opts []
    Up -> callCommand "stack Migrations.hs up"
    Down -> callCommand "stack Migrations.hs down"
    New xs -> callCommand ("stack Migrations.hs new " ++ xs)
    Redo -> callCommand ("stack Migrations.hs redo")
