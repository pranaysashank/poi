module Main where

import System.Environment
import System.Process

import Migrate

main :: IO ()
main = do
  (e:es) <- getArgs
  case e of
    "prepare" -> migrate opts []
    "up" -> callCommand "stack Migrations.hs up"
    "down" -> callCommand "stack Migrations.hs down"
    "new" -> callCommand ("stack Migrations.hs new " ++ head es)
    _ -> putStrLn "Usage: poi migrate [prepare | up | down | new <name> ]"
  where
    opts = MigrateOpts Prepare connectionInfo
  -- callCommand ("stack " ++ scriptsD ++ "/" ++ script ++ ".hs " ++ mconcat options)
