module Main where

import System.Environment
import System.Process

import Migrate

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usageError
    (e:es) -> do
      case e of
        "migrate" -> case (head es) of
          "prepare" -> migrate opts []
          "up" -> callCommand "stack Migrations.hs up"
          "down" -> callCommand "stack Migrations.hs down"
          "new" -> callCommand ("stack Migrations.hs new " ++ head (tail es))
          _ -> usageError
        _ -> usageError
  where
    opts = MigrateOpts Prepare connectionInfo
  -- callCommand ("stack " ++ scriptsD ++ "/" ++ script ++ ".hs " ++ mconcat options)

usageError :: IO ()
usageError = putStrLn "Usage: poi migrate [ prepare | up | down | new <name> ]"
