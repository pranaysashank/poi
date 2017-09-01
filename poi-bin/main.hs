module Main where

import System.Directory
import System.Environment
import System.FilePath
import System.Process

import Migrate

main :: IO ()
main = do
  (script:options) <- getArgs
  cd <- getCurrentDirectory
  let scriptsD = cd ++ "/scripts"
      opts = MigrateOpts Prepare connectionInfo
  scriptsDExists <- doesDirectoryExist scriptsD
  scripts <- if scriptsDExists then listDirectory scriptsD else return []
  putStrLn $ show $ script `elem` (map dropExtension scripts)
  putStrLn $ show scripts
  migrate opts []
  -- callCommand ("stack " ++ scriptsD ++ "/" ++ script ++ ".hs " ++ mconcat options)
