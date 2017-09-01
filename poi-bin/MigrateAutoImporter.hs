module Main where

import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Monoid ((<>))
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension, (</>))

main :: IO ()
main =
  do (src:input:output:[]) <- getArgs
     ls <- lines <$> readFile input
     let (imports, rest) = splitImports ls
     cd <- getCurrentDirectory
     ms' <- getDirectoryContents (cd </> "migrations")
     let ms = map dropExtension $
              filter (\m -> "M" `isPrefixOf` m && ".hs" `isSuffixOf` m) ms'
     let poiImports = map (\m -> "import qualified " <> m) ms
         poiMigrations = ["migrations = ["] <>
          ["  " <> intercalate "\n  ," (map (\m -> "(\"" <> m <> "\", " <> m <> ".migrate)") ms)] <> ["\n  ]"]
         poiAdded = ["\n\n"] <>
                         poiImports <> ["\n\n"] <>
                         poiMigrations  <> ["\n\n"]
     writeFile output $ unlines $ ["{-# LINE 1 \"" <> src <> "\" #-}"] <>
                                  imports <>
                                  poiAdded <>
                                  rest
  where splitImports ls = splitImports' [] (reverse ls)
        splitImports' imp [] = ([], imp)
        splitImports' imp (l:ls) = if "import" `isPrefixOf` l
                                      then (reverse (l:ls), imp)
                                      else splitImports' (l:imp) ls
