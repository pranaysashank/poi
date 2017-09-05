module Main where

import Control.Exception (IOException, try)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
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
     result <- try $ getDirectoryContents (cd </> "Migrations") :: IO (Either IOException [FilePath])
     either (\m -> do
                putStrLn $ show m
                putStrLn "Did you first run \"poi migrate prepare\" from project root?")
            (\n -> do
                let ms' = sort n
                let ms = map dropExtension $
                         filter (\m -> "M" `isPrefixOf` m && ".hs" `isSuffixOf` m) ms'
                let poiImports = map (\m -> "import qualified Migrations." <> m) ms
                    poiMigrations = ["migrations = ["] <>
                                    ["  " <> intercalate "\n  ," (map (\m -> "(\"" <> m <> "\", " <> "Migrations." <> m <> ".migrate)") ms)] <> ["\n  ]"]
                    poiAdded = ["\n\n"] <>
                               poiImports <> ["\n\n"] <>
                               poiMigrations  <> ["\n\n"]
                writeFile output $ unlines $ ["{-# LINE 1 \"" <> src <> "\" #-}"] <>
                                             imports <> poiAdded <> rest)
            result
  where splitImports ls = splitImports' [] (reverse ls)
        splitImports' imp [] = ([], imp)
        splitImports' imp (l:ls) =
          if "import" `isPrefixOf` l
          then (reverse (l:ls), imp)
          else splitImports' (l:imp) ls
