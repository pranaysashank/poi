{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, try)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Monoid ((<>))
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (dropExtension, (</>))
import Data.Yaml
import Text.InterpolatedString.Perl6 (qc)

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
                let fs = sort n
                let hs = map dropExtension $
                           filter (\m -> "M" `isPrefixOf` m && ".hs" `isSuffixOf` m) fs
                sqls <- mapM makeMigrateFromSql $
                          filter (\m -> "M" `isPrefixOf` m && ".sql" `isSuffixOf` m && m /= "schema.sql") fs
                yamls <- mapM makeMigrateFromYaml $
                           filter (\m -> "M" `isPrefixOf` m && (".yaml" `isSuffixOf` m || ".yml" `isSuffixOf` m)) fs
                let poiImports = map (\m -> "import qualified Migrations." <> m) hs
                    poiMigrations = ["migrations = ["] <>
                                    ["  " <> intercalate "\n  ," ((map (\m -> "(\"" <> m <> "\", " <> "Migrations." <> m <> ".migrate)") hs) ++ sqls ++ yamls)] <> ["\n  ]"]
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


makeMigrateFromSql :: String -> IO String
makeMigrateFromSql x = do
  query <- readFile ("Migrations" </> x)
  return $ [qc|("{(dropExtension x)}", ([qc|{query}|] ++ "|]" ++ [qc|, [qc|{query}|] ++ "|]" ++ [qc|))|]

makeMigrateFromYaml :: String -> IO String
makeMigrateFromYaml f = decodeFile ("Migrations" </> f) >>= directionToString
  where
    directionToString direction = case direction of
      Just d -> return $ [qc|("{(dropExtension f)}",([qc|{(up d)}|] ++ "|]" ++ [qc|, [qc|{(down d)}|] ++ "|]" ++ [qc|))|]
      Nothing -> error $ "Parsing " ++ f ++ " failed."

data Direction = Direction { up :: String
                           , down :: String
                           } deriving (Show, Eq)

instance FromJSON Direction where
  parseJSON (Object v) = Direction <$> v .: "up"
                                   <*> v .: "down"
  parseJSON invalid = error ( "Direction" ++ show invalid)
