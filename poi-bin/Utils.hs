{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Char (toLower)
import           Data.List (foldl', sortOn)
import qualified Data.Ord
import qualified Data.Map.Strict as DM
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import           Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import           Data.Yaml.Config (loadYamlSettings, ignoreEnv)
import           Database.PostgreSQL.Simple (ConnectInfo(..))
import           Options.Applicative
import           System.Environment (getEnv)
import           Types

up :: Parser Mode
up = pure Up

down :: Parser Mode
down = pure Down

redo :: Parser Mode
redo = pure Redo

prepare :: Parser Mode
prepare = pure Prepare

new :: Parser Mode
new = New <$> argument str (metavar "name")

modeOpts :: Parser Mode
modeOpts = subparser
                ( command "up" (info up (progDesc "Runs migration up"))
                <> command "down"(info down (progDesc "Runs migration down"))
                <> command "redo" (info redo (progDesc "Redoes the last migration run"))
                <> command "new" (info new (progDesc "Create a new migration in migrations directory"))
                <> command "prepare" (info prepare (progDesc "Creates schema_migrations table, migrations directory and Migrations.hs file"))
                )

migrateOpts :: Parser MigrateArgs
migrateOpts = MigrateArgs <$> modeOpts
                          <*> strOption
                          ( long "env"
                         <> short 'e'
                         <> help "production or development environment"
                         <> metavar "ENVIRONMENT"
                         <> showDefault
                         <> value "development")
                          <*> (gg <$> strOption
                          ( long "version"
                         <> short 'v'
                         <> help "Fuzzy matches the specific migration to run."
                         <> metavar "VERSION"
                         <> value ""))
  where
    gg :: String -> Maybe String
    gg "" = Nothing
    gg a = Just a

options :: Parser Options
options = Options <$> migrateOpts

poiOpts :: Parser Options
poiOpts = subparser
        ( command "migrate" (info (options <**> helper) (fullDesc <> progDesc "Runs migrations")))

poiArgs :: (Options -> IO ()) -> IO ()
poiArgs f = f =<< execParser opts
  where
    opts = info (poiOpts <**> helper)
      ( fullDesc
     <> progDesc "Has helper functions"
     <> header "Poi - a tool to organize web apps in haskell" )


migArgs :: (MigrateArgs -> IO ()) -> IO ()
migArgs f = f =<< execParser opts
  where
    opts = info (migrateOpts <**> helper)
         ( fullDesc
         <> progDesc "Runs migrations")

main :: IO ()
main = poiArgs greet

greet :: Options -> IO ()
greet (Options (MigrateArgs mode env ver)) = do case mode of
                                                        Up -> putStrLn "Up"
                                                        Down -> putStrLn "Down"
                                                        Redo -> putStrLn "Redo"
                                                        New x -> putStrLn $ "New " ++ x
                                                        Prepare -> putStrLn "Prepare"
                                                putStrLn env
                                                putStrLn $ show ver

--
-- DB Config
--

data DbConfig = DbConfig { getConnectInfo :: ConnectInfo } deriving (Eq, Show)

--
-- Config
--

type EnvName = String

data Config = Config
  { getDbConfig          :: DbConfig
  , getEnvName           :: EnvName
  } deriving (Eq, Show)

instance FromJSON DbConfig where
  parseJSON (Y.Object v) = DbConfig
                           <$> (ConnectInfo
                           <$> v .: "host"
                           <*> (fromInteger <$> v .: "port")
                           <*> v .: "username"
                           <*> v .: "password"
                           <*> v .: "name")
  parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON Config where
  parseJSON (Y.Object v) = Config
     <$> v .: "database"
     <*> (pure undefined) -- will be filled from the read config function
  parseJSON _ = error $ "unable to parse config.yml"

readConfigForEnv :: EnvName -> IO Config
readConfigForEnv ename = do
  settingsMap <- loadYamlSettings ["config.yml"] [] ignoreEnv :: IO (DM.Map String Config)
  case DM.lookup ename settingsMap of
    Just c -> return (c { getEnvName = ename})
    Nothing -> error $ "Config section for APP_ENV='" ++ ename ++ "' not found. Have you mis-spelt it? Does the section exist in config.yml?"

readConfig :: IO Config
readConfig = (getEnv "APP_ENV") >>= readConfigForEnv

dbConfig :: Config -> ConnectInfo
dbConfig c = getConnectInfo (getDbConfig c)

timeStampFromFileName :: String -> String
timeStampFromFileName name@(x:xs) | x == 'M' = takeWhile (\c -> c /= '_') xs
                                  | otherwise = error ("File not properly named " ++ name ++ ".")
timeStampFromFileName _ = error ("Invalid file name")

match :: String       -- ^ The pattern to match against
      -> String       -- ^ The value containing the text to search in
      -> Maybe (String, Int)
match p t =
    if null p then Nothing else Just (t, totalScore)
  where
    (s', pattern') = let f = map toLower
                     in (f t, f p)
    (totalScore, _, _, _) =
      foldl'
       (\(tot, cur, res, pat) c ->
          case splitAtPrefix pat of
            Nothing -> (tot, 0, res ++ [c], pat)
            Just (x, xs) ->
              if x == c
                 then let cur' = cur * 2 + 1
                      in (tot + cur', cur', res ++ [c], xs)
                 else (tot, 0, res ++ [c], pat)
       ) (0, 0, "", pattern') s'

fuzzyFilter :: String -> [Migration] -> [Migration]
fuzzyFilter pat xs = map fst $ bestMatch $ filter ((> 0) . snd) $ sortOn (Data.Ord.Down . snd) $
  mapMaybe (\x@(t, _) -> maybe (Nothing) (\(_, score) -> Just (x, score)) (match pat t)) xs

bestMatch :: [(Migration, Int)] -> [(Migration, Int)]
bestMatch [] = []
bestMatch (x : []) = [x]
bestMatch (x : y : z) = if (snd x) > (snd y)
                           then [x]
                           else x : y : bestMatch z

splitAtPrefix :: [t] -> Maybe (t, [t])
splitAtPrefix [] = Nothing
splitAtPrefix (x:xs) = Just (x, xs)
