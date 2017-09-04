{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Map.Strict as DM
import           Data.Monoid
import           Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import           Data.Yaml.Config (loadYamlSettings, ignoreEnv)
import           Database.PostgreSQL.Simple (ConnectInfo(..))
import           Migrate (Mode(..), MigrateOpts (..))
import           Options.Applicative
import           System.Environment (getEnv)

data Migrate

data Options = Options { poiMigrate :: MigrateArgs
                       }

data MigrateArgs = MigrateArgs { migMode :: Mode
                               , migEnvironment :: String
                               }

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
                         <> help "prod or dev environment"
                         <> metavar "ENVIRONMENT"
                         <> showDefault
                         <> value "development")

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

-- main :: IO ()
-- main = poiArgs greet

-- greet :: Options -> IO ()
-- greet (Options (MigrateArgs mode env)) = do case mode of
--                                                     Up -> putStrLn "Up"
--                                                     Down -> putStrLn "Down"
--                                                     Redo -> putStrLn "Redo"
--                                                     New x -> putStrLn $ "New " ++ x
--                                                     Prepare -> putStrLn "Prepare"
--                                             putStrLn env

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
