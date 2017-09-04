module Utils where

import Options.Applicative
import Data.Monoid
import Migrate (Mode(..), MigrateOpts (..))

data Migrate

data Options = Options { poiMigrate :: Mode
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

migrateOpts :: Parser Mode
migrateOpts = subparser
            ( command "up" (info up (progDesc "Runs migration up"))
            <> command "down"(info down (progDesc "Runs migration down"))
            <> command "redo" (info redo (progDesc "Redoes the last migration run"))
            <> command "new" (info new (progDesc "Create a new migration in migrations directory"))
            <> command "prepare" (info prepare (progDesc "Creates schema_migrations table, migrations directory and Migrations.hs file"))
            )

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


migArgs :: (Mode -> IO ()) -> IO ()
migArgs f = f =<< execParser opts
  where
    opts = info (migrateOpts <**> helper)
         ( fullDesc
         <> progDesc "Runs migrations")

-- main :: IO ()
-- main = poiArgs greet

-- greet :: Options -> IO ()
-- greet (Options mode) = case mode of
--                               Up -> putStrLn "Up"
--                               Down -> putStrLn "Down"
--                               Redo -> putStrLn "Redo"
--                               New x -> putStrLn $ "New " ++ x
--                               Prepare -> putStrLn "Prepare"
