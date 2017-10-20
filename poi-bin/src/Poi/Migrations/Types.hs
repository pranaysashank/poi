module Poi.Migrations.Types where

import Database.PostgreSQL.Simple

data Mode = Up | Down | Redo | Prepare | New FileName FileType | Status

data MigrateOpts = MigrateOpts { migration :: Mode
                               , connectInfo :: ConnectInfo
                               , environment :: String
                               }

type Migration = (String, (Query, Query))

data Options = Options { poiMigrate :: MigrateArgs
                       }

data MigrateArgs = MigrateArgs { migMode :: Mode
                               , migEnvironment :: String
                               , migVersion :: Maybe String
                               }

type FileName = String

data FileType = Sql | Hs | Yaml deriving (Show, Eq)
