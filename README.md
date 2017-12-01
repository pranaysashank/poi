# poi

Syntax : `poi migrate [prepare | up | down | redo | status | new <newMigration>] [--version <fileName> | --env ]`

## Commands

 - `prepare` : Creates a `schema_migrations` table, a `Migrations.hs` file, and a `Migrations` directory in the current directory.
 - `new <newMigration>` : Creates a new Migration file in the Migrations directory. By default it's a haskell file. Passing a --sql or an --yaml flag creates the respective migration files. The created haskell file is prefixed with the current timestamp.
 - `up` : Runs the up code block present in the migration files that aren't run yet.
 - `down` : Runs the down block present in the last run migration file.
 - `redo` : Redoes the the last run migration.
 - `status` : Lists all the migrations that aren't run yet and the last run migration.

## Flags
 - `--hs` : The default. Creates the following haskell migration file.

  ``` haskell
  {-# LANGUAGE OverloadedString #-}
  
  import Database.PostgreSQL.Simple

  migrate :: (Query, Query)
  migrate = (up, down)
  
  up :: Query
  up = undefined
  
  down :: Query
  down = undefined
  
  -- The `Query` datatype is exported by postgresql-simple, which is basically a `String`.
  
  ```

 - `--yaml`: Create a yaml migration file with up and down as keys
 - `--sql` : Create a sql migration file, It doesn't contain an up and a down block, runs the same migration for both the commands.
 
 - `--version fuzzyFile` : Fuzzy match a specific migration file to run migrations in it.
 - `--env environmentName`: `development` is the default environment. If the flag is absent, it uses the `APP_ENV` environment variable.
 
## Structure of `schema_migrations` table
  The table contains one single column `version`, that stores the time stamp of the last run migration.

The database settings are read from a `config.yml` file that should be present in the same directory as the commands that are run. The config file should contain the following keys:
  ``` yaml
  development:
    database:
      name: postgres
      username: postgres
      password: postgres
      host: localhost
      port: 5432
  ```

When run in the development environment, the migrate command creates a schema.sql file that dumps the entire database.
