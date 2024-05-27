{-# LANGUAGE OverloadedStrings #-}

module Database
  ( withDatabaseConnection
  , runDb
  ) where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (SqlPersistT, ConnectionPool, runSqlPool)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Models (migrateAll)

connectionString :: ConnectionString
connectionString = "host=localhost dbname=mydb user=postgres password=root port=5432"

withDatabaseConnection :: (ConnectionPool -> IO ()) -> IO ()
withDatabaseConnection action = runStdoutLoggingT $ withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
    -- flip runSqlPersistMPool pool $ runMigration migrateAll
    runSqlPool (runMigration migrateAll) pool
    action pool

runDb :: SqlPersistT IO a -> ConnectionPool -> IO a
runDb query pool = runSqlPool query pool
