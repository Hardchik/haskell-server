{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    let connInfo = defaultConnectInfo
            { connectHost = "localhost"
            , connectUser = "postgres"
            , connectPassword = "root"
            -- , connectDatabase = "postgres" -- Connect to the default 'postgres' database
            }
    result <- try (connect connInfo) :: IO (Either SomeException Connection)
    case result of
        Left ex -> do
            let errorMsg = "Failed to connect to PostgreSQL server: " ++ show ex
            putStrLn errorMsg
            -- Additional action if connection fails
            putStrLn "Database creation failed."
        Right conn -> do
            putStrLn "Connected to PostgreSQL server."
            createDatabaseIfNotExists conn
            putStrLn "Database created if it did not exist."
            close conn

createDatabaseIfNotExists :: Connection -> IO ()
createDatabaseIfNotExists conn = do
    -- Check if the database exists
    [Only exists] <- query conn "SELECT EXISTS (SELECT FROM pg_database WHERE datname = 'mydb')" ()
    if exists then
        putStrLn "Database already exists."
    else do
        execute_ conn "CREATE DATABASE mydb"
        putStrLn "Database created."