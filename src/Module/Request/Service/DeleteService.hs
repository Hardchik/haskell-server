{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Request.Service.DeleteService (handleDelete) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived, strictRequestBody)
import Network.HTTP.Types (badRequest400, status200, status404, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, FromJSON(..), withObject, (.:), decode, object, (.=), eitherDecode')
import Data.Aeson.Types (Key)
import Data.String (fromString)
import Control.Exception (try, SomeException)
import Models (User(..))
import Database (runDb)
import Database.Persist (delete)
import Database.Persist.Sql (ConnectionPool, toSqlKey)
import qualified Database.Persist.Class as DB

handleDelete :: ConnectionPool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleDelete pool req respond = do
    -- Extract user ID from request body
    body <- strictRequestBody req
    case decode body of
        Nothing -> respond $ badRequestResponse "Invalid JSON format"
        Just (jsonBody :: JsonRequest) -> do
            putStrLn $ "Body contains: " ++ show jsonBody
            let userIdValue = userId jsonBody
            deleted <- deleteUser pool userIdValue
            if deleted
                then handleSuccess respond
                else handleNotFoundError respond

-- Define a data type to represent the JSON request body
data JsonRequest = JsonRequest
    { userId :: Int
    } deriving (Show)

-- Define how to parse the JSON request body into JsonRequest
instance FromJSON JsonRequest where
    parseJSON = withObject "json" $ \obj ->
        JsonRequest <$> obj .: "userId"

-- Function to delete a user from the database and return a boolean indicating success or failure
deleteUser :: ConnectionPool -> Int -> IO Bool
deleteUser pool userId = do
    result <- try $ runDb (delete (toSqlKey (fromIntegral userId) ::DB.Key User)) pool
    case result of
        Left (ex :: SomeException) -> do
            putStrLn $ "Error deleting user: " ++ show ex
            return False
        Right _ -> return True

badRequestResponse :: String -> Response
badRequestResponse err = responseLBS badRequest400 [(hContentType, B.pack "application/json")] $ encode $ object [(fromString "error" :: Key) .= err]

-- Handle not found error
handleNotFoundError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleNotFoundError respond = do
    let responseJson = object [(fromString "error" :: Key) .= ("User not found" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status404 headers (encode responseJson)
    respond response

-- Handle success response
handleSuccess :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSuccess respond = do
    let responseJson = object [(fromString "message" :: Key) .= ("User deleted successfully" :: String)
                              ,(fromString "method" :: Key) .= ("DELETE" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response