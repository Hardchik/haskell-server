{-# LANGUAGE OverloadedStrings #-}

module Module.Request.Service.PostService (handlePost) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived, strictRequestBody)
import Network.HTTP.Types (status200, status404, status500, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Aeson (encode, object, (.=), decode)
import Data.Aeson.Types (Key, Value)
import Data.String (fromString)
import Control.Exception (try, SomeException)
import Database.Persist.Sql (runSqlPool)
import Models (User(..))
import qualified Data.Text.Encoding as TE
import DTO.DTO (UserDTO(..))
import Database (runDb)
import Database.Persist (insert)
import Database.Persist.Sql (ConnectionPool)
import Data.Text (pack, Text)
import Database.Persist.Class (PersistEntity)
import qualified Database.Persist.Class as DB

handlePost :: ConnectionPool -> UserDTO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePost pool jsonBody req respond = do
    -- Convert JSON body to User entity
    let userUsername = pack $ username jsonBody
        userEmail = pack $ email jsonBody
    -- Try to insert the user into the database
    result <- try $ insertUser pool userUsername userEmail
    case result of
        Left ex -> do
            putStrLn $ "Error inserting user: " ++ show (ex :: SomeException)
            handleInternalServerError respond
        Right _ -> do
            putStrLn "User inserted successfully"
            handleSuccess jsonBody respond

-- Function to insert a new user into the database
insertUser :: ConnectionPool -> Text -> Text -> IO (DB.Key User)
insertUser pool userUsername userEmail = do
    runDb (insert $ User userUsername userEmail) pool

-- Handle internal server error
handleInternalServerError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleInternalServerError respond = do
    let responseJson = object [(fromString "error" :: Key) .= ("Internal Server Error" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status500 headers (encode responseJson)
    respond response

-- Handle success response
handleSuccess :: UserDTO -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSuccess jsonBody respond = do
    let responseJson = object [(fromString "message" :: Key) .= ("User created successfully" :: String)
                              ,(fromString "data" :: Key) .= jsonBody
                              ,(fromString "method" :: Key) .= ("POST" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response