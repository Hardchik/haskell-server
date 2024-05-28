{-# LANGUAGE OverloadedStrings #-}

module Module.Request.Service.PutService (handlePut) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived)
import Network.HTTP.Types (status200, status500, status400, hContentType)
import qualified Data.ByteString.Char8 as B
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Key)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (ConnectionPool, (=.), (==.), SqlPersistT, runSqlPool, updateWhere, getBy, Entity(..))
import qualified Database.Persist.Sql as DB
import Models (User(..), Unique( UniqueUserUsername ), EntityField( UserUsername, UserEmail ), userEmail)
import Data.String (fromString)
import DTO.DTO (UserDTO, email, username)
import Data.Text (Text, pack)
import qualified Data.Text as T

-- Handle PUT request to update user's email
handlePut :: ConnectionPool -> UserDTO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePut pool jsonBody req respond = do
    let gotUsername = pack $ username jsonBody
        newEmail = pack $ email jsonBody
    result <- try $ updateUserEmail pool gotUsername newEmail
    case result of
        Left ex -> do
            putStrLn $ "Error updating user's email: " ++ show (ex :: SomeException)
            handleInternalServerError respond
        Right updatedUser -> do
            putStrLn "User's email updated successfully"
            handleSuccess updatedUser respond

-- Function to update user's email in the database
updateUserEmail :: ConnectionPool -> Text -> Text -> IO (Maybe User)
updateUserEmail pool username newEmail =
    runSqlPool (updateAndGetUserEmail username newEmail) pool

-- Update user's email and return the updated user
updateAndGetUserEmail :: Text -> Text -> SqlPersistT IO (Maybe User)
updateAndGetUserEmail username newEmail = do
    updateWhere [UserUsername ==. username] [UserEmail =. newEmail]
    maybeEntity <- getBy $ UniqueUserUsername username
    case maybeEntity of
        Just (Entity _ updatedUser) -> return $ Just updatedUser
        Nothing -> return Nothing -- Handle appropriately

-- Handle bad request
handleBadRequest :: (Response -> IO ResponseReceived) -> String -> IO ResponseReceived
handleBadRequest respond errMsg = do
    let responseJson = object [((fromString "error" :: Key) .= errMsg)]
        headers = [(hContentType, "application/json")]
        response = responseLBS status400 headers (encode responseJson)
    respond response

-- Handle internal server error
handleInternalServerError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleInternalServerError respond = do
    let responseJson = object [((fromString "error" :: Key) .= ("Internal Server Error" :: String))]
        headers = [(hContentType, "application/json")]
        response = responseLBS status500 headers (encode responseJson)
    respond response

-- Handle success response
handleSuccess :: Maybe User -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSuccess maybeUser respond = do
    let responseJson = case maybeUser of
            Just user -> object [((fromString "message" :: Key) .= ("User email updated successfully" :: String))
                                ,((fromString "data" :: Key) .= user)
                                ,((fromString "method" :: Key) .= ("PUT" :: String))]
            Nothing -> object [((fromString "message" :: Key) .= ("User not found" :: String))]
        headers = [(hContentType, "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response