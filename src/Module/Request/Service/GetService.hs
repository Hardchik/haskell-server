{-# LANGUAGE OverloadedStrings #-}

module Module.Request.Service.GetService (handleGet) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived, queryString)
import Network.HTTP.Types (status200, status404, status400, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Key)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.String (fromString)
import Database.Persist.Sql (ConnectionPool, Entity(..), runSqlPool, getBy, selectList, entityVal)
import Models (User(..), Unique(UniqueUserUsername))

handleGet :: ConnectionPool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGet pool req respond = do
    let maybeSingle = lookup "single" (queryString req) >>= (>>= (Just . decodeUtf8))
    case maybeSingle of
        Just "true"  -> handleSingleUserRequest pool req respond
        Just "false" -> handleAllUsersRequest pool respond
        _            -> handleBadRequest respond "Invalid or missing 'single' parameter"

-- Handle request for single user information
handleSingleUserRequest :: ConnectionPool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSingleUserRequest pool req respond = do
    let maybeUsername = lookupUsernameFromQueryParams req
    case maybeUsername of
        Nothing -> handleBadRequest respond "Username not provided in query parameters"
        Just username -> do
            maybeUser <- getUserByUsername pool username
            case maybeUser of
                Nothing -> handleNotFoundError respond
                Just user -> handleSuccess user respond

-- Handle request for all users information
handleAllUsersRequest :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleAllUsersRequest pool respond = do
    users <- getAllUsers pool
    handleSuccessAll users respond

-- Function to extract username from query parameters
lookupUsernameFromQueryParams :: Request -> Maybe Text
lookupUsernameFromQueryParams req =
    case lookup "username" (queryString req) of
        Just (Just x) -> Just $ decodeUtf8 x
        _             -> Nothing

-- Function to fetch user details by username from the database
getUserByUsername :: ConnectionPool -> Text -> IO (Maybe User)
getUserByUsername pool username = do
    maybeEntity <- runSqlPool (getBy $ UniqueUserUsername username) pool
    case maybeEntity of
        Just (Entity _ user) -> return $ Just user
        Nothing              -> return Nothing

-- Function to fetch all users from the database
getAllUsers :: ConnectionPool -> IO [User]
getAllUsers pool = do
    entities <- runSqlPool (selectList [] []) pool
    return $ map entityVal entities

-- Handle bad request
handleBadRequest :: (Response -> IO ResponseReceived) -> String -> IO ResponseReceived
handleBadRequest respond errMsg = do
    let responseJson = object [((fromString "error" :: Key) .= errMsg)]
        headers = [(hContentType, "application/json")]
        response = responseLBS status400 headers (encode responseJson)
    respond response

-- Handle not found error
handleNotFoundError :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleNotFoundError respond = do
    let responseJson = object [((fromString "error" :: Key) .= ("User not found" :: String))]
        headers = [(hContentType, "application/json")]
        response = responseLBS status404 headers (encode responseJson)
    respond response

-- Handle success response for single user
handleSuccess :: User -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSuccess user respond = do
    let responseJson = object [((fromString "message" :: Key) .= ("User details fetched successfully" :: String))
                              ,((fromString "data" :: Key) .= user)
                              ,((fromString "method" :: Key) .= ("GET" :: String))]
        headers = [(hContentType, "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response

-- Handle success response for all users
handleSuccessAll :: [User] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleSuccessAll users respond = do
    let responseJson = object [((fromString "message" :: Key) .= ("Users details fetched successfully" :: String))
                              ,((fromString "data" :: Key) .= users)
                              ,((fromString "method" :: Key) .= ("GET" :: String))]
        headers = [(hContentType, "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response
