{-# LANGUAGE OverloadedStrings #-}

module Redis (KeyValue(..), saveHandler, deleteHandler) where

import Network.Wai
import Database.Redis (Connection, runRedis, set, del)
import Data.Aeson (decode, encode, object, (.=))
import Data.Aeson.Types (FromJSON(..), ToJSON(..), withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status200, status400)

-- Data type for JSON parsing
data KeyValue = KeyValue
    { key   :: Text
    , value :: Text
    } deriving (Show)

instance FromJSON KeyValue where
    parseJSON = withObject "KeyValue" $ \v ->
        KeyValue <$> v .: "key"
                 <*> v .: "value"

instance ToJSON KeyValue where
    toJSON kv = object
        [ "key"   .= key kv
        , "value" .= value kv
        ]

-- Handler to save data to Redis
saveHandler :: Connection -> Application
saveHandler redisConn req respond = do
    body <- strictRequestBody req
    case decode body of
        Just (KeyValue k v) -> do
            runRedis redisConn $ set (encodeUtf8 k) (encodeUtf8 v)
            respond $ responseLBS status200 [("Content-Type", B8.pack "text/plain")] "Saved to Redis"
        Nothing -> respond $ responseLBS status400 [("Content-Type", B8.pack "text/plain")] "Invalid JSON"

-- Handler to delete data from Redis
deleteHandler :: Connection -> Application
deleteHandler redisConn req respond = do
    body <- strictRequestBody req
    case decode body of
        Just (KeyValue k _) -> do
            runRedis redisConn $ del [encodeUtf8 k]
            respond $ responseLBS status200 [("Content-Type", B8.pack "text/plain")] "Deleted from Redis"
        Nothing -> respond $ responseLBS status400 [("Content-Type", B8.pack "text/plain")] "Invalid JSON"