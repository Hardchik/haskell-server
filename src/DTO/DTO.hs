{-# LANGUAGE OverloadedStrings #-}

module DTO.DTO (UserDTO(..), OtherDTO(..)) where

import Data.Aeson
import Data.Aeson.Types (Key)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)
import Data.Time (UTCTime)

-- Define UserDTO
data UserDTO = UserDTO
    { username :: String
    , email :: String
    } deriving (Show)

instance FromJSON UserDTO where
    parseJSON (Object v) = UserDTO
        <$> v .: (fromString "username" :: Key)
        <*> v .: (fromString "email" :: Key)
    parseJSON _ = fail "Invalid UserDTO JSON"

instance ToJSON UserDTO where
    toJSON (UserDTO username email) = object
        [ (fromString "username" :: Key) .= username
        , (fromString "email" :: Key) .= email
        ]

-- Define OtherDTO (example)
data OtherDTO = OtherDTO
    { someField :: Int
    , anotherField :: String
    } deriving (Show)

instance FromJSON OtherDTO where
    parseJSON (Object v) = OtherDTO
        <$> v .: (fromString "someField" :: Key)
        <*> v .: (fromString "anotherField" :: Key)
    parseJSON _ = fail "Invalid OtherDTO JSON"

instance ToJSON OtherDTO where
    toJSON (OtherDTO someField anotherField) = object
        [ (fromString "someField" :: Key) .= someField
        , (fromString "anotherField" :: Key) .= anotherField
        ]

-- Define ProductDTO
data ProductDTO = ProductDTO
    { name :: String
    , price :: Float
    , margin :: Int
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , currency :: String
    } deriving (Show)

instance FromJSON ProductDTO where
    parseJSON (Object v) = ProductDTO
        <$> v .: (fromString "name" :: Key)
        <*> v .: (fromString "price" :: Key)
        <*> v .: (fromString "margin" :: Key)
        <*> v .: (fromString "createdAt" :: Key)
        <*> v .: (fromString "updatedAt" :: Key)
        <*> v .: (fromString "currency" :: Key)
    parseJSON _ = fail "Invalid ProductDTO JSON"

instance ToJSON ProductDTO where
    toJSON (ProductDTO name price margin createdAt updatedAt currency) = object
        [ (fromString "name" :: Key) .= name
        , (fromString "price" :: Key) .= price
        , (fromString "margin" :: Key) .= margin
        , (fromString "createdAt" :: Key) .= createdAt
        , (fromString "updatedAt" :: Key) .= updatedAt
        , (fromString "currency" :: Key) .= currency
        ]