{-# LANGUAGE OverloadedStrings #-}

module Module.Request.Service.PostService (handlePost) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived, strictRequestBody)
import Network.HTTP.Types (status200, status404, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Aeson (encode, object, (.=), decode)
import Data.Aeson.Types (Key, Value)
import Data.String (fromString)
import DTO.DTO (UserDTO)

handlePost :: UserDTO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePost jsonBody req respond = do
    body <- strictRequestBody req
    putStrLn $ "Received JSON data: " ++ LB8.unpack (encode jsonBody)
    let responseJson = object [(fromString "message" :: Key) .= ("JSON data received" :: String),
                               (fromString "method" :: Key) .= ("POST" :: String),
                               (fromString "data" :: Key) .= jsonBody]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response
