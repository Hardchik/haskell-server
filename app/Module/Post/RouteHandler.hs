module Module.Post.RouteHandler (postHandler) where

import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200, status404, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Aeson (encode, object, (.=), decode)
import Data.Aeson.Types (Key, Value)
import Data.String (fromString)
import Module.Post.Service (processPostRequest)

postHandler :: Application
postHandler req respond = do
    maybeBody <- processPostRequest req
    case maybeBody of
        Just body -> do
            putStrLn $ "Received JSON data: " ++ B.unpack body
            let maybeJson = decode (LB.fromStrict body) :: Maybe Value
            case maybeJson of
                Just jsonValue -> do
                    let responseJson = object [(fromString "message" :: Key) .= ("JSON data received" :: String),
                                               (fromString "data" :: Key) .= jsonValue]
                        headers = [(hContentType, B.pack "application/json")]
                        response = responseLBS status200 headers (encode responseJson)
                    respond response
                Nothing -> do
                    let responseJson = object [(fromString "error" :: Key) .= ("Invalid JSON" :: String)]
                        headers = [(hContentType, B.pack "application/json")]
                        response = responseLBS status404 headers (encode responseJson)
                    respond response
        Nothing -> do
            let responseJson = object [(fromString "error" :: Key) .= ("Only POST method allowed" :: String)]
                headers = [(hContentType, B.pack "application/json")]
                response = responseLBS status404 headers (encode responseJson)
            respond response
