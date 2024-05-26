module Request.Service.PostService (handlePost) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived, strictRequestBody)
import Network.HTTP.Types (status200, status404, hContentType)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Aeson (encode, object, (.=), decode)
import Data.Aeson.Types (Key, Value)
import Data.String (fromString)

handlePost :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePost req respond = do
    body <- strictRequestBody req
    putStrLn $ "Received JSON data: " ++ LB8.unpack body
    let maybeJson = decode body :: Maybe Value
    case maybeJson of
        Just jsonValue -> do
            let responseJson = object [(fromString "message" :: Key) .= ("JSON data received" :: String),
                                       (fromString "method" :: Key) .= ("POST" :: String),
                                       (fromString "data" :: Key) .= jsonValue]
                headers = [(hContentType, B.pack "application/json")]
                response = responseLBS status200 headers (encode responseJson)
            respond response
        Nothing -> do
            let responseJson = object [(fromString "error" :: Key) .= ("Invalid JSON" :: String)]
                headers = [(hContentType, B.pack "application/json")]
                response = responseLBS status404 headers (encode responseJson)
            respond response