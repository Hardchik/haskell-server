module Module.Request.Service.DeleteService (handleDelete) where

import Network.Wai (Request, Response, responseLBS, ResponseReceived)
import Network.HTTP.Types (status200, hContentType)
import qualified Data.ByteString.Char8 as B
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Key)
import Data.String (fromString)

handleDelete :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleDelete _ respond = do
    let responseJson = object [(fromString "message" :: Key) .= ("DELETE request received" :: String),
                                       (fromString "method" :: Key) .= ("DELETE" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status200 headers (encode responseJson)
    respond response