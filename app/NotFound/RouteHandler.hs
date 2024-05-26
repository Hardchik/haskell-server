module NotFound.RouteHandler (notFoundHandler) where

import Network.Wai
import Network.HTTP.Types (status404, hContentType)
import NotFound.Controller (getNotFoundHandler)
import Data.Aeson.Types (Key)
import Data.String (fromString)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as B

notFoundHandler :: Application
notFoundHandler _ respond = do
    notfound <- getNotFoundHandler
    let jsonData = encode $ object [(fromString "data" :: Key) .= notfound]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status404 headers jsonData
    respond response