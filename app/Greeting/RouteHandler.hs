module Greeting.RouteHandler (greetingHandler) where

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Greeting.Controller (getGreetingHandler)
import Data.Aeson.Types (Key)
import Data.String (fromString)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as B

greetingHandler :: Application
greetingHandler _ respond = do
    greeting <- getGreetingHandler
    let jsonData = encode $ object [(fromString "data" :: Key) .= greeting]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status200 headers jsonData
    respond response