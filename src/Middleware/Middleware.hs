module Middleware.Middleware (validateJsonBody) where

import Network.Wai (responseLBS, strictRequestBody, Application, Response)
import Network.HTTP.Types (hContentType, badRequest400)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.String (fromString)

validateJsonBody :: FromJSON a => (a -> Application) -> Application
validateJsonBody app req respond = do
    body <- strictRequestBody req
    let result = eitherDecode' body
    case result of
        Left err -> respond $ badRequestResponse err
        Right jsonBody -> appWithBody jsonBody req respond
  where
    -- Function to construct an application with the JSON body
    appWithBody jsonBody req' respond' = do
        let decoded = fromJSON jsonBody
        case decoded of
            Error err -> respond' $ badRequestResponse err
            Success dto -> app dto req' respond'

badRequestResponse :: String -> Response
badRequestResponse err = responseLBS badRequest400 [(hContentType, B.pack "application/json")] $ encode $ object [(fromString "error" :: Key) .= err]
