
module Module.Request.RouteHandler (requestHandler) where

import Network.Wai (responseLBS, requestMethod, Application, Response, ResponseReceived)
import Network.HTTP.Types (hContentType, status405, methodGet, methodPost, methodPut, methodDelete)
import qualified Module.Request.Controller.PostController as PostController
import qualified Module.Request.Service.GetService as GetService
import qualified Module.Request.Controller.PutController as PutController
import qualified Module.Request.Service.DeleteService as DeleteService
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types (Key)
import Data.String (fromString)
import Database.Persist.Sql (ConnectionPool)

requestHandler :: ConnectionPool -> Application
requestHandler pool req respond = do
    let reqMethod = requestMethod req
    if reqMethod == methodPost
        then PostController.handlePostRequest pool req respond
        else if reqMethod == methodGet
            then GetService.handleGet pool req respond
            else if reqMethod == methodPut
                then PutController.handlePutRequest pool req respond
                else if reqMethod == methodDelete
                    then DeleteService.handleDelete pool req respond
                    else handleMethodNotAllowed respond

handleMethodNotAllowed :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleMethodNotAllowed respond = do
    let responseJson = object [(fromString "error" :: Key) .= ("Method Not Allowed" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status405 headers (encode responseJson)
    respond response
