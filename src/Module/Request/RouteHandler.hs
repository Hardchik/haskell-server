
module Module.Request.RouteHandler (requestHandler) where

import Network.Wai (responseLBS, requestMethod, Application, Response, ResponseReceived)
import Network.HTTP.Types (hContentType, status405, methodGet, methodPost, methodPut, methodDelete)
import qualified Module.Request.Controller.PostController as PostController
import qualified Module.Request.Service.GetService as GetService
import qualified Module.Request.Service.PutService as PutService
import qualified Module.Request.Service.DeleteService as DeleteService
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as B
import Data.Aeson.Types (Key)
import Data.String (fromString)

requestHandler :: Application
requestHandler req respond = do
    let reqMethod = requestMethod req
    if reqMethod == methodPost
        then PostController.handlePostRequest requestHandler req respond
        else if reqMethod == methodGet
            then GetService.handleGet req respond
            else if reqMethod == methodPut
                then PutService.handlePut req respond
                else if reqMethod == methodDelete
                    then DeleteService.handleDelete req respond
                    else handleMethodNotAllowed respond

handleMethodNotAllowed :: (Response -> IO ResponseReceived) -> IO ResponseReceived
handleMethodNotAllowed respond = do
    let responseJson = object [(fromString "error" :: Key) .= ("Method Not Allowed" :: String)]
        headers = [(hContentType, B.pack "application/json")]
        response = responseLBS status405 headers (encode responseJson)
    respond response