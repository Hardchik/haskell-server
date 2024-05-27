module Module.Request.Controller.PostController (handlePostRequest) where

import Network.Wai (Application, Request, Response, ResponseReceived)
import Module.Request.Service.PostService (handlePost)
import Middleware.Middleware (validateJsonBody)
import DTO.DTO (UserDTO)
import Data.Aeson.Types (Value)
import Database.Persist.Sql (ConnectionPool)

-- Define a function to handle post requests in the controller
handlePostRequest :: ConnectionPool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePostRequest pool req respond = validateJsonBody (handlePostLogic pool) req respond


-- Define a function to handle the logic after validating the request body
handlePostLogic :: ConnectionPool -> UserDTO -> Application
handlePostLogic pool jsonData req respond = handlePost pool jsonData req respond
