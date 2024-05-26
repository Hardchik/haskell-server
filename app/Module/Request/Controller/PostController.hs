module Module.Request.Controller.PostController (handlePostRequest) where

import Network.Wai (Application, Request, Response, ResponseReceived)
import Module.Request.Service.PostService (handlePost)
import Middleware.Middleware (validateJsonBody)
import DTO.DTO (UserDTO)
import Data.Aeson.Types (Value)

-- Define a function to handle post requests in the controller
handlePostRequest :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePostRequest app req respond = validateJsonBody handlePostLogic req respond


-- Define a function to handle the logic after validating the request body
handlePostLogic :: UserDTO -> Application
handlePostLogic jsonData req respond = handlePost jsonData req respond
