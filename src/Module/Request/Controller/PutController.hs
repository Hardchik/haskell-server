module Module.Request.Controller.PutController (handlePutRequest) where

import Network.Wai (Application, Request, Response, ResponseReceived)
import Module.Request.Service.PutService (handlePut)
import Middleware.Middleware (validateJsonBody)
import DTO.DTO (UserDTO)
import Data.Aeson.Types (Value)
import Database.Persist.Sql (ConnectionPool)

-- Define a function to handle post requests in the controller
handlePutRequest :: ConnectionPool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handlePutRequest pool req respond = validateJsonBody (handlePutLogic pool) req respond


-- Define a function to handle the logic after validating the request body
handlePutLogic :: ConnectionPool -> UserDTO -> Application
handlePutLogic pool jsonData req respond = handlePut pool jsonData req respond