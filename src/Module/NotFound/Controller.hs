module Module.NotFound.Controller (getNotFoundHandler) where

import Module.NotFound.Service (getNotFound)

getNotFoundHandler :: IO String
getNotFoundHandler = getNotFound