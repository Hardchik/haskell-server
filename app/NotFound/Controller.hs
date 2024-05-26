module NotFound.Controller (getNotFoundHandler) where

import NotFound.Service (getNotFound)

getNotFoundHandler :: IO String
getNotFoundHandler = getNotFound