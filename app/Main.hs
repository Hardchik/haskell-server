-- main.hs
module Main (main) where

-- {-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
-- import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
-- import Data.Aeson (object, (.=))
import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
import Greeting.RouteHandler (greetingHandler)
import Post.RouteHandler (postHandler)
import Request.RouteHandler (requestHandler)
import NotFound.RouteHandler (notFoundHandler)

-- Define routes
routes :: [([Text], Application)]
routes =
    [ ([T.pack "greeting"], greetingHandler)
    , ([T.pack "post"], postHandler)
    , ([T.pack "request"], requestHandler)
    , ([T.pack "unknown"], notFoundHandler)
    ]

-- Router function
router :: Application
router req respond = do
    let path = pathInfo req
       -- pathText = mconcat path
    -- TIO.putStrLn pathText
    -- Route the request
    case lookup path routes of
        Just handler -> handler req respond
        Nothing      -> notFoundHandler req respond

main :: IO ()
main = do
    putStrLn "Server started at http://localhost:8000/"
    run 8000 router
