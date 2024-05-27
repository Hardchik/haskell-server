module Main (main) where

-- {-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
-- import Data.Aeson (object, (.=))
import Data.Text (Text)
-- import Control.Monad.Logger (runStdoutLoggingT)
import Database (withDatabaseConnection)
import Database.Persist.Sql (ConnectionPool)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (createPostgresqlPool)
-- import qualified Data.Text.IO as TIO
-- import System.Process (callProcess)
import Module.Greeting.RouteHandler (greetingHandler)
import Module.Post.RouteHandler (postHandler)
import Module.Request.RouteHandler (requestHandler)
import Module.NotFound.RouteHandler (notFoundHandler)

-- Define routes
routes :: ConnectionPool -> [([Text], Application)]
routes pool =
    [ ([T.pack "greeting"], greetingHandler)
    , ([T.pack "post"], postHandler)
    , ([T.pack "request"], requestHandler)
    , ([T.pack "unknown"], notFoundHandler)
    ]

-- Router function
router :: ConnectionPool -> Application
router pool req respond = do
    let path = pathInfo req
       -- pathText = mconcat path
    -- TIO.putStrLn pathText
    -- Route the request
    case lookup path (routes pool) of
        Just handler -> handler req respond
        Nothing      -> notFoundHandler req respond

main :: IO ()
main = withDatabaseConnection $ \pool -> do
    -- callProcess "stack" ["runghc", "migration.hs"]
    putStrLn "Server started at http://localhost:8000/"
    run 8000 (router pool)
