{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


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
import Database.Redis (checkedConnect, defaultConnectInfo, Connection)
import Redis (saveHandler, deleteHandler)

-- Define routes
routes :: ConnectionPool -> Connection -> [([Text], Application)]
routes pool redisConn =
    [ ([T.pack "greeting"], greetingHandler)
    , ([T.pack "post"], postHandler)
    , ([T.pack "request"], requestHandler pool)
    , ([T.pack "unknown"], notFoundHandler)
    , ([T.pack "redis", T.pack "save"], saveHandler redisConn)
    , ([T.pack "redis", T.pack "delete"], deleteHandler redisConn)
    ]

-- Router function
router :: ConnectionPool -> Connection -> Application
router pool redisConn req respond = do
    let path = pathInfo req
       -- pathText = mconcat path
    -- TIO.putStrLn pathText
    -- Route the request
    case lookup path (routes pool redisConn) of
        Just handler -> handler req respond
        Nothing      -> notFoundHandler req respond

main :: IO ()
main = withDatabaseConnection $ \pool -> do
    redisConn <- checkedConnect defaultConnectInfo
    -- callProcess "stack" ["runghc", "migration.hs"]
    putStrLn "Server started at http://localhost:8000/"
    run 8000 (router pool redisConn)
