module Module.Post.Service (processPostRequest) where

import Network.Wai (Request(..), strictRequestBody)
import Network.HTTP.Types (Method, methodPost)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

processPostRequest :: Request -> IO (Maybe B.ByteString)
processPostRequest req =
    if requestMethod req == methodPost
        then do
            body <- strictRequestBody req
            return $ Just (LB.toStrict body)
        else return Nothing