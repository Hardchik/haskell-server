-- src/RabbitMQ.hs
{-# LANGUAGE OverloadedStrings #-}

module RabbitMQ (runProducer, runConsumer, publishMessage) where

import Network.AMQP
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Data.ByteString.Lazy.Char8 (pack)

-- Function to run RabbitMQ producer
runProducer :: IO ()
runProducer = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareQueue chan newQueue {queueName = "haskellQueue"}
    publishMsg chan "" "haskellQueue" newMsg { msgBody = "Hello from Haskell RabbitMQ Producer!", msgDeliveryMode = Just Persistent}
    putStrLn "Message published"
    closeConnection conn

-- Function to run RabbitMQ consumer
runConsumer :: IO ()
runConsumer = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareQueue chan newQueue {queueName = "haskellQueue"}
    consumeMsgs chan "haskellQueue" Ack myCallback
    putStrLn "Waiting for messages..."
    forever getLine
    closeConnection conn

-- Callback function for the consumer
myCallback :: (Message, Envelope) -> IO ()
myCallback (msg, env) = do
    putStrLn $ "Received message: " ++ (show $ msgBody msg)
    ackEnv env

-- Function to publish a message to RabbitMQ
publishMessage :: String -> IO ()
publishMessage msg = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    declareQueue chan newQueue {queueName = "haskellQueue"}
    publishMsg chan "" "haskellQueue" newMsg {msgBody = pack msg, msgDeliveryMode = Just Persistent}
    putStrLn $ "Message published: " ++ msg
    closeConnection conn
