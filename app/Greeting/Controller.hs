module Greeting.Controller (getGreetingHandler) where

import Greeting.Service (getGreeting)

getGreetingHandler :: IO String
getGreetingHandler = getGreeting