module Module.Greeting.Controller (getGreetingHandler) where

import Module.Greeting.Service (getGreeting)

getGreetingHandler :: IO String
getGreetingHandler = getGreeting