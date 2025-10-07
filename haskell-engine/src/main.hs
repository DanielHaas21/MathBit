{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- this is an ai generated test route for TSOA api intergration
--will be refactored and expanded later in the future 

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

-- Define API type
type API = "simplify" :> Get '[PlainText] String

-- Implement server
server :: Server API
server = return "Hello from Haskell microservice!"

-- Generate WAI application
app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  putStrLn "✅ Starting Haskell microservice on port 8081"
  run 8081 app

