{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Test

type API = Get '[PlainText] String

server :: Server API
server = pure "Haskell Engine is running"

x :: Test
x = "Test string"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
    let port = 8081
    putStrLn $ "Listening on port " <> show port
    putStrLn $ "Test value: " <> x
    run port app
