{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import GHC.Generics
import Servant
import Api (SolveRequest(..), SolveResponse(..), ErrorResponse(..), jsonError)

type API = "solve" :> ReqBody '[JSON] SolveRequest :> Post '[JSON] SolveResponse



server :: Server API
server = solve where
    solve :: SolveRequest -> Handler SolveResponse
    solve req =
        if null (rawExpression req) then
            throwError $ jsonError 400 "Bad Request" "rawExpression cannot be empty"
        else
            pure SolveResponse {
                finalExpression = "Solved: " <> rawExpression req  -- solving logic will be implemented
            }


api :: Proxy API
api = Proxy

app :: Application
app = serve api server


main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Listening on port " <> show port
    run port app
