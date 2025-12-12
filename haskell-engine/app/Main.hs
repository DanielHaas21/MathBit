{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import GHC.Generics
import Servant
import Api (SolveRequest(..), SolveResponse(..),SolveResponseStep(..) , ErrorResponse(..), jsonError)

type API = "solve" :> ReqBody '[JSON] SolveRequest :> Post '[JSON] SolveResponse



server :: Server API
server = solve where
    solve :: SolveRequest -> Handler SolveResponse
    solve req = 
        case rawExpression req of 
        Nothing -> 
            throwError $ jsonError 400 "Bad Request" "rawExpression is required"
        Just Null ->
            throwError $ jsonError 400 "Bad Request" "rawExpression cannot be empty"
        Just expr ->
            pure SolveResponse {
                    finalExpression = "Solved: "  -- solving logic will be implemented
                    , steps = Just [SolveResponseStep {
                        expressionStep = "Step 1: ", -- example step notation
                        explanationStep = "This is the first step."
                    }]
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
