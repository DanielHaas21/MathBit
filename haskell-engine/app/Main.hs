{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import GHC.Generics
import Parser
import Servant
import Struct.Expr (Expr(..))
import Engine.Engine (simplifyWithLog)
import Cleanup (cleanupExpr)
import Struct.Step (Step(..))
import Api (SolveRequest(..), SolveResponse(..),SolveResponseStep(..) ,ErrorResponse(..), jsonError)
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.IO.Class (liftIO)
import Print (renderLatex)
import qualified Data.Text as Text

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
            Just v -> 
                case AesonTypes.parseEither fromMathJSON v of
                    Left err ->
                        throwError $ jsonError 400 "Parse Error" err

                    Right expr -> do
                        -- Log parsed AST
                        liftIO $ putStrLn "Parsed AST:"
                        liftIO $ print expr

                        -- Run simplification with log
                        let (simplifiedExpr, logSteps) = simplifyWithLog expr
                        let cleanedExpr = cleanupExpr simplifiedExpr
                        liftIO $ putStrLn "Cleaned AST:"
                        liftIO $ print cleanedExpr
                        let prettyExpr = Text.unpack (renderLatex cleanedExpr)
                        liftIO $ putStrLn "Pretty print:"
                        liftIO $ print prettyExpr
                        -- Optionally print log
                        liftIO $ putStrLn "Simplification steps:"
                        mapM_ (liftIO . print) logSteps

                        pure SolveResponse
                            { finalExpression = prettyExpr
                            , steps = Just $ map toStep logSteps
                            }

-- Convert engine Step to API Step
toStep :: Step -> SolveResponseStep
toStep s =
  let
    cleanedBefore = cleanupExpr (before s)
    cleanedAfter  = cleanupExpr (after s)
    prettyBefore  = Text.unpack (renderLatex cleanedBefore)

    prettyAfter   = Text.unpack (renderLatex cleanedAfter)
  in SolveResponseStep
       { stepBefore = prettyBefore
       , stepAfter  = prettyAfter
       , stepRule   = rule s
       , stepRuleDescription = description s
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
