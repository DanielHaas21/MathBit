{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (ToJSON, FromJSON, encode, Value)

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

-- This module contains definitions for the exposed API endpoint, that is in and out data JSON types and Error handling 

-- Arbitrary value since the raw expression is Array of (N) dimensions
data SolveRequest = SolveRequest {
  rawExpression :: Maybe Value  
} deriving (Show, Generic)

-- Similar to Step in engine
data SolveResponseStep = SolveResponseStep
  { stepBefore :: String -- step before, this approach help create a chain-like pattern
  , stepAfter  :: String -- step after, this approach help create a chain-like pattern
  , stepRule   :: String -- Name of the rule, mostly functional, likely wont be shown to the user
  , stepRuleDescription :: String -- Explains what the rule does
  } deriving (Show, Generic)
instance ToJSON SolveResponseStep

data SolveResponse = SolveResponse {
  finalExpression :: String,
  steps :: Maybe [SolveResponseStep]    
} deriving (Show, Generic)

instance FromJSON SolveRequest
instance ToJSON SolveResponse

data ErrorResponse = ErrorResponse
  { errHTTPCode :: Int
  , errReason   :: String
  , errMessage     :: String
  } deriving (Generic, Show)

instance ToJSON ErrorResponse

-- Servant already has its own ServerError type, but we can create a helper function to put our ErrorResponse into it
jsonError :: Int -> String -> String -> ServerError
jsonError code reason msg =
  let body = encode $ ErrorResponse code reason msg
  in err400
      { errReasonPhrase = reason
      , errBody = body 
      , errHeaders = [("Content-Type", "application/json")]
      }


