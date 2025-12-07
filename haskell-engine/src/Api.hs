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

import Data.Aeson (ToJSON, FromJSON, encode)

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

-- This module contains definitions for the exposed API endpoint, that is in and out data JSON types and Error handling 

data SolveRequest = SolveRequest {
  rawExpression :: Maybe String    
} deriving (Show, Generic)

data SolveResponseStep = SolveResponseStep {
  expressionStep :: String, 
  explanationStep :: String
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


