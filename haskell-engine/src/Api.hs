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

-- This module contains definitions for the exposed API endpoint, that is in and out data JSON types 

data SolveRequest = SolveRequest {
    rawExpression :: String    
} deriving (Show, Generic)

data SolveResponse = SolveResponse {
    finalExpression :: String    
} deriving (Show, Generic)

data ErrorResponse = ErrorResponse
  { errHTTPCode :: Int
  , errReason   :: String
  , errMessage     :: String
  } deriving (Generic, Show)

instance ToJSON ErrorResponse

jsonError :: Int -> String -> String -> ServerError
jsonError code reason msg =
  let body = encode $ ErrorResponse code reason msg
  in err400
      { errReasonPhrase = reason
      , errBody = body  -- now unambiguous: refers to ServerError.errBody
      , errHeaders = [("Content-Type", "application/json")]
      }
instance FromJSON SolveRequest
instance ToJSON SolveResponse

