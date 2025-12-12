module Validator ( 
    Validate
)where

import Data.Aeson
import Struct.Expr (Expr(..))
import GHC.Generics (Generic,)
import Data.Aeson.Types (Parser)

data StepData = StepData {
    isIntegral :: Bool,
    isCombinatorical :: Bool,
    isTrigo :: Bool,
    isFunc :: Bool
} deriving (Show, Generic)

validator :: Parser Expr -> StepData
validator expr args case epxr of

