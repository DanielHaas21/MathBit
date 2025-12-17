module Struct.Step (Step(..)) where

import Struct.Expr    

data Step = Step
  { before :: Expr
  , after  :: Expr
  , description :: String
  , rule   :: String
  } deriving (Show)