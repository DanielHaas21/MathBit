module Struct.Step (Step(..)) where

import Struct.Expr    

data Step = Step
  { before :: Expr
  , after  :: Expr
  , description :: String -- Explains what the rule does
  , rule   :: String -- Dev only purposses 
  } deriving (Show)