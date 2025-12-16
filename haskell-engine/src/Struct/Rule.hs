module Struct.Rule (Rule(..)) where 

import Struct.Expr    

data Rule = Rule
    { ruleName :: String -- Refers to function name of the rule
    , ruleDescription :: String -- text description of what the rule does / should do
    , priority :: Int -- higher = applied first
    , apply    :: Expr -> Maybe Expr 
    }    