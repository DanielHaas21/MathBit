module Struct.Rule (Rule(..)) where 

import Struct.Expr    

data Rule = Rule
    { ruleName :: String
    , apply    :: Expr -> Maybe Expr
    }    