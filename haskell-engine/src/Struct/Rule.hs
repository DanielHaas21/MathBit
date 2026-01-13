module Struct.Rule (Rule(..), RulePhase(..)) where 

import Struct.Expr    

-- Rule phases for categorizing rules
data RulePhase
  = Structural   -- normalization, cancellation-enabling
  | Rewriting    -- expansion, distribution, heuristics
  deriving (Eq)

data Rule = Rule
    { ruleName :: String -- Refers to function name of the rule
    , ruleDescription :: String -- text description of what the rule does / should do
    , priority :: Int -- higher = applied first
    , phase    :: RulePhase -- phase of the rule
    , apply    :: Expr -> Maybe Expr 
    }    