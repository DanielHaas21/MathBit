module Engine
    ( simplify
) where


import Struct.Expr
import Validator (StepData(..), validator)

simplify :: Expr -> Expr
simplify expr =
    let meta = validator expr -- validated metadata foreach step
    in simplifyWith meta expr


simplifyWith :: StepData -> Expr -> Expr