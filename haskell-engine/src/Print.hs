module Print where

import Struct.Expr
import Prettyprinter as PP
import Prettyprinter.Render.String (renderString)
import Data.List (intersperse)

prettyExpr :: Expr -> Doc ann
prettyExpr = \case
  Num n       -> pretty n
  Var x       -> pretty x
  Add a b     -> hsep $ punctuate "+" (map prettyTerm (collectAdd (Add a b)))
  Mul a b     -> hsep $ map prettyFactor (collectMul (Mul a b))
  Div a b     -> "\\" <> "frac" <> braces (prettyExpr a) <> braces (prettyExpr b)
  Pow a b     -> braces (prettyExpr a) <> "^" <> braces (prettyExpr b)
  Neg e       -> "-" <> prettyExpr e
  Func f a    -> pretty f <> parens (prettyExpr a)