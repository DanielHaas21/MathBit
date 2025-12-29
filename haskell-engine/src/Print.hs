{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Print (renderLatex) where

import Struct.Expr (Expr(..), Number(..))
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderStrict)
import Data.Text (Text)
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)

renderLatex :: Expr -> Text
renderLatex =
  renderStrict . layoutCompact . prettyLatex

prettyLatex :: Expr -> Doc ann
prettyLatex = prettyPrec PAdd 

prettyAddRhs :: Expr -> Doc ann
prettyAddRhs = \case
  Mul (Num n) x | numberLtZero n ->
    pretty "-" <+> prettyPrec PAdd (Mul (Num (negNum n)) x)

  Num n | numberLtZero n ->
    pretty "-" <+> prettyPrec PAtom (Num (absNum n))

  e -> pretty "+" <+> prettyPrec PAdd e

  
prettyMulRhs :: Expr -> Doc ann
prettyMulRhs e =
  case e of
    Add{} -> PP.space <> parens (prettyLatex e)
    _     -> PP.space <> prettyPrec PMul e

isIntegerNum :: Number -> Bool
isIntegerNum (R r) = denominator r == 1
isIntegerNum (D d) = d == fromInteger (round d)
roundNumber :: Number -> Integer
roundNumber (R r) = numerator r
roundNumber (D d) = round d

prettyPrec :: Prec -> Expr -> Doc ann
prettyPrec ctx = \case

  -- ========= ADDITION =========
  Add a b ->
    let doc =
          prettyPrec PAdd a
          <+> prettyAddRhs b
    in parensIf (ctx > PAdd) doc

  -- ========= MULTIPLICATION =========
  Mul a b ->
    let doc =
          prettyPrec PMul a
          <> prettyMulRhs b
    in parensIf (ctx > PMul) doc

  -- ========= DIVISION =========
  Div a b ->
    pretty "\\frac"
      <> braces (prettyPrec PAdd a)
      <> braces (prettyPrec PAdd b)

  -- ========= POWER =========
  Pow a b ->
    prettyPrec PPow a <> pretty "^" <> braces (prettyPrec PAdd b)


  -- ========= ATOMS =========
  Num n
    | isIntegerNum n -> pretty (roundNumber n)
    | otherwise      -> prettyNumber n

  Var v ->
    pretty v

  ConstantPi ->
    pretty "\\pi"

  ConstantE ->
    pretty "e"

  Func f x ->
    pretty "\\" <> pretty f <> braces (prettyLatex x)

  Sqrt x ->
    pretty  "\\sqrt" <> braces (prettyLatex x)

  Root n x ->
    pretty "\\sqrt[" <> prettyLatex n <> pretty "]" <> braces (prettyLatex x)

  e ->
    error ("Unhandled in pretty printer: " ++ show e)



data Prec = PAdd | PMul | PPow | PAtom
  deriving (Eq, Ord)
parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = parens
parensIf False = id

numberLtZero :: Number -> Bool
numberLtZero (R r) = r < 0
numberLtZero (D d) = d < 0

negNum :: Number -> Number
negNum (R r) = R (-r)
negNum (D d) = D (-d)

absNum :: Number -> Number
absNum (R r) = R (abs r)
absNum (D d) = D (abs d)

prettyNumber :: Number -> Doc ann
prettyNumber (R r)
  | denominator r == 1 = pretty (numerator r)
  | otherwise = pretty (fromRational r :: Double)
prettyNumber (D d) = pretty d