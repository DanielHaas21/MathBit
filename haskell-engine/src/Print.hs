{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Print (renderLatex) where

import Struct.Expr (Expr(..), Number(..))
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderStrict)
import Data.Text (Text)
import Data.List (intersperse)
import Data.Ratio (numerator, denominator)
import Helpers.Numbers (absNum, numberLtZero, negNum,)

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
    let (negA, a') = extractNeg a
        doc = prettyPrec PMul a' <> prettyMulRhs b
    in if negA then pretty "-" <> doc else doc



  Neg e@(Var _)  -> pretty "-" <> prettyPrec PAtom e
  Neg e@(Num _)  -> pretty "-" <> prettyPrec PAtom e
  Neg e          -> pretty "-" <> parens (prettyPrec PAdd e)
  
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


prettyNumber :: Number -> Doc ann
prettyNumber (R r)
  | denominator r == 1 =
      pretty (numerator r)
  | otherwise =
      pretty "\\frac"
        <> braces (pretty (numerator r))
        <> braces (pretty (denominator r))
prettyNumber (D d) =
  pretty d

extractNeg :: Expr -> (Bool, Expr)
extractNeg (Num n) | numberLtZero n = (True, Num (negNum n))
extractNeg (Div (Num n) d) | numberLtZero n = (True, Div (Num (negNum n)) d)
extractNeg (Mul (Num n) rest) | numberLtZero n = (True, Mul (Num (negNum n)) rest)
extractNeg e = (False, e)