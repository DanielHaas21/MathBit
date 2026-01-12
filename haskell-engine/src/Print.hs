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
prettyAddRhs e = pretty "+" <+> prettyPrec PAdd e

  
prettyMulRhs :: Expr -> Doc ann
prettyMulRhs e =
  case e of
    Add{} -> space <> parens (prettyLatex e)
    Sub{} -> space <> parens (prettyLatex e)
    Neg{} -> space <> parens (prettyLatex e)
    _     -> space <> prettyPrec PMul e

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
    case extractNeg b of
      (True, b') ->
        let doc =
              prettyPrec PAdd a
              <+> pretty "-"
              <+> prettyPrec PAdd b'
        in parensIf (ctx > PAdd) doc

      (False, _) ->
        let doc =
              prettyPrec PAdd a
              <+> pretty "+"
              <+> prettyPrec PAdd b
        in parensIf (ctx > PAdd) doc

  Sub a b ->
    let doc =
          prettyPrec PAdd a
          <+> pretty "-"
          <+> prettyPrec PAdd b
    in parensIf (ctx > PAdd) doc

  -- ========= MULTIPLICATION =========
  Mul a b ->
    let
      (negA, a') = extractNeg a

      left  = prettyPrec PMul a'
      right = prettyPrec PMul b

      mid
        | canImplicitMul a' b = mempty
        | otherwise          = space <> pretty "\\times " <> space

      doc = left <> mid <> right
    in
      (if negA then pretty "-" <> space else mempty) <> doc



  Neg e ->
    case e of
      Num _ -> pretty "-" <> prettyPrec PAtom e
      Var _ -> pretty "-" <> prettyPrec PAtom e
      _     -> pretty "-" <> parens (prettyPrec PAdd e)
  
  -- ========= DIVISION =========
  Div a b ->
    pretty "\\frac"
      <> braces (prettyPrec PAdd a)
      <> braces (prettyPrec PAdd b)

  -- ========= POWER =========
  Pow a b ->
    let base =
          case a of
            Var _ -> prettyPrec PPow a
            Num _ -> prettyPrec PPow a
            _     -> parens (prettyPrec PAdd a)
    in base <> pretty "^" <> braces (prettyPrec PAdd b)
  
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

  Abs s ->
    pretty "\\left|" <> prettyLatex s <> pretty "\\right|"

  Factorial f ->
    braces (prettyLatex f) <> pretty "!"

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
extractNeg (Neg e) = (True, e)
extractNeg (Num n) | numberLtZero n = (True, Num (negNum n))
extractNeg (Div (Num n) d) | numberLtZero n = (True, Div (Num (negNum n)) d)
extractNeg (Mul (Num n) rest) | numberLtZero n = (True, Mul (Num (negNum n)) rest)
extractNeg e = (False, e)

-- list of cases where showing mutliplication symbol is preffered and where not 
canImplicitMul :: Expr -> Expr -> Bool
canImplicitMul a b =
  case (a, b) of
    -- number * symbol-like
    (Num _, Var _)        -> True
    (Num _, Pow _ _)      -> True
    (Num _, Func _ _)     -> True
    (Num _, Sqrt _)       -> True
    (Num _, Root _ _)     -> True
    (Num _, Abs _)        -> True

    -- symbol * symbol-like
    (Var _, Pow _ _)      -> True
    (Var _, Func _ _)     -> True
    (Var _, Sqrt _)       -> True
    (Var _, Root _ _)     -> True
    (Var _, Abs _)        -> True
    (Var _, Var _)        -> False

    -- symbol * parentheses
    (Var _, Add{})        -> True
    (Var _, Sub{})        -> True

    -- number * parentheses
    (Num _, Add{})        -> True
    (Num _, Sub{})        -> True

    -- power * parentheses (x^2)(x+1) → needs dot
    (Pow _ _, Add{})      -> False
    (Pow _ _, Sub{})      -> False

    -- anything with numbers on the right
    (_, Num _)            -> False

    -- parentheses on the left
    (Add{}, _)            -> False
    (Sub{}, _)            -> False

    _                     -> False