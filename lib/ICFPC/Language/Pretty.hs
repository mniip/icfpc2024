module ICFPC.Language.Pretty where

import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import ICFPC.Language
import Numeric
import Numeric.Natural


type Prec = Int

data IntBase = Dec | Hex | Nonary

data PrettySettings = PrettySettings
  { intBase :: IntBase
  , decodeLet :: Bool
  , chainLambda :: Bool
  , chainApp :: Bool
  , chainLet :: Bool
  , chainAnd :: Bool
  , chainOr :: Bool
  , varName :: Natural -> ShowS
  , and :: Prec -> NonEmpty (Prec -> ShowS) -> ShowS
  , or :: Prec -> NonEmpty (Prec -> ShowS) -> ShowS
  , not :: Prec -> (Prec -> ShowS) -> ShowS
  }

prettyExpr :: PrettySettings -> Expr -> String
prettyExpr settings = ($ "") . go 0
  where
    go prec = \case
      Binary Apply (Lambda v t) x
        | settings.decodeLet
        , settings.chainLet
        -> chainLet (NE.singleton (v, x)) t
        | settings.decodeLet
        -> formatLet (NE.singleton (v, x)) t

      Binary Apply f x
        | settings.chainApp
        -> chainApp prec (NE.singleton x) f
        | otherwise
        -> formatApp prec f (NE.singleton x)

      Lambda v x
        | settings.chainLambda
        -> chainLambda prec (NE.singleton v) x
        | otherwise
        -> formatLambda prec (NE.singleton v) x

      Binary And x y
        | settings.chainAnd
        -> formatAnd prec (collectAnd x <> collectAnd y)
        | otherwise
        -> formatAnd prec (x :| [y])

      Binary Or x y
        | settings.chainOr
        -> formatOr prec (collectOr x <> collectOr y)
        | otherwise
        -> formatOr prec (x :| [y])

      Unary Not x -> settings.not prec (flip go x)

      Unary Neg x -> showParen (prec > 6)
        $ showString "-" . go 7 x
      Unary Int2Str x -> showParen (prec > 10)
        $ showString "icfpFromInt " . go 11 x
      Unary Str2Int x -> showParen (prec > 10)
        $ showString "icfpToInt " . go 11 x
      Binary Add x y -> showParen (prec > 6)
        $ go 6 x . showString " + " . go 7 y
      Binary Subtract x y -> showParen (prec > 6)
        $ go 6 x . showString " - " . go 7 y
      Binary Multiply x y -> showParen (prec > 7)
        $ go 7 x . showString " * " . go 8 y
      Binary Divide x y -> showParen (prec > 7)
        $ go 7 x . showString " `quot` " . go 8 y
      Binary Modulo x y -> showParen (prec > 7)
        $ go 7 x . showString " `rem` " . go 8 y
      Binary Less x y -> showParen (prec > 4)
        $ go 5 x . showString " < " . go 5 y
      Binary Greater x y -> showParen (prec > 4)
        $ go 5 x . showString " > " . go 5 y
      Binary Equal x y -> showParen (prec > 6)
        $ go 6 x . showString " == " . go 7 y
      Binary Concat x y -> showParen (prec > 6)
        $ go 7 x . showString " <> " . go 6 y
      Binary Take x y -> showParen (prec > 10)
        $ showString "take " <> go 11 x . showString " " . go 11 y
      Binary Drop x y -> showParen (prec > 10)
        $ showString "drop " <> go 11 x . showString " " . go 11 y

      If x y z -> showParen (prec > 0)
        $ showString "if " . go 0 x
        . showString " then " . go 0 y
        . showString " else " . go 0 z

      Var n -> settings.varName n

      EBool b -> shows b
      EInt n -> formatInt prec n settings.intBase
      EString s -> shows s


    chainLet bindings = \case
      Binary Apply (Lambda v t) x
        -> chainLet ((v, x) NE.<| bindings) t
      x -> formatLet (NE.reverse bindings) x

    formatLet ((v0, t0) :| binds) x = showString "let "
      . settings.varName v0 . showString " = " . go 0 t0
      . appEndo (flip foldMap binds \(v, t) -> Endo $
        showString "; " . settings.varName v . showString " = " . go 0 t)
      . showString " in " . go 0 x

    chainApp prec args = \case
      Binary Apply f x -> chainApp prec (x NE.<| args) f
      f -> formatApp prec f args

    formatApp prec f args = showParen (prec > 10) $ go 11 f
      . appEndo (flip foldMap args \x -> Endo $ showString " " . go 11 x)

    chainLambda prec vars = \case
      Lambda v x -> chainLambda prec (v NE.<| vars) x
      x -> formatLambda prec vars x

    formatLambda prec (v0 :| vars) x = showParen (prec > 0) $ showString "\\"
      . settings.varName v0
      . appEndo (flip foldMap vars \v
        -> Endo $ showString " " . settings.varName v)
      . showString " -> " . go 0 x

    collectAnd = \case
      Binary And x y -> collectAnd x <> collectAnd y
      x -> NE.singleton x

    formatAnd prec args = settings.and prec (flip go <$> args)

    collectOr = \case
      Binary Or x y -> collectOr x <> collectOr y
      x -> NE.singleton x

    formatOr prec args = settings.or prec (flip go <$> args)

    formatInt prec n base = case compare n 0 of
      LT -> showParen (prec > 6) $ goPos (abs n) base
      EQ -> showString "0"
      GT -> goPos n base
      where
        goPos m = \case
          Dec -> shows m
          Hex -> showString "0x" . showHex m
          Nonary -> showString "0n" . goNonary m
        goNonary 0 = id
        goNonary k = case divMod k 9 of
          (d, m) -> goNonary d . (intToDigit (fromInteger m) :)

haskell :: PrettySettings
haskell = PrettySettings
  { intBase = Dec
  , decodeLet = True
  , chainLambda = True
  , chainApp = True
  , chainLet = True
  , chainAnd = True
  , chainOr = True
  , varName = \n -> showString "x" . shows n
  , and = \prec (a0 :| args) -> showParen (prec > 3)
    $ a0 4 . appEndo (flip foldMap args \a -> Endo $ showString " && " . a 3)
  , or = \prec (a0 :| args) -> showParen (prec > 2)
    $ a0 3 . appEndo (flip foldMap args \a -> Endo $ showString " || " . a 2)
  , not = \prec x -> showParen (prec > 10) $ showString "not " . x 11
  }

z3 :: PrettySettings
z3 = haskell
  { varName = \n -> showString "x[" . shows n . showString "]"
  , and = \_ (a0 :| args) -> showString "And(" . a0 0
    . appEndo (flip foldMap args \a -> Endo $ showString "," . a 0)
  , or = \_ (a0 :| args) -> showString "Or(" . a0 0
    . appEndo (flip foldMap args \a -> Endo $ showString "," . a 0)
  , not = \_ x -> showString "Not(" . x 0 . showString ")"
  }
