module ICFPC.Language.Eval where

import Data.ByteString qualified as BS
import ICFPC.Language
import Numeric.Natural


subst :: Natural -> Expr -> Expr -> Expr
subst v t = go
  where
    go = \case
      e@EBool{} -> e
      e@EInt{} -> e
      e@EString{} -> e
      Unary op x -> Unary op (go x)
      Binary op x y -> Binary op (go x) (go y)
      If x y z -> If (go x) (go y) (go z)
      e@(Lambda v' b)
        | v == v' -> e
        | otherwise -> Lambda v' (go b)
      e@(Var v')
        | v == v' -> t
        | otherwise -> e

eval :: Expr -> Expr
eval = \case
  e@Lambda{} -> e
  e@EBool{} -> e
  e@EInt{} -> e
  e@EString{} -> e
  Binary Apply (eval -> Lambda var body) arg
    -> eval $ subst var arg body

  Unary Neg (eval -> EInt x) -> EInt $! negate x
  Unary Not (eval -> EBool x) -> EBool $! not x
  Unary Int2Str (eval -> EInt x)
    -> EString $! ICFPText $ icfpFromInt $ fromInteger x
  Unary Str2Int (eval -> EString (ICFPText x))
    -> EInt $! toInteger $ icfpToInt x

  Binary Add (eval -> EInt x) (eval -> EInt y) -> EInt $! x + y
  Binary Subtract (eval -> EInt x) (eval -> EInt y) -> EInt $! x - y
  Binary Multiply (eval -> EInt x) (eval -> EInt y) -> EInt $! x * y
  Binary Divide (eval -> EInt x) (eval -> EInt y) -> EInt $! x `quot` y
  Binary Modulo (eval -> EInt x) (eval -> EInt y) -> EInt $! x `rem` y
  Binary Less (eval -> EInt x) (eval -> EInt y) -> EBool $! x < y
  Binary Greater (eval -> EInt x) (eval -> EInt y) -> EBool $! x > y
  Binary Equal (eval -> x) (eval -> y)
    | EInt ix <- x, EInt iy <- y -> EBool $! ix == iy
    | EBool ix <- x, EBool iy <- y -> EBool $! ix == iy
    | EString ix <- x, EString iy <- y -> EBool $! ix == iy
  Binary And (eval -> EBool x) (eval -> EBool y) -> EBool $! x && y
  Binary Or (eval -> EBool x) (eval -> EBool y) -> EBool $! x || y
  Binary Concat (eval -> EString (ICFPText x)) (eval -> EString (ICFPText y))
    -> EString $! ICFPText (x <> y)
  Binary Take (eval -> EInt x) (eval -> EString (ICFPText y))
    -> EString $! ICFPText (BS.take (fromIntegral x) y)
  Binary Drop (eval -> EInt x) (eval -> EString (ICFPText y))
    -> EString $! ICFPText (BS.drop (fromIntegral x) y)

  If (eval -> b) x y
    | EBool True <- b -> eval x
    | EBool False <- b -> eval y

  e -> error $ "stuck term: " <> show e
