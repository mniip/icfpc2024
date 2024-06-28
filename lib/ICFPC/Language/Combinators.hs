{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.Language.Combinators where

import ICFPC.Language
import Data.ByteString qualified as BS
import Data.String
import Numeric.Natural


instance Num Expr where
  fromInteger = EInt

instance IsString Expr where
  fromString = EString . fromString

class AsVar t where
  asVar :: Natural -> t

instance AsVar Natural where
  asVar = id

instance AsVar Expr where
  asVar = Var

pattern (:::) :: Expr -> Expr -> Expr
pattern f ::: x = Binary Apply f x
infixl 0 :::

pattern (:-) :: Expr -> Expr -> Expr
pattern x :- y = Binary Subtract x y
infixl 6 :-

pattern (:==) :: Expr -> Expr -> Expr
pattern x :== y = Binary Equal x y
infix 4 :==

pattern (:<>) :: Expr -> Expr -> Expr
pattern x :<> y = Binary Concat x y
infixr 6 :<>

pattern Let :: Natural -> Expr -> Expr -> Expr
pattern Let v t r = Binary Apply (Lambda v r) t

halfReplicate :: Expr
halfReplicate = Lambda self $ Lambda n $ Lambda x $
  If (n :== 0)
    ""
    (x :<> (self ::: self ::: (n :- 1) ::: x))
  where
    self, x, n :: AsVar t => t
    self = asVar 0
    n = asVar 1
    x = asVar 2

replicate' :: Expr
replicate' = Let repl halfReplicate (repl ::: repl)
  where
    repl :: AsVar t => t
    repl = asVar 0

times81 :: Expr
times81 = Lambda x $ Let triple (Lambda z $ z :<> z :<> z)
  (triple ::: (triple ::: (triple ::: (triple ::: x))))
  where
    x, triple, z :: AsVar t => t
    x = asVar 0
    triple = asVar 1
    z = asVar 2

codeSize :: Expr -> Int
codeSize = BS.length . encodeExpr
