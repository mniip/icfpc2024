{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.Language.TH where

import Data.ByteString.Char8 qualified as BS8
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.String
import ICFPC.Language
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric.Natural
import GHC.Exts.Heap
import GHC.Num
import System.IO.Unsafe
import Unsafe.Coerce


deriving stock instance Lift ICFPText
deriving stock instance Lift UnaryOp
deriving stock instance Lift BinaryOp
deriving stock instance Lift Expr

fromHaskell :: Exp -> Expr
fromHaskell ex = snd (go ex) mempty
  where
    go :: Exp -> (Set Name, Map Name Natural -> Expr)
    go = \case
      ConE con
        | con == 'True -> (mempty, \_ -> EBool True)
        | con == 'False -> (mempty, \_ -> EBool False)
      LitE (IntegerL int)
        | int >= 0 -> (mempty, \_ -> EInt int)
      LitE (StringL str) -> (mempty, \_ -> EString $ fromString str)
      VarE v -> (S.singleton v, \names -> case M.lookup v names of
        Just n -> Var n
        Nothing -> error $ "Not in scope: " <> show v)
      InfixE (Just x) (VarE f) (Just y)
        | Just op <- binary f
        -> case (go x, go y) of
          ((sx, kx), (sy, ky)) -> (sx <> sy, \names
            -> Binary op (kx names) (ky names))
      InfixE (Just x) (VarE f) Nothing
        | Just op <- unary f
        -> case go x of
          (sx, kx) -> (sx, \names -> Unary op (kx names))
      AppE (AppE (VarE f) x) y
        | Just op <- binary f
        -> case (go x, go y) of
          ((sx, kx), (sy, ky)) -> (sx <> sy, \names
            -> Binary op (kx names) (ky names))
      AppE (VarE f) x
        | Just op <- unary f
        -> case go x of
          (sx, kx) -> (sx, \names -> Unary op (kx names))
      AppE f x -> case (go f, go x) of
        ((sf, kf), (sx, kx)) -> (sf <> sx, \names
          -> Binary Apply (kf names) (kx names))
      ParensE x -> go x
      CondE b x y -> case (go b, go x, go y) of
        ((sb, kb), (sx, kx), (sy, ky)) -> (sb <> sx <> sy, \names
          -> If (kb names) (kx names) (ky names))
      LamE pats b -> goLam pats b
      LetE binds b -> goLet binds b
      e -> error $ "Unsupported Exp: " <> show e
      where
        unary f
          | f == 'negate = Just Neg
          | f == 'not = Just Not
          | f == 'read = Just Str2Int
          | f == 'show = Just Int2Str
          | otherwise = Nothing
        binary f
          | f == '(+) = Just Add
          | f == '(-) = Just Subtract
          | f == '(*) = Just Multiply
          | f == 'quot = Just Divide
          | f == 'rem = Just Modulo
          | f == '(<) = Just Less
          | f == '(>) = Just Greater
          | f == '(==) = Just Equal
          | f == '(||) = Just Or
          | f == '(&&) = Just And
          | f == '(<>) = Just Concat
          | f == 'take = Just Take
          | f == 'drop = Just Drop
          | f == '($) = Just Apply
          | otherwise = Nothing

    goLam [] b = go b
    goLam ((VarP name):ps) b = case goLam ps b of
      (sb, kb) -> (sb,) \names -> let
          used = S.fromList $ M.elems $ M.restrictKeys names sb
          unused = fromJust $ find (`S.notMember` used) [0..]
        in Lambda unused $ kb (M.insert name unused names)
    goLam (p:_) _ = error $ "Unsupported Pat: " <> show p

    goLet [] b = go b
    goLet (FunD name [Clause ps (NormalB t) ds']:ds) b
      = case (goLet ds' (LamE ps t), goLet ds b) of
        ((st, kt), (sb, kb)) -> (st <> sb,) \names -> let
            used = S.fromList $ M.elems $ M.restrictKeys names sb
            unused = fromJust $ find (`S.notMember` used) [0..]
          in Binary Apply
            (Lambda unused $ kb (M.insert name unused names))
            (kt names)
    goLet (ValD (VarP name) (NormalB t) ds':ds) b
      = case (goLet ds' t, goLet ds b) of
        ((st, kt), (sb, kb)) -> (st <> sb,) \names -> let
            used = S.fromList $ M.elems $ M.restrictKeys names sb
            unused = fromJust $ find (`S.notMember` used) [0..]
          in Binary Apply
            (Lambda unused $ kb (M.insert name unused names))
            (kt names)
    goLet (d:_) _ = error $ "Unsupported Dec: " <> show d

class PolymorphicEq a b where
  polymorphicEq :: a -> b -> Bool

instance {-# OVERLAPPABLE #-} PolymorphicEq a b where
  polymorphicEq :: a -> b -> Bool
  polymorphicEq !x !y = unsafePerformIO do
    getBoxedClosureData (asBox x) >>= \case
      ConstrClosure{name = "IS"}
        -> pure $! integerEq (unsafeCoerce x) (unsafeCoerce y)
      ConstrClosure{name} -> error name
      _ -> error "polymorphicEq"

instance {-# INCOHERENT #-} PolymorphicEq Integer Integer where
  {-# INLINE polymorphicEq #-}
  polymorphicEq = integerEq

instance {-# INCOHERENT #-} PolymorphicEq Integer b where
  {-# INLINE polymorphicEq #-}
  polymorphicEq x y = integerEq x (unsafeCoerce y)

instance {-# INCOHERENT #-} PolymorphicEq a Integer where
  {-# INLINE polymorphicEq #-}
  polymorphicEq x y = integerEq (unsafeCoerce x) y

toHaskell :: Expr -> Q Exp
toHaskell = \case
  EBool False -> conE 'False
  EBool True -> conE 'True
  EInt x -> sigE (litE $ integerL x) (conT ''Integer)
  EString x -> litE $ stringL $ BS8.unpack $ icfpToByteString x
  Unary Not x -> appE (varE 'not) (toHaskell x)
  Unary Neg x -> appE (varE 'negate) (toHaskell x)
  Unary Int2Str x -> appE (varE 'icfpFromInt) (toHaskell x)
  Unary Str2Int x -> appE (varE 'icfpToInt) (toHaskell x)
  Binary Add x y
    -> infixE (Just $ toHaskell x) (varE 'integerAdd) (Just $ toHaskell y)
  Binary Subtract x y
    -> infixE (Just $ toHaskell x) (varE 'integerSub) (Just $ toHaskell y)
  Binary Multiply x y
    -> infixE (Just $ toHaskell x) (varE 'integerMul) (Just $ toHaskell y)
  Binary Divide x y
    -> infixE (Just $ toHaskell x) (varE 'quot) (Just $ toHaskell y)
  Binary Modulo x y
    -> infixE (Just $ toHaskell x) (varE 'integerRem) (Just $ toHaskell y)
  Binary Less x y
    -> infixE (Just $ toHaskell x) (varE 'integerLt) (Just $ toHaskell y)
  Binary Greater x y
    -> infixE (Just $ toHaskell x) (varE 'integerGt) (Just $ toHaskell y)
  Binary Equal x y
    -> infixE (Just $ toHaskell x) (varE 'polymorphicEq) (Just $ toHaskell y)
  Binary Or x y
    -> infixE (Just $ toHaskell x) (varE '(||)) (Just $ toHaskell y)
  Binary And x y
    -> infixE (Just $ toHaskell x) (varE '(&&)) (Just $ toHaskell y)
  Binary Concat x y
    -> infixE (Just $ toHaskell x) (varE 'BS8.append) (Just $ toHaskell y)
  Binary Take x y
    -> varE 'BS8.take `appE` toHaskell x `appE` toHaskell y
  Binary Drop x y
    -> varE 'BS8.drop `appE` toHaskell x `appE` toHaskell y
  If x y z
    -> condE (toHaskell x) (toHaskell y) (toHaskell z)
  Binary Apply x y
    -> appE (toHaskell x) (toHaskell y)
  Lambda n x -> lamE [varP $ mkName $ "x" <> show n] (toHaskell x)
  Var n -> varE 'unsafeCoerce `appE` varE (mkName $ "x" <> show n)

baseDecoder :: String -> Q Exp
baseDecoder dictionary = [|\self n -> if n == 0
    then ""
    else
      let
        q = n `quot` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
        r = n `rem` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
      in take 1 (drop r $(pure $ LitE $ StringL dictionary)) <> self self q
  |]

baseDecoderDouble :: String -> Q Exp
baseDecoderDouble dictionary = [|\self n -> if n == 0
    then ""
    else
      let
        q = n `quot` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
        r = (n `rem` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)) * 2
      in take 2 (drop r $(pure $ LitE $ StringL $ dictionary <* [(),()])) <> self self q
  |]

encodeBase :: String -> String -> Integer
encodeBase dictionary = go 0 1
  where
    go !acc !_ [] = acc
    go acc p (x:xs) = go
      (acc + p * toInteger (fromJust $ elemIndex x dictionary)) (p * b) xs
    !b = toInteger $ length dictionary

encodeBaseDouble :: String -> String -> Integer
encodeBaseDouble dictionary = go
  where
    go [] = 0
    go (x:x':xs)
      | x == x'
      = toInteger (fromJust $ elemIndex x dictionary)
        + toInteger (length dictionary) * go xs
    go xs = error $ take 2 xs
