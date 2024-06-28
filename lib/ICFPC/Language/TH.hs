module ICFPC.Language.TH where

import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.String
import ICFPC.Language
import Language.Haskell.TH
import Numeric.Natural


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
