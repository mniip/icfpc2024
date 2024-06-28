module ICFPC.Dijkstra where

import Data.Hashable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S

dijkstra
  :: (Monoid w, Ord w, Ord v)
  => v -> (v -> Bool) -> (v -> [(v, w, c)]) -> Maybe (w, [c])
dijkstra initV stop adj = go
  (M.singleton initV (mempty, []))
  (S.singleton initV)
  (PQ.singleton mempty initV)
  where
    go weights seen queue
      | Just (v, _) <- PQ.minView queue
      , stop v
      = Just (weights M.! v)
      | Just (v, queue') <- PQ.minView queue
      , (w, clues) <- weights M.! v
      , (weights', queue'', seen')
        <- foldl' (visit w clues) (weights, queue', seen) (adj v)
      = go weights' seen' queue''
      | otherwise
      = Nothing
    visit w clues (!weights, !queue, !seen) (v', w', c')
      = case M.lookup v' weights of
        Just (w'', _) | w'' <= w <> w' -> (weights, queue, seen)
        _
          | v' `S.member` seen -> (weights, queue, seen)
          | otherwise ->
            ( M.insert v' (w <> w', c':clues) weights
            , PQ.insert (w <> w') v' queue
            , S.insert v' seen
            )
{-# INLINE dijkstra #-}

dijkstraHashable
  :: (Monoid w, Ord w, Hashable v)
  => v -> (v -> Bool) -> (v -> [(v, w, c)]) -> Maybe (w, [c])
dijkstraHashable initV stop adj = go
  (HM.singleton initV (mempty, []))
  (HS.singleton initV)
  (PQ.singleton mempty initV)
  where
    go weights seen queue
      | Just (v, _) <- PQ.minView queue
      , stop v
      = Just (weights HM.! v)
      | Just (v, queue') <- PQ.minView queue
      , (w, clues) <- weights HM.! v
      , (weights', queue'', seen')
        <- foldl' (visit w clues) (weights, queue', seen) (adj v)
      = go weights' seen' queue''
      | otherwise
      = Nothing
    visit w clues (!weights, !queue, !seen) (v', w', c')
      = case HM.lookup v' weights of
        Just (w'', _) | w'' <= w <> w' -> (weights, queue, seen)
        _
          | v' `HS.member` seen -> (weights, queue, seen)
          | otherwise ->
            ( HM.insert v' (w <> w', c':clues) weights
            , PQ.insert (w <> w') v' queue
            , HS.insert v' seen
            )
{-# INLINE dijkstraHashable #-}
