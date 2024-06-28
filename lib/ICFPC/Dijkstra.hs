module ICFPC.Dijkstra where

import Data.Set qualified as S
import Data.List
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ

dijkstra
  :: (Monoid w, Ord w, Ord v)
  => v -> (v -> Bool) -> (v -> [(v, w, c)]) -> Maybe (w, [c])
dijkstra initV stop adj
  = go (M.singleton initV (mempty, [])) S.empty (PQ.singleton mempty initV)
  where
    go weights seen queue
      | Just (v, _) <- PQ.minView queue
      , stop v
      = Just (weights M.! v)
      | Just (v, queue') <- PQ.minView queue
      , v `S.member` seen
      = go weights seen queue'
      | Just (v, queue') <- PQ.minView queue
      , (w, clues) <- weights M.! v
      , (weights', queue'') <- foldl' (visit w clues) (weights, queue') (adj v)
      = go weights' (S.insert v seen) queue''
      | otherwise
      = Nothing
    visit w clues (weights, queue) (v', w', c') = case M.lookup v' weights of
      Just (w'', _) | w'' <= w <> w' -> (weights, queue)
      _ ->
        ( M.insert v' (w <> w', c':clues) weights
        , PQ.insert (w <> w') v' queue
        )
