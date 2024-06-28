module ICFPC.LambdaMan.BreadthFirst where

import Data.IntMap qualified as IM
import Data.Monoid
import ICFPC.Dijkstra
import ICFPC.LambdaMan


breadthFirstSolution :: Input -> [Direction]
breadthFirstSolution input
  = case dijkstraHashable (initialState input) isSolved adjacent of
    Just (_, path) -> reverse path
    Nothing -> error "No solution"
  where
    isSolved state = IM.null state.pills
    adjacent state =
      [ (state', Sum (1 :: Int), d)
      | d <- [U, D, L, R]
      , let pos' = advance d (state.posX, state.posY)
      , validDestination input pos'
      , let state' = simulateStep input state d
      ]
