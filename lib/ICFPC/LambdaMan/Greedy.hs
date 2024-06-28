module ICFPC.LambdaMan.Greedy where

import Control.Monad
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Monoid
import ICFPC.Dijkstra
import ICFPC.LambdaMan


greedySolution :: Input -> [Direction]
greedySolution input = go (initialState input)
  where
    go st
      | IM.null st.pills = []
      | otherwise = case dijkstra (st.posX, st.posY) (isPill st) adjacent of
        Nothing -> error $ "No path to any pill: " <> show st
        Just (_, path)
          | (!posX, !posY) <- foldl' (flip advance) (st.posX, st.posY) path
          -> reverse path ++ go LambdaManState
            { posX
            , posY
            , pills = IM.update
              (mfilter (not . IS.null) . Just . IS.delete posX) posY st.pills
            }
      where
        adjacent pos =
          [ (pos', Sum (1 :: Int), d)
          | d <- [U, D, L, R]
          , let pos' = advance d pos
          , validDestination input pos'
          ]
