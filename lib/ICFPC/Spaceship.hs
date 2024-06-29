module ICFPC.Spaceship where

import Data.Bifunctor
import Data.Functor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe


type X = Int
type Y = Int
type VX = Int
type VY = Int
type AX = Int
type AY = Int
type T = Int

data Input = Input
  { points :: [(X, Y)]
  }

parseInput :: String -> Input
parseInput = Input . map (go . words) . lines
  where
    go [x, y] = (read x, read y)
    go _ = error "urk"

formatNumpad :: (AX, AY) -> Char
formatNumpad = \case
  (-1, -1) -> '1'
  (0, -1) -> '2'
  (1, -1) -> '3'
  (-1, 0) -> '4'
  (0, 0) -> '5'
  (1, 0) -> '6'
  (-1, 1) -> '7'
  (0, 1) -> '8'
  (1, 1) -> '9'
  p -> error $ "formatNumpad: " <> show p

data Range = Range !Int !Int -- incl
  deriving stock (Eq, Ord, Show)

posByTimeAndVel :: [IntMap Range] -- memoized
posByTimeAndVel = iterate next (IM.singleton 0 $ Range 0 0)
  where
    next m = IM.mapWithKey shiftRange
      $ IM.unionWith unionRanges
        (IM.fromAscList . map (first pred) . IM.toAscList $ m)
        $ IM.unionWith unionRanges
          m
          (IM.fromAscList . map (first succ) . IM.toAscList $ m)

    unionRanges (Range min1 max1) (Range min2 max2)
      = Range (min min1 min2) (max max1 max2)

shiftRange :: Int -> Range -> Range
shiftRange off (Range rmin rmax) = Range (off + rmin) (off + rmax)

inRange :: Int -> Range -> Bool
inRange !x (Range rmin rmax) = x >= rmin && x <= rmax

velByTimeAndPos :: [IntMap Range] -- memoized
velByTimeAndPos = posByTimeAndVel <&> go
  where
    go m = IM.fromList
      [ (pos, Range minVel maxVel)
      | pos <- [-maxPos..maxPos]
      , (minVel, _) <- take 1
        $ filter (\(_, r) -> inRange pos r)
        $ IM.toAscList m
      , (maxVel, _) <- take 1
        $ filter (\(_, r) -> inRange pos r)
        $ IM.toDescList m
      ]
      where maxPos = maximum $ IM.elems m <&> \(Range _ rmax) -> rmax

backtrack :: T -> (X, VX) -> (X, VX) -> [AX]
backtrack !t (!x0, !vx0) (!x1, !vx1)
  = go (reverse (take t posByTimeAndVel)) (x1 - x0 - t * vx0) (vx1 - vx0) []
  where
    go (m:ms) !dx !dvx as
      | any ((dx - dvx) `inRange`) $ IM.lookup (dvx - 1) m
      = go ms (dx - dvx) (dvx - 1) (1:as)
      | any ((dx - dvx) `inRange`) $ IM.lookup (dvx + 1) m
      = go ms (dx - dvx) (dvx + 1) (-1:as)
      | otherwise
      = go ms (dx - dvx) dvx (0:as)
    go [] 0 0 as = as
    go _ _ _ _ = error "backtrack"

plans :: (X, Y, VX, VY) -> (X, Y) -> [(T, Range, Range)]
plans (!x0, !y0, !vx0, !vy0) (!x1, !y1) =
  [ (t, shiftRange vx0 dvxs, shiftRange vy0 dvys)
  | (t, dyn) <- zip [0..] velByTimeAndPos
  , dvxs <- maybeToList $ IM.lookup (x1 - x0 - t * vx0) dyn
  , dvys <- maybeToList $ IM.lookup (y1 - y0 - t * vy0) dyn
  ]

reachGreedily :: (X, Y, VX, VY) -> (X, Y) -> (T, VX, VY)
reachGreedily p q = case plans p q of
  (t, Range minvx _, Range minvy _) : _ -> (t, minvx, minvy)
  _ -> error "no plans"

-- visit in order, reach each greedily
dumbSolution :: Input -> [(AX, AY)]
dumbSolution input = go (0, 0, 0, 0) input.points
  where
    go (!x0, !y0, !vx0, !vy0) ((!x1, !y1):ps) = let
        (!t, !vx1, !vy1) = reachGreedily (x0, y0, vx0, vy0) (x1, y1)
        asx = backtrack t (x0, vx0) (x1, vx1)
        asy = backtrack t (y0, vy0) (y1, vy1)
      in zip asx asy ++ go (x1, y1, vx1, vy1) ps
    go _ [] = []

