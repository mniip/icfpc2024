module ICFPC.Spaceship where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Functor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List
import Data.Maybe
import Data.Ord

import Debug.Trace

newtype Input = Input
  { points :: [SpaceshipPos]
  } deriving stock (Eq, Ord, Show)

data SpaceshipPos = SpaceshipPos
  { x :: !Int
  , y :: !Int
  } deriving stock (Eq, Ord, Show)

data SpaceshipVel = SpaceshipVel
  { x :: !Int
  , y :: !Int
  } deriving stock (Eq, Ord, Show)

data SpaceshipState = SpaceshipState
  { pos :: SpaceshipPos
  , vel :: SpaceshipVel
  } deriving stock (Eq, Ord, Show)

initState :: SpaceshipState
initState = SpaceshipState
  { pos = SpaceshipPos
    { x = 0
    , y = 0
    }
  , vel = SpaceshipVel
    { x = 0
    , y = 0
    }
  }

data SpaceshipCommand = SpaceshipCommand
  { accelX :: !AX
  , accelY :: !AY
  } deriving stock (Eq, Ord, Show)

parseInput :: ByteString -> Input
parseInput = either error id . Attoparsec.parseOnly do
  points <- Attoparsec.many' do
    x <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 ' '
    y <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 '\n'
    pure SpaceshipPos{x, y}
  Attoparsec.endOfInput
  pure Input{..}

formatNumpad :: SpaceshipCommand -> Char
formatNumpad = \case
  SpaceshipCommand (-1) (-1) -> '1'
  SpaceshipCommand 0 (-1) -> '2'
  SpaceshipCommand 1 (-1) -> '3'
  SpaceshipCommand (-1) 0 -> '4'
  SpaceshipCommand 0 0 -> '5'
  SpaceshipCommand 1 0 -> '6'
  SpaceshipCommand (-1) 1 -> '7'
  SpaceshipCommand 0 1 -> '8'
  SpaceshipCommand 1 1 -> '9'
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

type X = Int
type Y = Int
type VX = Int
type VY = Int
type AX = Int
type AY = Int
type T = Int

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

plans :: SpaceshipState -> SpaceshipPos -> [(T, Range, Range)]
plans s tgt =
  [ (t, shiftRange s.vel.x dvxs, shiftRange s.vel.y dvys)
  | (t, dyn) <- zip [0..] velByTimeAndPos
  , dvxs <- maybeToList $ IM.lookup (tgt.x - s.pos.x - t * s.vel.x) dyn
  , dvys <- maybeToList $ IM.lookup (tgt.y - s.pos.y - t * s.vel.y) dyn
  ]

reachGreedily :: SpaceshipState -> SpaceshipPos -> (T, SpaceshipVel)
reachGreedily p q = case plans p q of
  (t, Range minvx _, Range minvy _) : _ -> (t, SpaceshipVel minvx minvy)
  _ -> error "no plans"

-- visit in order, reach each greedily
dumbSolution :: Input -> [SpaceshipCommand]
dumbSolution input = go initState input.points
  where
    go !s (!p:ps) = let
        (!t, !v) = reachGreedily s p
        asx = backtrack t (s.pos.x, s.vel.x) (p.x, v.x)
        asy = backtrack t (s.pos.y, s.vel.y) (p.y, v.y)
      in zipWith SpaceshipCommand asx asy ++ go (SpaceshipState p v) ps
    go _ [] = []

-- go for closest according to heuristic
greedySolution :: Input -> [SpaceshipCommand]
greedySolution input = pick initState input.points
  where
    go _ q _ | trace ("Picked " <> show q) False = undefined
    go !s !p ps = let
        (!t, !v) = reachGreedily s p
        asx = backtrack t (s.pos.x, s.vel.x) (p.x, v.x)
        asy = backtrack t (s.pos.y, s.vel.y) (p.y, v.y)
      in zipWith SpaceshipCommand asx asy ++ pick (SpaceshipState p v) ps

    pick _ [] = []
    pick st points = let
        p = minimumBy (comparing $ heuristic st) points
      in go st p (delete p points)

    heuristic s p = mergeWith max
      (mergeWith min
        (timeEstimate s.pos.x s.vel.x p.x 1)
        (timeEstimate s.pos.x s.vel.x p.x (-1)))
      (mergeWith min
        (timeEstimate s.pos.y s.vel.y p.y 1)
        (timeEstimate s.pos.y s.vel.y p.y (-1)))

    mergeWith f (Just x) (Just y) = Just (f x y)
    mergeWith _ p q = p <|> q

    timeEstimate x0 v0 x1 a = twicePositiveRoot
      (a / 2)
      (fromIntegral v0 + a / 2)
      (fromIntegral (x0 - x1))

    twicePositiveRoot a b c
      | d >= 0
      , p >= 0 = Just p
      | d >= 0
      , q >= 0 = Just q
      | otherwise = Nothing
      where
        d :: Double
        d = b * b - 4 * a * c
        p = -b - sqrt d
        q = -b + sqrt d
