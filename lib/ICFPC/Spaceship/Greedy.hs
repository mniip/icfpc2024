module ICFPC.Spaceship.Greedy where

import Control.Applicative
import Data.List
import Data.Ord
import ICFPC.Spaceship
import ICFPC.Spaceship.Manoeuvre

import Debug.Trace


fastestPlanAnySpeed :: SpaceshipState -> SpaceshipPos -> T
fastestPlanAnySpeed s tgt = case
    ( manoeuvreTimesAnySpeed (s.pos.x, s.vel.x) tgt.x
    , manoeuvreTimesAnySpeed (s.pos.y, s.vel.y) tgt.y )
  of
    ((Just (xt1, xt2), _), (Just (yt1, yt2), _))
      | max xt1 yt1 <= min xt2 yt2 -> max xt1 yt1
    ((_, xt3), (Just (yt1, yt2), _))
      | xt3 <= yt2 -> max xt3 yt1
    ((Just (xt1, xt2), _), (_, yt3))
      | yt3 <= xt2 -> max xt1 yt3
    ((_, xt3), (_, yt3)) -> max xt3 yt3

greedyOrder :: Input -> Input
greedyOrder = Input . go (SpaceshipPos 0 0) . points
    where dist (SpaceshipPos x y) (SpaceshipPos x' y') = max (abs $ x - x') (abs $ y - y')
          go _ [] = []
          go p ps = let closest = minimumBy (comparing $ dist p) ps
                        rest = traceShow (dist p closest) $ filter (/= closest) ps
                    in closest : go closest rest


-- visit in order, reach each greedily
dumbSolution :: Input -> [SpaceshipCommand]
dumbSolution input = go initState input.points
  where
    go !s (!p:ps) = let
        !t = fastestPlanAnySpeed s p
        asx = buildPossibleManoeuvreAnySpeed (s.pos.x, s.vel.x) p.x t
        asy = buildPossibleManoeuvreAnySpeed (s.pos.y, s.vel.y) p.y t
        !v = SpaceshipVel
          { x = s.vel.x + sum asx
          , y = s.vel.y + sum asy
          }
      in zipWith SpaceshipCommand asx asy ++ go (SpaceshipState p v) ps
    go _ [] = []

-- go for closest according to heuristic
greedySolution :: Input -> [SpaceshipCommand]
greedySolution input = pick initState input.points
  where
    go !s !p ps = let
        !t = fastestPlanAnySpeed s p
        asx = buildPossibleManoeuvreAnySpeed (s.pos.x, s.vel.x) p.x t
        asy = buildPossibleManoeuvreAnySpeed (s.pos.y, s.vel.y) p.y t
        !v = SpaceshipVel
          { x = s.vel.x + sum asx
          , y = s.vel.y + sum asy
          }
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