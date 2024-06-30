module ICFPC.Spaceship.Greedy where

import Data.Functor
import Data.List
import Data.Ord
import ICFPC.Spaceship
import ICFPC.Spaceship.Manoeuvre


fastestPlan :: SpaceshipState -> SpaceshipState -> T
fastestPlan s tgt = case
    ( manoeuvreTimes (s.pos.x, s.vel.x) (tgt.pos.x, tgt.vel.x)
    , manoeuvreTimes (s.pos.y, s.vel.y) (tgt.pos.y, tgt.vel.y) )
  of
    ((Just (xt1, xt2), _), (Just (yt1, yt2), _))
      | max xt1 yt1 <= min xt2 yt2 -> max xt1 yt1
    ((_, xt3), (Just (yt1, yt2), _))
      | xt3 <= yt2 -> max xt3 yt1
    ((Just (xt1, xt2), _), (_, yt3))
      | yt3 <= xt2 -> max xt1 yt3
    ((_, xt3), (_, yt3)) -> max xt3 yt3


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
                        rest = filter (/= closest) ps
                    in closest : go closest rest

greedyManoeuvreOrder :: Input -> Input
greedyManoeuvreOrder = Input . go initState . points
  where
    go _ [] = []
    go s ps = let
        (t, p) = minimum $ ps <&> \q -> (fastestPlanAnySpeed s q, q)
        asx = buildPossibleManoeuvreAnySpeed (s.pos.x, s.vel.x) p.x t
        asy = buildPossibleManoeuvreAnySpeed (s.pos.y, s.vel.y) p.y t
        !v = SpaceshipVel
          { x = s.vel.x + sum asx
          , y = s.vel.y + sum asy
          }
      in p : go SpaceshipState { pos = p, vel = v } (delete p ps)


-- visit in order, reach each greedily
dumbSolution :: Input -> [(T, SpaceshipVel)]
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
      in (t, v) : go (SpaceshipState p v) ps
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
        p = minimumBy (comparing $ fastestPlanAnySpeed st) points
      in go st p (delete p points)

fillManoeuvre :: Input -> [(T, SpaceshipVel)] -> [SpaceshipCommand]
fillManoeuvre input mans = zip3 (initState:states) states (fst <$> mans)
  >>= \(!s, !tgt, !t) -> let
      asx = buildPossibleManoeuvre (s.pos.x, s.vel.x) (tgt.pos.x, tgt.vel.x) t
      asy = buildPossibleManoeuvre (s.pos.y, s.vel.y) (tgt.pos.y, tgt.vel.y) t
    in zipWith SpaceshipCommand asx asy
  where
    states = zipWith SpaceshipState input.points (snd <$> mans)
