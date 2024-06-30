module ICFPC.Spaceship.Manoeuvre where

import Data.Bits
import ICFPC.Spaceship
import Math.NumberTheory.Logarithms


isManoeuvrePossible :: (X, VX) -> (X, VX) -> T -> Bool
isManoeuvrePossible (x0, vx0) (x1, vx1) t =
  t*t + 2*t*dv - dv*dv + 4*vx0*t + 2*dv - 4*dx >= 0 &&
  t*t - 2*t*dv - dv*dv - 4*vx0*t - 2*dv + 4*dx >= 0
  where
    !dx = x1 - x0
    !dv = vx1 - vx0

-- [t1, t2] U [t3, +inf]
manoeuvreTimes :: (X, VX) -> (X, VX) -> (Maybe (T, T), T)
manoeuvreTimes (x0, vx0) (x1, vx1) = case
    ( integerQuadraticInequality 1 ( 2*dv + 4*vx0) (-dv*dv + 2*dv - 4*dx)
    , integerQuadraticInequality 1 (-2*dv - 4*vx0) (-dv*dv - 2*dv + 4*dx) )
  of
    (NotFromTo negT1 t1, NotFromTo t2 t3)
      | negT1 < 0, t1 <= t2 -> (Just (t1, t2), t3)
      | negT1 < 0, t1 > t2 -> (Nothing, max 0 $ max t1 t3)
    (NotFromTo negT1 t3, Always)
      | negT1 < 0 -> (Nothing, max 0 t3)
    (NotFromTo t2 t3, NotFromTo negT1 t1)
      | negT1 < 0, t1 <= t2 -> (Just (t1, t2), t3)
      | negT1 < 0, t1 > t2 -> (Nothing, max 0 $ max t1 t3)
    (Always, NotFromTo negT1 t3)
      | negT1 < 0 -> (Nothing, max 0 t3)
    (Always, Always) -> (Nothing, 0)
    p -> error $ "manoeuvreTimes: " <> show p
  where
    !dx = x1 - x0
    !dv = vx1 - vx0

isManoeuvrePossibleAnySpeed :: (X, VX) -> X -> T -> Bool
isManoeuvrePossibleAnySpeed (x0, vx0) x1 t =
  t*t + t + 2*vx0*t - 2*dx >= 0 &&
  t*t + t - 2*vx0*t + 2*dx >= 0
  where
    !dx = x1 - x0

-- [t1, t2] U [t3, +inf]
manoeuvreTimesAnySpeed :: (X, VX) -> X -> (Maybe (T, T), T)
manoeuvreTimesAnySpeed (x0, vx0) x1 = case
    ( integerQuadraticInequality 1 (1 + 2*vx0) (-2*dx)
    , integerQuadraticInequality 1 (1 - 2*vx0) ( 2*dx)
    )
  of
    (NotFromTo negT1 t1, NotFromTo t2 t3)
      | negT1 < 0, t1 <= t2 -> (Just (t1, t2), t3)
      | negT1 < 0, t1 > t2 -> (Nothing, max 0 $ max t1 t3)
    (NotFromTo negT1 t3, Always)
      | negT1 < 0 -> (Nothing, max 0 t3)
    (NotFromTo t2 t3, NotFromTo negT1 t1)
      | negT1 < 0, t1 <= t2 -> (Just (t1, t2), t3)
      | negT1 < 0, t1 > t2 -> (Nothing, max 0 $ max t1 t3)
    (Always, NotFromTo negT1 t3)
      | negT1 < 0 -> (Nothing, max 0 t3)
    (Always, Always) -> (Nothing, 0)
    p -> error $ "manoeuvreTimes: " <> show p
  where
    !dx = x1 - x0

buildPossibleManoeuvre :: (X, VX) -> (X, VX) -> T -> [AX]
buildPossibleManoeuvre (!x0, !vx0) !q !t
  | t == 0 = if q == (x0, vx0) then []
    else error "Impossible manoeuver"
  | p' <- (x0 + vx0 + 1, vx0 + 1)
  , isManoeuvrePossible p' q (t - 1)
  = 1 : buildPossibleManoeuvre p' q (t - 1)
  | p' <- (x0 + vx0 - 1, vx0 - 1)
  , isManoeuvrePossible p' q (t - 1)
  = (-1) : buildPossibleManoeuvre p' q (t - 1)
  | otherwise
  = 0 : buildPossibleManoeuvre (x0 + vx0, vx0) q (t - 1)


buildPossibleManoeuvreAnySpeed :: (X, VX) -> X -> T -> [AX]
buildPossibleManoeuvreAnySpeed (!x0, !vx0) !q !t
  | t == 0 = if x0 == q then []
    else error "Impossible manoeuvre"
  | p' <- (x0 + vx0 + 1, vx0 + 1)
  , isManoeuvrePossibleAnySpeed p' q (t - 1)
  = 1 : buildPossibleManoeuvreAnySpeed p' q (t - 1)
  | p' <- (x0 + vx0 - 1, vx0 - 1)
  , isManoeuvrePossibleAnySpeed p' q (t - 1)
  = (-1) : buildPossibleManoeuvreAnySpeed p' q (t - 1)
  | otherwise
  = 0 : buildPossibleManoeuvreAnySpeed (x0 + vx0, vx0) q (t - 1)


data WeakIQISolution
  = FromTo !Int !Int -- [x, y]
  | NotFromTo !Int !Int -- (-inf, x] U [y, +inf)
  | From !Int -- [x, +inf)
  | UpTo !Int -- (-inf, x]
  | Always -- (-inf, +inf)
  | Never -- \varnothing
  deriving stock (Eq, Ord, Show)

satisfiesSolution :: Int -> WeakIQISolution -> Bool
satisfiesSolution x = \case
  FromTo a b -> a <= x && x <= b
  NotFromTo a b -> x <= a || b <= x
  From a -> a <= x
  UpTo a -> x <= a
  Always -> True
  Never -> False

-- | Find conditions on x such that ax^2 + bx + c >= 0
integerQuadraticInequality :: Int -> Int -> Int -> WeakIQISolution
integerQuadraticInequality a b c = case compare a 0 of
  -- actually linear
  EQ -> case compare b 0 of
    -- actually constant
    EQ -> case compare c 0 of
      LT -> Never
      EQ -> Always
      GT -> Always
    LT -> UpTo ((-c) `div` b)
    GT -> From (negate $ c `div` b)
  -- downwards parabola
  LT -> case compare d 0 of
    LT -> Never
    EQ -> case divMod (-b) (2 * a) of
      (q, 0) -> FromTo q q
      _ -> Never
    GT -> let
        -- equivalent when a < 0
        satisfies x = (2 * a * x + b) ^ (2 :: Int) <= d
      in case (satisfies apexFloor, satisfies apexCeil) of
        (True, True) -> FromTo
          (binSearchFwd
            ((-b + sqrtDUpperBound) `div` (2 * a))
            apexFloor
            satisfies)
          (binSearchBck
            apexCeil
            (negate $ (b + sqrtDUpperBound) `div` (2 * a))
            satisfies)
        (True, False) -> FromTo apexFloor apexFloor
        (False, True) -> FromTo apexCeil apexCeil
        (False, False) -> Never
  -- upwards parabola
  GT -> case compare d 0 of
    LT -> Always
    EQ -> Always
    GT -> let
        -- equivalent when a > 0
        satisfies x = (2 * a * x + b) ^ (2 :: Int) >= d
      in case (satisfies apexFloor, satisfies apexCeil) of
        (False, False) -> NotFromTo
          (binSearchBck
            ((-b - sqrtDUpperBound) `div` (2 * a))
            apexFloor
            satisfies)
          (binSearchFwd
            apexCeil
            (negate $ (b - sqrtDUpperBound) `div` (2 * a))
            satisfies)
        (True, False) -> NotFromTo (apexCeil - 1) (apexCeil + 1)
        (False, True) -> NotFromTo (apexFloor - 1) (apexFloor + 1)
        (True, True) -> Always
  where
    d = b * b - 4 * a * c
    sqrtDUpperBound = 1 `shiftL` ((intLog2 d `div` 2) + 1)
    apexFloor = (-b) `div` (2 * a)
    apexCeil = negate (b `div` (2 * a))

    binSearchFwd l r p -- precondition: l <= r, p r
      | not (p r) = error "urk1"
      | l > r = error "urk2"
      | p l = l
      | p m = binSearchFwd l m p
      | otherwise = binSearchFwd (m + 1) r p
      where m = (l + r) `div` 2
    binSearchBck l r p -- precondition: l <= r, p l
      | not (p l) = error "urk3"
      | l > r = error "urk4"
      | p r = r
      | p m = binSearchBck m r p
      | otherwise = binSearchBck l (m - 1) p
      where m = (l + r + 1) `div` 2
