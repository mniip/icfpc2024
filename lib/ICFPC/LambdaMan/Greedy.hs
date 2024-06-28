module ICFPC.LambdaMan.Greedy where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as S
import ICFPC.Dijkstra


data Input = Input
  { size :: (Int, Int)
  , start :: (Int, Int)
  , walls :: Set (Int, Int)
  } deriving stock (Eq, Ord, Show)

parseInput :: String -> Input
parseInput (lines -> xss) = Input
  { size = (length $ head xss, length xss)
  , start =
    ( fromJust $ elemIndex 'L' =<< find ('L' `elem`) xss
    , fromJust $ findIndex ('L' `elem`) xss
    )
  , walls = S.fromList
    [ (x, y)
    | (y, xs) <- zip [0..] xss
    , (x, c) <- zip [0..] xs
    , c == '#'
    ]
  }

data Direction = U | D | L | R
  deriving stock (Eq, Ord, Show)

advance :: Direction -> (Int, Int) -> (Int, Int)
advance = \case
  L -> first pred
  R -> first succ
  U -> second pred
  D -> second succ

greedySolution :: Input -> [Direction]
greedySolution inp = go initPills inp.start
  where
    (width, height) = inp.size
    initPills = S.fromList
      [ (x, y)
      | x <- [0..width-1]
      , y <- [0..height-1]
      , (x, y) `S.notMember` inp.walls
      , (x, y) /= inp.start
      ]
    inBounds (x, y) = x >= 0 && y >= 0 && x < width && y < height

    go pills pos
      | S.null pills = []
      | otherwise = case dijkstra pos (`S.member` pills) adj of
        Nothing -> error "urk"
        Just (_, path)
          | !pos' <- foldl' (flip advance) pos path
          -> reverse path ++ go (S.delete pos' pills) pos'

    adj pos
      = [ (pos', Sum (1 :: Int), d)
        | d <- [U, D, L, R]
        , let pos' = advance d pos
        , inBounds pos'
        , pos' `S.notMember` inp.walls
        ]
