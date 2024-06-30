module ICFPC.LambdaMan where

import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable
import Data.Hashable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Word
import GHC.Generics


data Input = Input
  { size :: (Int, Int)
  , walls :: Array (PrimArray Word8)
  , start :: (Int, Int)
  } deriving stock (Eq, Ord, Show)

parseInput :: ByteString -> Input
parseInput inp = case BS8.lines $ BS8.strip inp of
  ls@(line0 : _) -> let
      width = BS8.length line0
      height = length ls
    in case find (BS8.elem 'L' . snd) $ zip [0..] ls of
        Nothing -> error "No L"
        Just (y, l) -> case BS8.elemIndex 'L' l of
          Nothing -> error "No L"
          Just x -> Input
            { size = (width, height)
            , walls = arrayFromListN height
              $ primArrayFromListN width . map toWall . BS8.unpack <$> ls
            , start = (x, y)
            }
  _ -> error "No lines"
  where
    toWall '#' = 1
    toWall _ = 0

data Direction = U | D | L | R
  deriving stock (Eq, Ord, Show)

showDirection :: Direction -> Char
showDirection = \case
  L -> 'L'
  R -> 'R'
  D -> 'D'
  U -> 'U'

readDirection :: Char -> Direction
readDirection = \case
  'L' -> L
  'R' -> R
  'D' -> D
  'U' -> U
  c -> error $ "readDirection " <> show c

advance :: Direction -> (Int, Int) -> (Int, Int)
advance = \case
  L -> first pred
  R -> first succ
  U -> second pred
  D -> second succ

data LambdaManState = LambdaManState
  { posX :: !Int
  , posY :: !Int
  , pills :: !(IntMap IntSet)
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable)

initialState :: Input -> LambdaManState
initialState !input = LambdaManState
  { posX
  , posY
  , pills = IM.fromList $ filter (not . IS.null . snd)
    [ (y, IS.fromList r)
    | (y, row) <- zip [0..] $ toList input.walls
    , let
        r =
          [ x
          | (x, b) <- zip [0..] $ primArrayToList row
          , b == 0
          , (x, y) /= input.start
          ]
    ]
  } where (posX, posY) = input.start

validDestination :: Input -> (Int, Int) -> Bool
validDestination input (!x, !y) = x >= 0 && y >= 0 && x < width && y < height
  && indexPrimArray (indexArray input.walls y) x == 0
  where (!width, !height) = input.size

isPill :: LambdaManState -> (Int, Int) -> Bool
isPill state (!x, !y) = any (x `IS.member`) $ IM.lookup y state.pills

simulateStep
  :: Input -> LambdaManState -> Direction -> LambdaManState
simulateStep !input !st !d
  | (!x, !y) <- advance d (st.posX, st.posY)
  , validDestination input (x, y)
  = LambdaManState
    { posX = x
    , posY = y
    , pills = IM.update
      (mfilter (not . IS.null) . Just . IS.delete x) y st.pills
    }
  | otherwise = st
{-# INLINE simulateStep #-}

checkSolution :: Input -> [Direction] -> LambdaManState
checkSolution input = foldl' (simulateStep input) (initialState input)

formatState :: Input -> LambdaManState -> [ByteString]
formatState input state =
  [ BSL.toStrict $ B.toLazyByteString $ mconcat
    [ B.char8 if
        | (x, y) == (state.posX, state.posY) -> 'L'
        | not $ validDestination input (x, y) -> '#'
        | isPill state (x, y) -> '.'
        | otherwise -> ' '
    | x <- [0 .. width - 1]
    ]
  | y <- [0 .. height - 1]
  ]
  where (width, height) = input.size
