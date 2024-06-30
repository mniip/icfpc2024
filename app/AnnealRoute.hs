import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.List
import Data.Primitive
import ICFPC.Spaceship
import System.Environment
import System.Random
import System.Random.Stateful
import Text.Read (readMaybe)

import Debug.Trace


reverseRange :: MutablePrimArray s Int -> Int -> Int -> ST s ()
reverseRange a !i !j
  | i >= j = pure ()
  | otherwise = do
    x <- readPrimArray a i
    y <- readPrimArray a j
    writePrimArray a i y
    writePrimArray a j x
    reverseRange a (i + 1) (j - 1)

annealing
  :: Int
  -> StdGen
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> ST s ()
annealing plannedSteps g0 mxs mys = do
  gen <- newSTGenM g0
  size <- getSizeofMutablePrimArray mxs
  let
    scoreForOrigin i = do
      x <- readPrimArray mxs i
      y <- readPrimArray mys i
      pure $ max (abs x) (abs y)

    scoreForIndexPair i j = do
      x1 <- readPrimArray mxs i
      y1 <- readPrimArray mys i
      x2 <- readPrimArray mxs j
      y2 <- readPrimArray mys j
      pure $ max (abs $ x2 - x1) (abs $ y2 - y1)

    scoreAll = foldl'
      (\ !tot !i -> liftA2 (+) tot $ scoreForIndexPair i (i + 1))
      (scoreForOrigin 0)
      [0 .. size - 2]

  initScore <- scoreAll
  let
    temperature !step = max 0
      $ 1 - fromIntegral step / fromIntegral plannedSteps :: Double

    go !step !score = do
      u01 <- uniformDouble01M gen
      i <- uniformRM (0, size - 2) gen
      j <- uniformRM (i + 1, size - 1) gen

      delIedge <- if i == 0
        then negate <$> scoreForOrigin i
        else negate <$> scoreForIndexPair (i - 1) i
      delJedge <- if j == size - 1
        then pure 0
        else negate <$> scoreForIndexPair j (j + 1)
      addIedge <- if j == size - 1
        then pure 0
        else scoreForIndexPair i (j + 1)
      addJedge <- if i == 0
        then scoreForOrigin j
        else scoreForIndexPair (i - 1) j
      let
        scoreDiff = delIedge + delJedge + addIedge + addJedge
        prob = exp $ negate (fromIntegral scoreDiff)
          / (temperature step * fromIntegral initScore)

      score' <- if prob > u01
        then do
          reverseRange mxs i j
          reverseRange mys i j
          pure $ score + scoreDiff
        else pure score
      if scoreDiff < 0 || temperature step /= 0
      then go (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score

  go (0 :: Int) initScore

keepRestarting :: Int -> FilePath -> StdGen -> Input -> IO ()
keepRestarting steps filename g input = do
  input' <- stToIO do
    let
      xs = primArrayFromList $ (.x) <$> input.points
      ys = primArrayFromList $ (.y) <$> input.points
    mxs <- unsafeThawPrimArray xs
    mys <- unsafeThawPrimArray ys
    annealing steps g mxs mys

    size <- getSizeofMutablePrimArray mxs
    xs' <- freezePrimArray mxs 0 size
    ys' <- freezePrimArray mys 0 size
    pure Input
      { points = zipWith SpaceshipPos
        (primArrayToList xs') (primArrayToList ys')
      }
  BS.writeFile (filename <> ".ann") $ formatInput input'
  keepRestarting steps filename g input'

main :: IO ()
main = getArgs >>= \case
  [readMaybe -> Just steps, filename] -> do
    input <- parseInput <$> BS.readFile filename
    g <- getStdGen
    keepRestarting steps filename g input

  _ -> error "Usage: anneal <steps> <problem>"
