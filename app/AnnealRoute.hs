import Control.Monad
import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List
import Data.Primitive
import Data.Traversable
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
  -> Int
  -> StdGen
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> ST s ()
annealing plannedSteps plannedScore g0 mxs mys = do
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

  -- for each vertex, sum of scores of its two incident edges
  weights <- newPrimArray size
  initScore <- sum <$> for [0 .. size - 1] \i -> do
    edgeL <- if i == 0
      then scoreForOrigin i
      else scoreForIndexPair (i - 1) i
    edgeR <- if i == size - 1
      then pure 0
      else scoreForIndexPair i (i + 1)
    writePrimArray weights i (edgeL + edgeR)
    pure edgeL

  let
    temperature !step = max 0
      $ 1 - fromIntegral step / fromIntegral plannedSteps :: Double

    randomPair = do
      totWeight <- sum <$> for [0 .. size - 1] do readPrimArray weights
      iW <- uniformRM (0, totWeight - 1) gen
      jW <- uniformRM (0, totWeight - 1) gen
      i <- bucket 0 iW
      j <- bucket 0 jW
      case compare i j of
        LT -> pure (i, j)
        EQ -> randomPair
        GT -> pure (j, i)
      where
        bucket !i !s = readPrimArray weights i >>= \w -> if s < w then pure i
          else bucket (i + 1) (s - w)

    loop !step !score = do
      u01 <- uniformDouble01M gen

      (i, j) <- randomPair

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
          / (temperature step * fromIntegral plannedScore)

      score' <- if prob > u01
        then do
          reverseRange mxs i j
          reverseRange mys i j

          readPrimArray weights i >>= writePrimArray weights i
            . (+ (addIedge + delIedge))
          readPrimArray weights j >>= writePrimArray weights j
            . (+ (addJedge + delJedge))
          unless (i == 0) do
            readPrimArray weights (i - 1) >>= writePrimArray weights (i - 1)
              . (+ (addJedge + delIedge))
          unless (j == size - 1) do
            readPrimArray weights (j + 1) >>= writePrimArray weights (j + 1)
              . (+ (addIedge + delJedge))

          reverseRange weights i j

          pure $ score + scoreDiff
        else pure score
      if scoreDiff < 0 || temperature step /= 0
      then loop (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score

  loop (0 :: Int) initScore

keepRestarting :: Int -> Int -> FilePath -> StdGen -> Input -> IO ()
keepRestarting steps score filename g input = do
  input' <- stToIO do
    let
      xs = primArrayFromList $ (.x) <$> input.points
      ys = primArrayFromList $ (.y) <$> input.points
    mxs <- unsafeThawPrimArray xs
    mys <- unsafeThawPrimArray ys
    annealing steps score g mxs mys

    size <- getSizeofMutablePrimArray mxs
    xs' <- freezePrimArray mxs 0 size
    ys' <- freezePrimArray mys 0 size
    pure Input
      { points = zipWith SpaceshipPos
        (primArrayToList xs') (primArrayToList ys')
      }
  BS.writeFile (filename <> ".ann") $ formatInput input'
  keepRestarting steps score filename g input'

main :: IO ()
main = getArgs >>= \case
  [readMaybe -> Just steps, readMaybe -> Just score, filename] -> do
    input <- parseInput <$> BS.readFile filename
    g <- getStdGen
    keepRestarting steps score filename g input

  _ -> error "Usage: anneal <steps> <score> <problem>"
