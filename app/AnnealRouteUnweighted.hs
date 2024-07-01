import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Foldable
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
  -> Double
  -> StdGen
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> ST s Bool
annealing plannedSteps plannedScore g0 mxs mys = do
  size <- getSizeofMutablePrimArray mxs
  gen <- newSTGenM g0
  let
    scoreForOrigin i j = do
      let
        x1 = 0
        y1 = 0
      x2 <- readPrimArray mxs i
      y2 <- readPrimArray mys i
      x3 <- readPrimArray mxs j
      y3 <- readPrimArray mys j
      pure $ max (abs $ x1 + x3 - 2 * x2) (abs $ y1 + y3 - 2 * y2)

    scoreForIndexTriple i j k = do
      x1 <- readPrimArray mxs i
      y1 <- readPrimArray mys i
      x2 <- readPrimArray mxs j
      y2 <- readPrimArray mys j
      x3 <- readPrimArray mxs k
      y3 <- readPrimArray mys k
      pure $ max (abs $ x1 + x3 - 2 * x2) (abs $ y1 + y3 - 2 * y2)

    scoreForEnd i j = do
      x1 <- readPrimArray mxs i
      y1 <- readPrimArray mys i
      x2 <- readPrimArray mxs j
      y2 <- readPrimArray mys j
      pure $ max (abs $ x2 - x1) (abs $ y2 - y1)

    scoreForPseudoIndexTriple i j k
      | i < 0 = scoreForOrigin j k
      | k == size = scoreForEnd i j
      | otherwise = scoreForIndexTriple i j k

  weights <- newPrimArray size
  initScore <- sum <$> for [0 .. size - 1] \i -> do
    triple <- scoreForPseudoIndexTriple (i - 1) i (i + 1)
    writePrimArray weights i triple
    pure triple

  let
    temperature !step = max 0
      $ 1 - fromIntegral step / fromIntegral plannedSteps :: Double

    randomPair = do
      i <- uniformRM (0, size - 2) gen
      j <- uniformRM (i + 1, size - 1) gen
      pure (i, j)

    loop !step !score = do
      u01 <- uniformDouble01M gen

      (i, j) <- randomPair

      oldI <- readPrimArray weights i
      oldIP <- if i == 0 then pure Nothing
        else Just <$> readPrimArray weights (i - 1)
      oldJ <- readPrimArray weights j
      oldJN <- if j == size - 1 then pure Nothing
        else Just <$> readPrimArray weights (j + 1)

      newI <- scoreForPseudoIndexTriple (i + 1) i (j + 1)
      newIP <- for oldIP \_ -> scoreForPseudoIndexTriple (i - 2) (i - 1) j
      newJ <- scoreForPseudoIndexTriple (i - 1) j (j - 1)
      newJN <- for oldJN \_ -> scoreForPseudoIndexTriple i (j + 1) (j + 2)

      let
        scoreDiff = newI + sum newIP + newJ + sum newJN - oldI - sum oldIP - oldJ - sum oldJN
        prob = exp $ negate (fromIntegral scoreDiff)
          / (temperature step * plannedScore)

      score' <- if prob > u01
        then do
          reverseRange mxs i j
          reverseRange mys i j

          writePrimArray weights i newI
          writePrimArray weights j newJ
          for_ newIP $ writePrimArray weights (i - 1)
          for_ newJN $ writePrimArray weights (j + 1)

          reverseRange weights i j

          pure $ score + scoreDiff
        else pure score
      if scoreDiff < 0 || temperature step /= 0
      then loop (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score <> " (" <> show plannedScore <> ")"
        pure $ score <= initScore

  loop (0 :: Int) initScore

keepRestarting :: Int -> Double -> Int -> FilePath -> Input -> IO ()
keepRestarting steps score !failCount filename input = do
  g <- newStdGen
  (ok, input') <- stToIO do
    let
      xs = primArrayFromList $ (.x) <$> input.points
      ys = primArrayFromList $ (.y) <$> input.points
    mxs <- unsafeThawPrimArray xs
    mys <- unsafeThawPrimArray ys
    ok <- annealing steps score g mxs mys

    (ok,) <$> if ok
    then do
      size <- getSizeofMutablePrimArray mxs
      xs' <- freezePrimArray mxs 0 size
      ys' <- freezePrimArray mys 0 size
      pure Input
        { points = zipWith SpaceshipPos
          (primArrayToList xs') (primArrayToList ys')
        }
    else pure input
  BS.writeFile (filename <> ".annu") $ formatInput input'
  if
    | ok -> keepRestarting steps score 0 filename input'
    | not ok, failCount > 10 -> keepRestarting steps (score / 2) 0 filename input'
    | otherwise -> keepRestarting steps score (failCount + 1) filename input'

main :: IO ()
main = getArgs >>= \case
  [readMaybe -> Just steps, readMaybe -> Just score, filename] -> do
    input <- parseInput <$> BS.readFile filename
    keepRestarting steps score 0 filename input

  _ -> error "Usage: anneal <steps> <score> <problem>"
