import Control.Applicative
import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Primitive
import Data.Traversable
import ICFPC.Spaceship
import ICFPC.Spaceship.Greedy
import ICFPC.Spaceship.Manoeuvre
import System.Environment
import System.Random
import System.Random.Stateful
import Text.Read (readMaybe)

import Debug.Trace


annealing
  :: Int
  -> Double
  -> StdGen
  -> PrimArray Int
  -> PrimArray Int
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> ST s Ordering
annealing plannedSteps plannedScore g0 xs ys mts mvxs mvys = do
  size <- getSizeofMutablePrimArray mts
  gen <- newSTGenM g0

  initScore <- sum <$> for [0 .. size - 1] \i -> readPrimArray mts i
  let
    temperature !step = max 0
      $ 1 - fromIntegral step / fromIntegral plannedSteps :: Double

    indexPos i = SpaceshipPos
      { x = indexPrimArray xs i
      , y = indexPrimArray ys i
      }

    readVel i = SpaceshipVel
      <$> readPrimArray mvxs i
      <*> readPrimArray mvys i

    randomTweak = do
      i <- uniformRM  (0, size - 1) gen
      prev <- if i == 0 then pure initState
        else do
          vel <- readVel (i - 1)
          pure SpaceshipState
            { pos = indexPos (i - 1)
            , vel
            }
      t <- readPrimArray mts i
      vel <- readVel i
      let
        pos = indexPos i
        tweaks = filter (\(t', vx', vy') -> (t', vx', vy') /= (t, vel.x, vel.y)
          && isManoeuvrePossible (prev.pos.x, prev.vel.x) (pos.x, vx') t'
          && isManoeuvrePossible (prev.pos.y, prev.vel.y) (pos.y, vy') t')
          $ liftA3 (,,)
            [max 0 (t - 2) .. t + 2] [vel.x - 2 .. vel.x + 2] [vel.y - 2 .. vel.y + 2]
      if null tweaks then randomTweak
      else do
        j <- uniformRM (0, length tweaks - 1) gen
        pure (i, pos, t, tweaks !! j)

    loop !step !score = do
      u01 <- uniformDouble01M gen

      (i, pos, t, (t', vx, vy)) <- randomTweak

      mT'' <- if i == size - 1 then pure Nothing
        else do
          let pos'' = indexPos (i + 1)
          vel'' <- readVel (i + 1)
          pure $ Just $ fastestPlan
            SpaceshipState
              { pos
              , vel = SpaceshipVel
                { x = vx
                , y = vy
                }
              }
            SpaceshipState
              { pos = pos''
              , vel = vel''
              }

      let thisT = t' - t
      nextT <- sum <$> for mT'' \t'' -> do
        (t'' -) <$> readPrimArray mts (i + 1)

      let
        scoreDiff = thisT + nextT
        prob = exp $ negate (fromIntegral scoreDiff)
          / (temperature step * plannedScore)

      score' <- if prob > u01
        then do
          writePrimArray mvxs i vx
          writePrimArray mvys i vy
          writePrimArray mts i t'
          for_ mT'' $ writePrimArray mts (i + 1)

          pure $ score + scoreDiff
        else pure score
      if scoreDiff < 0 || temperature step /= 0
      then loop (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score <> " (" <> show plannedScore <> ")"
        pure $ compare score initScore

  loop (0 :: Int) initScore

keepRestarting
  :: Int
  -> Double
  -> [Ordering]
  -> FilePath
  -> PrimArray Int
  -> PrimArray Int
  -> [(Int, SpaceshipVel)]
  -> IO ()
keepRestarting steps score last10 filename xs ys manoeuvres = do
  g <- newStdGen
  (ok, manoeuvres') <- stToIO do
    let
      ts = primArrayFromList $ fst <$> manoeuvres
      vxs = primArrayFromList $ (.x) . snd <$> manoeuvres
      vys = primArrayFromList $ (.y) . snd <$> manoeuvres
    mts <- unsafeThawPrimArray ts
    mvxs <- unsafeThawPrimArray vxs
    mvys <- unsafeThawPrimArray vys
    ok <- annealing steps score g xs ys mts mvxs mvys

    (ok,) <$> if ok /= GT
    then do
      size <- getSizeofMutablePrimArray mts
      ts' <- freezePrimArray mts 0 size
      vxs' <- freezePrimArray mvxs 0 size
      vys' <- freezePrimArray mvys 0 size
      pure $ zip (primArrayToList ts') $ zipWith SpaceshipVel
          (primArrayToList vxs') (primArrayToList vys')
    else pure manoeuvres
  BS.writeFile (filename <> ".ann") $ formatManoeuvres manoeuvres'
  let last10' = take 10 $ ok : last10
  if
    | LT <- ok -> keepRestarting steps score last10' filename xs ys manoeuvres'
    | all (== GT) last10' -> keepRestarting steps (score / 2) (replicate 10 LT) filename xs ys manoeuvres'
    | all (== EQ) last10' -> pure ()
    | otherwise -> keepRestarting steps score last10' filename xs ys manoeuvres'

main :: IO ()
main = getArgs >>= \case
  [readMaybe -> Just steps, readMaybe -> Just score, prob, mans] -> do
    input <- parseInput <$> BS.readFile prob
    manoeuvres <- parseManoeuvres <$> BS.readFile mans
    let
      xs = primArrayFromList $ (.x) <$> input.points
      ys = primArrayFromList $ (.y) <$> input.points
    keepRestarting steps score (replicate 10 LT) mans xs ys manoeuvres
  _ -> error "Usage: anneal <steps> <score> <problem> <manoeuvres>"
