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
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> MutablePrimArray s Int
  -> ST s Ordering
annealing plannedSteps plannedScore g0 mxs mys mts mvxs mvys = do
  size <- getSizeofMutablePrimArray mts
  gen <- newSTGenM g0

  initScore <- sum <$> for [0 .. size - 1] \i -> readPrimArray mts i
  let
    temperature !step = max 0
      $ 1 - fromIntegral step / fromIntegral plannedSteps :: Double

    readPos i = SpaceshipPos
      <$> readPrimArray mxs i
      <*> readPrimArray mys i

    readVel i = SpaceshipVel
      <$> readPrimArray mvxs i
      <*> readPrimArray mvys i

    randomTweak = do
      i <- uniformRM  (0, size - 1) gen
      prev <- if i == 0 then pure initState
        else do
          pos <- readPos (i - 1)
          vel <- readVel (i - 1)
          pure SpaceshipState
            { pos
            , vel
            }
      t <- readPrimArray mts i
      pos <- readPos i
      vel <- readVel i
      let
        tweaks = filter (\(t', vx', vy') -> (t', vx', vy') /= (t, vel.x, vel.y)
          && isManoeuvrePossible (prev.pos.x, prev.vel.x) (pos.x, vx') t'
          && isManoeuvrePossible (prev.pos.y, prev.vel.y) (pos.y, vy') t')
          $ liftA3 (,,)
            [max 0 (t - 2) .. t + 2] [vel.x - 2 .. vel.x + 2] [vel.y - 2 .. vel.y + 2]
      if null tweaks then randomTweak
      else do
        j <- uniformRM (0, length tweaks - 1) gen
        pure (i, pos, t, tweaks !! j)

    loop1 !step !score = do
      u01 <- uniformDouble01M gen

      (i, pos, t, (t', vx, vy)) <- randomTweak

      mT'' <- if i == size - 1 then pure Nothing
        else do
          pos'' <- readPos (i + 1)
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
      then loop2 (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score <> " (" <> show plannedScore <> ")"
        pure $ compare score initScore

    randomPair = do
      i <- uniformRM (0, size - 2) gen
      j <- uniformRM (i + 1, size - 1) gen
      pure (i, j)

    planScore i j = do
      pos <- readPos i
      pos' <- readPos j
      vel <- readVel i
      vel' <- readVel j
      pure $ fastestPlan
        SpaceshipState { pos, vel }
        SpaceshipState { pos = pos', vel = vel' }

    originPlanScore i = do
      pos <- readPos i
      vel <- readVel i
      pure $ fastestPlan initState SpaceshipState { pos, vel }

    loop2 !step !score = do
      u01 <- uniformDouble01M gen

      (i, j) <- randomPair

      delIedge <- if i == 0
        then originPlanScore i
        else planScore (i - 1) i
      delJedge <- if j == size - 1 then pure 0
        else planScore j (j + 1)
      addIedge <- if j == size - 1 then pure 0
        else do
          pos <- readPos i
          pos' <- readPos (j + 1)
          vel <- readVel i
          vel' <- readVel (j + 1)
          pure $ fastestPlan
            SpaceshipState { pos, vel }
            SpaceshipState { pos = pos', vel = SpaceshipVel (-vel'.x) (-vel'.y) }
      addJedge <- if i == 0
        then do
          pos <- readPos j
          vel <- readVel j
          pure $ fastestPlan initState SpaceshipState { pos, vel = SpaceshipVel (-vel.x) (-vel.y) }
        else do
          pos <- readPos (i - 1)
          pos' <- readPos j
          vel <- readVel (i - 1)
          vel' <- readVel j
          pure $ fastestPlan
            SpaceshipState { pos, vel = SpaceshipVel (-vel.x) (-vel.y) }
            SpaceshipState { pos = pos', vel = vel' }

      let
        scoreDiff = addIedge + addJedge - delIedge - delJedge
        prob = exp $ negate (fromIntegral scoreDiff)
          / (temperature step * plannedScore)

      score' <- if prob > u01
        then do
          reverseRange mxs i j
          reverseRange mys i j
          reverseRange mvxs i j
          reverseRange mvys i j

          for_ [i..j] \k -> do
            t <- if k == 0 then originPlanScore k else planScore (k - 1) k
            writePrimArray mts k t


          sum <$> for [0 .. size - 1] (readPrimArray mts)
        else pure score
      if scoreDiff < 0 || temperature step /= 0
      then loop1 (step + 1) score'
      else do
        traceM $ show initScore <> " -> " <> show score <> " (" <> show plannedScore <> ")"
        pure $ compare score initScore

  loop1 (0 :: Int) initScore



keepRestarting
  :: Int
  -> Double
  -> [Ordering]
  -> FilePath
  -> FilePath
  -> Input
  -> [(Int, SpaceshipVel)]
  -> IO ()
keepRestarting steps score last10 filename mans input manoeuvres = do
  g <- newStdGen
  (ok, (input', manoeuvres')) <- stToIO do
    let
      xs = primArrayFromList $ (.x) <$> input.points
      ys = primArrayFromList $ (.y) <$> input.points
      ts = primArrayFromList $ fst <$> manoeuvres
      vxs = primArrayFromList $ (.x) . snd <$> manoeuvres
      vys = primArrayFromList $ (.y) . snd <$> manoeuvres
    mxs <- unsafeThawPrimArray xs
    mys <- unsafeThawPrimArray ys
    mts <- unsafeThawPrimArray ts
    mvxs <- unsafeThawPrimArray vxs
    mvys <- unsafeThawPrimArray vys
    ok <- annealing steps score g mxs mys mts mvxs mvys

    (ok,) <$> if ok /= GT
    then do
      size <- getSizeofMutablePrimArray mts
      xs' <- freezePrimArray mxs 0 size
      ys' <- freezePrimArray mys 0 size
      ts' <- freezePrimArray mts 0 size
      vxs' <- freezePrimArray mvxs 0 size
      vys' <- freezePrimArray mvys 0 size
      pure
        ( Input
          { points = zipWith SpaceshipPos
            (primArrayToList xs') (primArrayToList ys')
          }
        , zip (primArrayToList ts') $ zipWith SpaceshipVel
          (primArrayToList vxs') (primArrayToList vys')
        )
    else pure (input, manoeuvres)
  BS.writeFile (filename <> ".anb") $ formatInput input'
  BS.writeFile (mans <> ".anb") $ formatManoeuvres manoeuvres'
  let last10' = take 10 $ ok : last10
  if
    | LT <- ok -> keepRestarting steps score last10' filename mans input' manoeuvres'
    | all (== GT) last10' -> keepRestarting steps (score / 2) (replicate 10 LT) filename mans input' manoeuvres'
    | all (== EQ) last10' -> pure ()
    | otherwise -> keepRestarting steps score last10' filename mans input' manoeuvres'

main :: IO ()
main = getArgs >>= \case
  [readMaybe -> Just steps, readMaybe -> Just score, filename, mans] -> do
    input <- parseInput <$> BS.readFile filename
    manoeuvres <- parseManoeuvres <$> BS.readFile mans

    keepRestarting steps score (replicate 10 LT) filename mans input manoeuvres

  _ -> error "Usage: anneal <steps> <score> <problem> <manoeuvres>"
