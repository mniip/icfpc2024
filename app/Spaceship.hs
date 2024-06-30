import qualified Data.Array as A
import Data.List
import Debug.Trace
import System.Random

import ICFPC.Spaceship
import ICFPC.Spaceship.Greedy

intRoot :: Int -> Int
intRoot 0 = 0
intRoot 1 = 1
intRoot d = root 1
    where root i | d >= i*i && (i+1)*(i+1) > d = i
                 | otherwise = root (i+1)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (a, b) (c, d) = max (foo $ abs (a-c)) (foo $ abs (b-d))
    where foo i = let n = intRoot i
                  in 2*n + i - n*n

type Trip = A.Array Int (Int, Int)

score :: Trip -> Int
score arr = sum [dist (arr A.! (i-1)) (arr A.! i) | i <- [low+1..up]]
    where (low, up) = A.bounds arr

-- Generate a random pair of distinct integers (up to a bound)
randomPair :: Int -> StdGen -> ((Int, Int), StdGen)
randomPair b g = let (x, g') = uniformR (1, b) g
                     (y, g'') = uniformR (1, b) g'
                 in if x == y then randomPair b g'' else ((x, y), g'')

-- Simulated annealing
annealing :: StdGen -> [(Int, Int)] -> [(Int, Int)]
annealing gen points = A.elems $ go 1.0 gen cost0 sol0
    where len = length points
          sol0 = A.listArray (0, len) $ sort ((0,0):points)
          cost0 = traceShow ("Initial: " ++ show (score sol0)) (score sol0)
          iter :: Int -> Float -> StdGen -> Int -> Trip -> (Trip, StdGen)
          iter 0 _ g _ sol = (sol, g)
          iter n temp g cost sol = let ((i, j), g') = randomPair len g
                                       (u01, g'') = uniformR (0, 1) g'
                                       sol' = sol A.// [(i, sol A.! j), (j, sol A.! i)]
                                       diff s k = if k < len then dist (s A.! (k-1)) (s A.! k) + dist (s A.! (k+1)) (s A.! k) else dist (s A.! (k-1)) (s A.! k)
                                       cost' = cost - (diff sol i) - (diff sol j) + (diff sol' i) + (diff sol' j) +
                                           (if i+1 == j || i == j+1 then dist (sol A.! i) (sol A.! j) - dist (sol' A.! i) (sol' A.! j) else 0) -- Special case when i and j are adjacent
                                   in if cost' < cost then iter (n+1) temp g' cost' sol'
                                      else if exp ((fromIntegral $ cost - cost') / ((fromIntegral cost0)*temp)) > u01 then iter (n-1) temp g'' cost' sol' else iter (n-1) temp g'' cost sol
          go :: Float -> StdGen -> Int -> Trip -> Trip
          go temp g cost sol = let (sol', g') = iter 1000 temp g cost sol
                                   cost' = traceShowId $ score sol'
                               in if cost' /= cost then go (0.9*temp) g' cost' sol' else traceShow cost sol


main :: IO ()
main = do
    file <- getContents
    gen <- initStdGen
    let goals = map ((\[a, b] -> (read a, read b)) . words) $ lines file
        goals' = annealing gen goals
        -- solution = solve goals'
        solution = dumbSolution (Input $ map (\(x, y) -> SpaceshipPos x y) goals')
    -- traceShow (length solution) (putStrLn solution)
    putStrLn $ map formatNumpad solution
