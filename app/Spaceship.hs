import Data.List (nub, sort, minimumBy)
import Data.Function (on)

-- Land with zero velocity on the target
fromTo :: Int -> Int -> [Int]
fromTo a b | a == b = []
           | a < b = [1] ++ (replicate (b-a-1) 0) ++ [-1]
           | a > b = [-1] ++ (replicate (a-b-1) 0) ++ [1]

-- Combine two trajectories
combine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
combine (x, y) (x', y') = if length mx < length my then zip (mx ++ cycle [0]) my else zip mx (my ++ cycle [0])
    where mx = fromTo x x'
          my = fromTo y y'

toNumpad :: (Int, Int) -> Char
toNumpad c = head . show . fst . head . filter (\(_, c') -> c == c') $ zip [1..] [(y, x) | x <- [-1..1], y <- [-1..1]]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (a, b) (c, d) = max (abs (a-c)) (abs (b-d))

solve :: [(Int, Int)] -> String
solve goals = go (0, 0) (filter (/= (0, 0)) goals)
    where go _ [] = []
          go coord gs = let g = minimumBy (compare `on` (dist coord)) gs
                            rest = filter (/= g) gs
                        in map toNumpad (combine coord g) ++ go g rest

main = do
    file <- getContents
    let goals = map ((\[a, b] -> (read a, read b)) . words) $ lines file
    putStrLn $ solve goals

