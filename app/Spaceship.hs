import Data.List (nub, sort, minimumBy)
import Data.Function (on)

fancyPlan :: Int -> [Int]
fancyPlan 0 = []
fancyPlan 1 = [1, -1]
fancyPlan d = replicate n 1 ++ replicate (n-1) (-1) ++ replicate (d - n*n) 0 ++ [-1]
    where n = root 1
          root i | d >= i*i && (i+1)*(i+1) > d = i
                 | otherwise = root (i+1)

-- Land with zero velocity on the target
fromTo :: Int -> Int -> [Int]
fromTo a b | a == b = []
           | a < b = fancyPlan (b-a) -- [1] ++ (replicate (b-a-1) 0) ++ [-1]
           | a > b = reverse (fromTo b a)

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

