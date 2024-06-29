import qualified Data.Map as M
import Data.List (nub, group)
import System.Environment

data Cell = Num Integer
          | Lefty
          | Righty
          | Top
          | Bottom
          | Plus
          | Minus
          | Mul
          | Div
          | Mod
          | Time
          | Equal
          | NotEq
          | Result
          | InpA
          | InpB
          deriving (Eq, Ord)

instance Show Cell where
    show (Num n) = show n
    show Lefty = "<"
    show Righty = ">"
    show Top = "^"
    show Bottom = "v"
    show Plus = "+"
    show Minus = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Time = "@"
    show Equal = "="
    show NotEq = "#"
    show Result = "S"
    show InpA = "A"
    show InpB = "B"

type Point = (Integer, Integer)
type Grid = M.Map Point Cell

dimensions :: Grid -> (Point, Point)
dimensions grid = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where ks = M.keys grid
          xs = map fst ks
          ys = map snd ks

printGrid :: Grid -> String
printGrid grid = unlines [unwords [showCell x y | x <- [x0..x1]] | y <- [y0..y1]]
    where ((x0, y0), (x1, y1)) = dimensions grid
          showCell x y = case M.lookup (x, y) grid of
                              Nothing -> "."
                              Just c -> show c

readGrid :: String -> Grid
readGrid str = M.fromList [((x, y), readCell w) | (y, l) <- zip [0..] (lines str), (x, w) <- zip [0..] (words l), w /= "."]
    where readCell "<" = Lefty
          readCell ">" = Righty
          readCell "^" = Top
          readCell "v" = Bottom
          readCell "+" = Plus
          readCell "-" = Minus
          readCell "*" = Mul
          readCell "/" = Div
          readCell "%" = Mod
          readCell "@" = Time
          readCell "=" = Equal
          readCell "#" = NotEq
          readCell "S" = Result
          readCell "A" = InpA
          readCell "B" = InpB
          readCell w = Num (read w)

-- Replace A and B constants by given integers
setupInputs :: Grid -> Integer -> Integer -> Grid
setupInputs grid a b = M.map setup grid
    where setup InpA = Num a
          setup InpB = Num b
          setup c = c

-- What can happen after time travel?
data TimeResult = CrashT -- A crash
                | Travel Integer [(Point, Cell)] -- Go back in time and modify given cells
                deriving (Show)

checkTimeTravel :: Grid -> Maybe TimeResult
checkTimeTravel grid = if null warpRequests then Nothing
                       else if (length . group $ map (\(Travel t _) -> t) warpRequests) > 1
                            then return CrashT
                            else let Travel time _ = head warpRequests
                                     commands = nub $ concatMap (\(Travel _ c) -> c) warpRequests
                                 in return $ Travel time commands
    where ks = M.keys grid
          warps = filter (\k -> M.lookup k grid == Just Time) ks
          readyWarp (x, y) = case (M.lookup (x-1, y) grid, M.lookup (x+1, y) grid, M.lookup (x, y-1) grid, M.lookup (x, y+1) grid) of
                                  (Just (Num dx), Just (Num dy), Just v, Just (Num dt)) -> [Travel dt [((x-dx, y-dy), v)]]
                                  _ -> []
          warpRequests = concatMap readyWarp warps

usualStep :: Grid -> Grid
usualStep grid = if length writePoss /= length (nub writePoss)
                 then error "Writing in the same cell"
                 else case filter (\(p, _) -> M.lookup p grid  == Just Result) writeReqs of 
                           ((_, res):_) -> error $ "Result: " ++ show res
                           _ -> foldr (uncurry M.insert) (foldr M.delete grid takeReqs) writeReqs
    where ks = M.keys grid
          takeReqs :: [Point]
          writeReqs :: [(Point, Cell)]
          (takeReqs, writeReqs) = (\(a, b) -> (concat a, concat b)) . unzip $ map readyOp ks
          writePoss = map fst writeReqs
          -- For each operator returns positions which must be cleared, and positions which must be written to
          readyOp (x, y) = case (M.lookup (x, y) grid, M.lookup (x-1, y) grid, M.lookup (x+1, y) grid, M.lookup (x, y-1) grid, M.lookup (x, y+1) grid) of
                                (Just Lefty, _, Just v, _, _) -> ([(x+1, y)], [((x-1, y), v)]) 
                                (Just Righty, Just v, _, _, _) -> ([(x-1, y)], [((x+1, y), v)])
                                (Just Top, _, _, _, Just v) -> ([(x, y+1)], [((x, y-1), v)])
                                (Just Bottom, _, _, Just v, _) -> ([(x, y-1)], [((x, y+1), v)])
                                (Just Plus, Just (Num v1), _, Just (Num v2), _) -> ([(x-1, y), (x, y-1)], [((x+1, y), Num $ v1+v2), ((x, y+1), Num $ v1+v2)])
                                (Just Minus, Just (Num v1), _, Just (Num v2), _) -> ([(x-1, y), (x, y-1)], [((x+1, y), Num $ v1-v2), ((x, y+1), Num $ v1-v2)])
                                (Just Mul, Just (Num v1), _, Just (Num v2), _) -> ([(x-1, y), (x, y-1)], [((x+1, y), Num $ v1*v2), ((x, y+1), Num $ v1*v2)])
                                (Just Div, Just (Num v1), _, Just (Num v2), _) -> ([(x-1, y), (x, y-1)], [((x+1, y), Num $ v1 `quot` v2), ((x, y+1), Num $ v1 `quot` v2)])
                                (Just Mod, Just (Num v1), _, Just (Num v2), _) -> ([(x-1, y), (x, y-1)], [((x+1, y), Num $ v1 `rem` v2), ((x, y+1), Num $ v1 `rem` v2)])
                                (Just Equal, Just v1, _, Just v2, _) -> if v1 == v2 then ([(x-1, y), (x, y-1)], [((x+1, y), v1), ((x, y+1), v1)]) else ([], [])
                                (Just NotEq, Just v1, _, Just v2, _) -> if v1 == v2 then ([(x-1, y), (x, y-1)], [((x+1, y), v2), ((x, y+1), v1)]) else ([], [])
                                _ -> ([], [])

-- The history of our program, most recent snapshots being at the front
type Universe = [Grid]

eval :: Universe -> Universe
eval (grid:uni) = let usualSt = usualStep grid
                  in case checkTimeTravel grid of
                          Just (Travel time writeReqs) -> let (grid':uni') = drop (fromIntegral time - 1) uni
                                                          in usualSt `seq` (foldr (uncurry M.insert) grid' writeReqs):uni'
                          Nothing -> (usualStep grid):grid:uni

evaluate :: Grid -> Integer -> Integer -> IO ()
evaluate grid a b = go [grid0]
    where grid0 = setupInputs grid a b
          go uni = do
              putStrLn $ printGrid (head uni)
              go $ eval uni

main = do
    [file, a, b] <- getArgs
    board <- readFile file
    let grid = readGrid board
    evaluate grid (read a) (read b)
