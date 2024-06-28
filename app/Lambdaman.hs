{-# LANGUAGE NoOverloadedStrings #-}
import qualified Data.Array as A
import Data.List (elem)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.IO

data Cell = Player | Empty | Pill | Wall deriving (Eq)

instance Show Cell where
    show Player = "L"
    show Empty = " "
    show Pill = "."
    show Wall = "#"

type Maze = A.Array (Int, Int) Cell

readMaze :: String -> Maze
readMaze str = A.listArray ((1, 1), (length $ lines str, length . head $ lines str)) . concatMap (map toCell) $ lines str
    where toCell 'L' = Player
          toCell '.' = Pill
          toCell '#' = Wall
          toCell _ = Empty

printMaze :: Maze -> String
printMaze maze = unlines [concat [show (maze A.! (x, y)) | y <- [1..w]] | x <- [1..h]]
    where (_, (h, w)) = A.bounds maze

data Game = Game {
    maze :: Maze,
    coord :: (Int, Int), -- Player coordinates
    solution :: String, -- Stored in reverse, i.e. recent moves are at the front
    score :: Int }

locatePlayer :: Maze -> (Int, Int)
locatePlayer = fst . head . filter (\(_, c) -> c == Player) . A.assocs

doMove :: Game -> Char -> Game
doMove game@(Game maze (x, y) sol score) comm = if x' > 0 && x' <= h && y' > 0 && y' <= w && maze A.! (x', y') /= Wall
                                                then Game (maze A.// [((x, y), Empty), ((x', y'), Player)]) (x', y') (comm : sol) (score+1)
                                                else game
    where (x', y') = case comm of
                        'U' -> (x-1, y)
                        'D' -> (x+1, y)
                        'L' -> (x, y-1)
                        'R' -> (x, y+1)
          (_, (h, w)) = A.bounds maze

wasdToUDLR :: Char -> Char
wasdToUDLR c = case c of
                    'W' -> 'U'
                    'A' -> 'L'
                    'S' -> 'D'
                    'D' -> 'R'
                    _ -> c

playGame :: Maze -> IO ()
playGame m = go (Game m (locatePlayer m) "" 0)
    where go game = do
              putStrLn $ printMaze (maze game)
              let sol = reverse $ solution game
              putStr $ "Solution: " ++ sol ++ "\n\n"
              putStr "> "
              inp <- getChar
              putStr "\n"
              let inp' = wasdToUDLR $ toUpper inp
              if inp' `elem` "UDLR"
              then go (doMove game inp') else putStrLn "Wrong command" >> go game

main = do
    args <- getArgs
    file <- readFile (head args)
    hSetBuffering stdin NoBuffering
    playGame $ readMaze file
