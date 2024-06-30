import Data.ByteString.Char8 qualified as BS8
import Data.Char
import ICFPC.LambdaMan
import System.Environment
import System.IO


data Game = Game
  { state :: !LambdaManState
  , solution :: [Direction] -- Stored in reverse, i.e. recent moves are at the front
  }

playGame :: Input -> Game -> IO ()
playGame input game = do
  BS8.putStrLn $ BS8.unlines $ formatState input game.state
  putStr $ "Solution: " ++ reverse (showDirection <$> game.solution) ++ "\n\n"
  putStr "> "
  c <- getChar
  putStr "\n"
  let
    mChar = case toUpper c of
      'W' -> Just U
      'S' -> Just D
      'A' -> Just L
      'D' -> Just R
      _ -> Nothing
  case mChar of
    Nothing -> playGame input game
    Just d -> do
      let state' = simulateStep input game.state d
      if (state'.posX, state'.posY) == (game.state.posX, game.state.posY)
      then playGame input game
      else playGame input game
        { state = state'
        , solution = d : game.solution
        }

main :: IO ()
main = getArgs >>= \case
  [fn] -> do
    input <- parseInput <$> BS8.readFile fn
    hSetBuffering stdin NoBuffering
    playGame input Game
      { state = initialState input
      , solution = []
      }
  _ -> error "Usage: ./lambdaman-manual <maze.txt>"
