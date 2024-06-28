
import Data.ByteString.Char8 qualified as BS8
import System.Environment
import ICFPC.LambdaMan


main :: IO ()
main = getArgs >>= \case
  [fn] -> do
    input <- parseInput <$> BS8.readFile fn
    solution <- filter (`BS8.elem` "UDRL") <$> getContents
    let state = checkSolution input (readDirection <$> solution)
    BS8.putStrLn $ BS8.unlines $ formatState input state
  _ -> error "Usage: ./lambdaman-vis <maze.txt>"
