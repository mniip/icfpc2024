import Data.ByteString qualified as BS
import ICFPC.LambdaMan
import ICFPC.LambdaMan.Greedy


main :: IO ()
main = do
  input <- parseInput <$> BS.getContents
  let sol = greedySolution input
  putStrLn $ showDirection <$> sol
