import Data.ByteString qualified as BS
import ICFPC.LambdaMan
import ICFPC.LambdaMan.Graph


main :: IO ()
main = do
  input <- parseInput <$> BS.getContents
  let sol = breadthFirstGraphSolution input
  putStrLn $ showDirection <$> sol
