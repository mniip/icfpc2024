import Data.ByteString qualified as BS
import ICFPC.Spaceship

main :: IO ()
main = do
  input <- BS.getContents
  putStrLn $ formatNumpad <$> greedySolution (parseInput input)
