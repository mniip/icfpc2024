import Data.ByteString qualified as BS
import ICFPC.Spaceship
import ICFPC.Spaceship.Greedy

main :: IO ()
main = do
  input <- BS.getContents
  BS.putStr $ formatManoeuvres $ dumbSolution (parseInput input)
