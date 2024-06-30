import Data.ByteString qualified as BS
import ICFPC.Spaceship
import ICFPC.Spaceship.Greedy
import System.Environment


main :: IO ()
main = getArgs >>= \case
  [filename] -> do
    input <- parseInput <$> BS.readFile filename
    BS.writeFile (filename <> ".greedy") $ formatInput $ greedyManoeuvreOrder input
  _ -> error "Usage: greedy-route <problem>"
