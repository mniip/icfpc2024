import Data.ByteString qualified as BS
import ICFPC.Spaceship
import ICFPC.Spaceship.Greedy
import System.Environment


main :: IO ()
main = getArgs >>= \case
  [prob, mans] -> do
    input <- parseInput <$> BS.readFile prob
    manoeuvres <- parseManoeuvres <$> BS.readFile mans
    putStrLn $ formatNumpad <$> fillManoeuvre input manoeuvres
  _ -> error "Usage: manoeuvre <problem> <manoeuvres>"
