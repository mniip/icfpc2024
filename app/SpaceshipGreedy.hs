import ICFPC.Spaceship

main :: IO ()
main = do
  input <- getContents
  putStrLn $ formatNumpad <$> greedySolution (parseInput input)
