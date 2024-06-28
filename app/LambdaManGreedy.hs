import ICFPC.LambdaMan.Greedy


main :: IO ()
main = do
  input <- parseInput <$> getContents
  let sol = greedySolution input
  putStrLn $ show =<< sol
