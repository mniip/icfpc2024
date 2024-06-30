import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import ICFPC.Spaceship
import System.Environment

main :: IO ()
main = getArgs >>= \case
  [solution] -> do
    commands <- map parseNumpad . BS8.unpack . BS8.strip
      <$> BS.readFile solution
    go initState commands
  _ -> error "Usage: spaceship-sim <solution>"
  where
    go !s [] = do
      putStrLn $ show s.pos.x <> " " <> show s.pos.y
    go !s (!a : xs) = do
      putStrLn $ show s.pos.x <> " " <> show s.pos.y
      go SpaceshipState
        { pos = SpaceshipPos
          { x = s.pos.x + s.vel.x + a.accelX
          , y = s.pos.y + s.vel.y + a.accelY
          }
        , vel = SpaceshipVel
          { x = s.vel.x + a.accelX
          , y = s.vel.y + a.accelY
          }
        } xs
