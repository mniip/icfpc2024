module ICFPC.Spaceship where

import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)

newtype Input = Input
  { points :: [SpaceshipPos]
  } deriving stock (Eq, Ord, Show)

data SpaceshipPos = SpaceshipPos
  { x :: !Int
  , y :: !Int
  } deriving stock (Eq, Ord, Show)

data SpaceshipVel = SpaceshipVel
  { x :: !Int
  , y :: !Int
  } deriving stock (Eq, Ord, Show)

data SpaceshipState = SpaceshipState
  { pos :: SpaceshipPos
  , vel :: SpaceshipVel
  } deriving stock (Eq, Ord, Show)

initState :: SpaceshipState
initState = SpaceshipState
  { pos = SpaceshipPos
    { x = 0
    , y = 0
    }
  , vel = SpaceshipVel
    { x = 0
    , y = 0
    }
  }

data SpaceshipCommand = SpaceshipCommand
  { accelX :: !Int
  , accelY :: !Int
  } deriving stock (Eq, Ord, Show)

parseInput :: ByteString -> Input
parseInput = either error id . Attoparsec.parseOnly do
  points <- Attoparsec.many' do
    x <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 ' '
    y <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 '\n'
    pure SpaceshipPos{x, y}
  Attoparsec.endOfInput
  pure Input{..}

formatNumpad :: SpaceshipCommand -> Char
formatNumpad = \case
  SpaceshipCommand (-1) (-1) -> '1'
  SpaceshipCommand 0 (-1) -> '2'
  SpaceshipCommand 1 (-1) -> '3'
  SpaceshipCommand (-1) 0 -> '4'
  SpaceshipCommand 0 0 -> '5'
  SpaceshipCommand 1 0 -> '6'
  SpaceshipCommand (-1) 1 -> '7'
  SpaceshipCommand 0 1 -> '8'
  SpaceshipCommand 1 1 -> '9'
  p -> error $ "formatNumpad: " <> show p
