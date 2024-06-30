module ICFPC.Spaceship where

import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BSL

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

formatInput :: Input -> ByteString
formatInput input = BSL.toStrict $ B.toLazyByteString
  $ flip foldMap input.points \SpaceshipPos{x, y} ->
    B.intDec x <> B.char8 ' ' <> B.intDec y <> B.char8 '\n'

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

parseNumpad :: Char -> SpaceshipCommand
parseNumpad = \case
  '1' -> SpaceshipCommand (-1) (-1)
  '2' -> SpaceshipCommand 0 (-1)
  '3' -> SpaceshipCommand 1 (-1)
  '4' -> SpaceshipCommand (-1) 0
  '5' -> SpaceshipCommand 0 0
  '6' -> SpaceshipCommand 1 0
  '7' -> SpaceshipCommand (-1) 1
  '8' -> SpaceshipCommand 0 1
  '9' -> SpaceshipCommand 1 1
  p -> error $ "parseNumpad: " <> show p

formatManoeuvres :: [(Int, SpaceshipVel)] -> ByteString
formatManoeuvres input = BSL.toStrict $ B.toLazyByteString
  $ flip foldMap input \(t, SpaceshipVel{x, y}) ->
    B.intDec t <> B.char8 ' '
      <> B.intDec x <> B.char8 ' ' <> B.intDec y <> B.char8 '\n'

parseManoeuvres :: ByteString -> [(Int, SpaceshipVel)]
parseManoeuvres = either error id . Attoparsec.parseOnly do
  mans <- Attoparsec.many' do
    t <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 ' '
    x <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 ' '
    y <- Attoparsec.signed Attoparsec.decimal
    Attoparsec.char8 '\n'
    pure (t, SpaceshipVel{x, y})
  Attoparsec.endOfInput
  pure mans
