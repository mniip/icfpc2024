{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.DB where

import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed
import Data.Int
import Data.UUID
import Hasql.Connection
import System.IO.Unsafe


newtype ProblemNumber = ProblemNumber Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

unProblemNumber :: ProblemNumber -> Int32
unProblemNumber (ProblemNumber i) = i

newtype SolutionID = SolutionID UUID
  deriving stock (Eq, Ord, Show)

unSolutionID :: SolutionID -> UUID
unSolutionID (SolutionID u) = u

dbSettings :: ByteString
dbSettings = BS8.strip $(embedStringFile "db_settings")

deriving anyclass instance Exception ConnectionError

connection :: Connection
connection = unsafePerformIO $ acquire dbSettings >>= either throwIO pure
{-# NOINLINE connection #-}

runDB :: (Connection -> IO a) -> IO a
runDB = bracket
  (acquire dbSettings >>= either throwIO pure)
  release
