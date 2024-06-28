{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.DB where

import Control.Exception.Safe
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed
import Data.Foldable
import Data.Int
import Data.Profunctor
import Data.Text (Text)
import Data.Time
import Data.UUID
import GHC.Generics
import Hasql.Connection
import Hasql.Session qualified as Session
import Hasql.TH
import System.IO.Unsafe


newtype ProblemID = ProblemID Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

unProblemID :: ProblemID -> Int32
unProblemID (ProblemID i) = i

data Problem = Problem
  { problemID :: ProblemID
  , statement :: Text
  } deriving stock (Eq, Ord, Show, Generic)

selectProblem :: ProblemID -> Connection -> IO Problem
selectProblem i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemID
    (\(ProblemID -> problemID, statement) -> Problem{..})
    [singletonStatement|
      SELECT id :: INT4, statement :: TEXT FROM problems WHERE id = $1 :: INT4
    |]

selectProblems :: Connection -> IO [Problem]
selectProblems = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \(ProblemID -> problemID, statement) -> Problem{..})
    [vectorStatement|
      SELECT id :: INT4, statement :: TEXT FROM problems
    |]

data ProblemInsert = ProblemInsert
  { statement :: Text
  } deriving stock (Eq, Ord, Show, Generic)

insertProblem :: ProblemInsert -> Connection -> IO ProblemID
insertProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ dimap
    (\ProblemInsert{..} -> statement)
    ProblemID
    [singletonStatement|
      INSERT INTO problems (statement) VALUES ($1 :: TEXT) RETURNING id :: INT4
    |]

data ProblemUpdate = ProblemUpdate
  { problemID :: ProblemID
  , statement :: Text
  } deriving stock (Eq, Ord, Show, Generic)

updateProblem :: ProblemUpdate -> Connection -> IO Bool
updateProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ dimap
    (\ProblemUpdate{..} -> (unProblemID problemID, statement))
    (/= 0)
    [rowsAffectedStatement|
      UPDATE problems SET statement = $2 :: TEXT where id = $1 :: INT4
    |]

newtype SolutionID = SolutionID UUID
  deriving stock (Eq, Ord, Show)

unSolutionID :: SolutionID -> UUID
unSolutionID (SolutionID u) = u

data Solution = Solution
  { problemID :: ProblemID
  , solutionID :: SolutionID
  , submission :: Text
  , score :: Int32
  , parent :: Maybe SolutionID
  , createdAt :: UTCTime
  } deriving stock (Eq, Ord, Show, Generic)

selectSolution :: SolutionID -> Connection -> IO Solution
selectSolution u = either throwIO pure <=< Session.run do
  Session.statement u $ dimap
    unSolutionID
    (\
      ( ProblemID -> problemID, SolutionID -> solutionID, submission, score
      , fmap SolutionID -> parent, createdAt ) -> Solution{..})
    [singletonStatement|
      SELECT
        problem_id :: INT4, id :: UUID, submission :: TEXT, score :: INT4,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM solutions WHERE id = $1 :: UUID
    |]

selectSolutions :: ProblemID -> Connection -> IO [Solution]
selectSolutions i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemID
    (toList . fmap \
      ( ProblemID -> problemID, SolutionID -> solutionID, submission, score
      , fmap SolutionID -> parent, createdAt ) -> Solution{..})
    [vectorStatement|
      SELECT
        problem_id :: INT4, id :: UUID, submission :: TEXT, score :: INT4,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM solutions WHERE problem_id = $1 :: INT4
    |]

selectBestSolutions :: Connection -> IO [Solution]
selectBestSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemID -> problemID, SolutionID -> solutionID, submission, score
      , fmap SolutionID -> parent, createdAt ) -> Solution{..})
    [vectorStatement|
      SELECT DISTINCT ON (problem_id)
        problem_id :: INT4, id :: UUID, submission :: TEXT, score :: INT4,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM solutions ORDER BY problem_id, score DESC
    |]

selectAllSolutions :: Connection -> IO [Solution]
selectAllSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemID -> problemID, SolutionID -> solutionID, submission, score
      , fmap SolutionID -> parent, createdAt ) -> Solution{..})
    [vectorStatement|
      SELECT
        problem_id :: INT4, id :: UUID, submission :: TEXT, score :: INT4,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM solutions
    |]

data SolutionInsert = SolutionInsert
  { problemID :: ProblemID
  , submission :: Text
  , score :: Int32
  , parent :: Maybe SolutionID
  } deriving stock (Eq, Ord, Show, Generic)

insertSolution :: SolutionInsert -> Connection -> IO SolutionID
insertSolution s = either throwIO pure <=< Session.run do
  Session.statement s $ dimap
    (\SolutionInsert{..}
      -> (unProblemID problemID, submission, score, fmap unSolutionID parent))
    SolutionID
    [singletonStatement|
      INSERT INTO solutions (problem_id, submission, score, parent)
      VALUES ($1 :: INT4, $2 :: TEXT, $3 :: INT4, $4 :: UUID?)
      RETURNING id :: UUID
    |]

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
