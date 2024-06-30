module ICFPC.DB.LambdaMan where

import Control.Exception.Safe
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Int
import Data.Profunctor
import Data.Time
import Data.Primitive
import Data.Vector qualified as V
import GHC.Generics
import Hasql.Connection
import Hasql.Session qualified as Session
import Hasql.TH
import ICFPC.DB
import Data.Word


data LambdaManProblem = LambdaManProblem
  { number :: ProblemNumber
  , size :: (Int32, Int32)
  , walls :: Array (PrimArray Word8)
  , start :: (Int32, Int32)
  } deriving stock (Eq, Ord, Show, Generic)

selectProblem :: ProblemNumber -> Connection -> IO LambdaManProblem
selectProblem i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemNumber
    (\
      ( ProblemNumber -> number, width, (width,) -> size
      , fmap (primArrayFromList . BS.unpack) . V.toArray -> walls
      , startX, (startX,) -> start ) -> LambdaManProblem{..})
    [singletonStatement|
      SELECT
        number :: INT4, width :: INT4, height :: INT4, walls :: BYTEA[],
        startX :: INT4, startY :: INT4
      FROM lambdaman_problems WHERE number = $1 :: INT4
    |]

selectProblems :: Connection -> IO [LambdaManProblem]
selectProblems = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemNumber -> number, width, (width,) -> size
      , fmap (primArrayFromList . BS.unpack) . V.toArray -> walls
      , startX, (startX,) -> start ) -> LambdaManProblem{..})
    [vectorStatement|
      SELECT
        number :: INT4, width :: INT4, height :: INT4, walls :: BYTEA[],
        startX :: INT4, startY :: INT4
      FROM lambdaman_problems
    |]

insertProblem :: LambdaManProblem -> Connection -> IO ()
insertProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ dimap
    (\LambdaManProblem{..} ->
      ( unProblemNumber number, fst size, snd size
      , BS.pack . primArrayToList <$> V.fromArray walls
      , fst start, snd start ))
    id
    [resultlessStatement|
      INSERT INTO lambdaman_problems
        (number, width, height, walls, startX, startY)
      VALUES
        ($1 :: INT4, $2 :: INT4, $3 :: INT4, $4 :: BYTEA[],
          $5 :: INT4, $6 :: INT4)
    |]

data LambdaManSolution = LambdaManSolution
  { problem :: ProblemNumber
  , solutionID :: SolutionID
  , program :: ByteString
  , parent :: Maybe SolutionID
  , createdAt :: UTCTime
  } deriving stock (Eq, Ord, Show, Generic)

selectSolution :: SolutionID -> Connection -> IO LambdaManSolution
selectSolution u = either throwIO pure <=< Session.run do
  Session.statement u $ dimap
    unSolutionID
    (\
      ( ProblemNumber -> problem, SolutionID -> solutionID, program
      , fmap SolutionID -> parent, createdAt ) -> LambdaManSolution{..})
    [singletonStatement|
      SELECT
        problem :: INT4, id :: UUID, program :: BYTEA,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM lambdaman_solutions WHERE id = $1 :: UUID
    |]

selectSolutions :: ProblemNumber -> Connection -> IO [LambdaManSolution]
selectSolutions i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemNumber
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, program
      , fmap SolutionID -> parent, createdAt ) -> LambdaManSolution{..})
    [vectorStatement|
      SELECT
        problem :: INT4, id :: UUID, program :: BYTEA,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM lambdaman_solutions WHERE problem = $1 :: INT4
    |]

selectBestSolutions :: Connection -> IO [LambdaManSolution]
selectBestSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, program
      , fmap SolutionID -> parent, createdAt ) -> LambdaManSolution{..})
    [vectorStatement|
      SELECT DISTINCT ON (problem)
        problem :: INT4, id :: UUID, program :: BYTEA,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM lambdaman_solutions ORDER BY problem, LENGTH(program) DESC
    |]

selectAllSolutions :: Connection -> IO [LambdaManSolution]
selectAllSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, program
      , fmap SolutionID -> parent, createdAt ) -> LambdaManSolution{..})
    [vectorStatement|
      SELECT
        problem :: INT4, id :: UUID, program :: BYTEA,
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM lambdaman_solutions
    |]

data LambdaManSolutionInsert = LambdaManSolutionInsert
  { problem :: ProblemNumber
  , program :: ByteString
  , parent :: Maybe SolutionID
  } deriving stock (Eq, Ord, Show, Generic)

insertSolution :: LambdaManSolutionInsert -> Connection -> IO SolutionID
insertSolution s = either throwIO pure <=< Session.run do
  Session.statement s $ dimap
    (\LambdaManSolutionInsert{..}
      -> (unProblemNumber problem, program, fmap unSolutionID parent))
    SolutionID
    [singletonStatement|
      INSERT INTO lambdaman_solutions (problem, program, parent)
      VALUES ($1 :: INT4, $2 :: BYTEA, $3 :: UUID?)
      RETURNING id :: UUID
    |]
