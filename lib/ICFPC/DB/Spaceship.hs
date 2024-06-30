module ICFPC.DB.Spaceship where

import Control.Exception.Safe
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.Primitive
import Data.Profunctor
import Data.Time
import Data.UUID
import Data.Vector qualified as V
import GHC.Generics
import Hasql.Connection
import Hasql.Session qualified as Session
import Hasql.TH
import ICFPC.DB


data SpaceshipProblem = SpaceshipProblem
  { number :: ProblemNumber
  , points :: Array (Int32, Int32)
  } deriving stock (Eq, Ord, Show, Generic)

selectProblem :: ProblemNumber -> Connection -> IO SpaceshipProblem
selectProblem i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemNumber
    (\(ProblemNumber -> number, xs, V.toArray . V.zip xs -> points)
      -> SpaceshipProblem{..})
    [singletonStatement|
      SELECT number :: INT4, xs :: INT4[], ys :: INT4[]
      FROM spaceship.problems WHERE number = $1 :: INT4
    |]

selectProblems :: Connection -> IO [SpaceshipProblem]
selectProblems = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      (ProblemNumber -> number, xs, V.toArray . V.zip xs -> points)
      -> SpaceshipProblem{..})
    [vectorStatement|
      SELECT number :: INT4, xs :: INT4[], ys :: INT4[]
      FROM spaceship.problems
    |]

insertProblem :: SpaceshipProblem -> Connection -> IO ()
insertProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ dimap
    (\SpaceshipProblem{..} ->
      ( unProblemNumber number
      , fst <$> V.fromArray points, snd <$> V.fromArray points
      ))
    id
    [resultlessStatement|
      INSERT INTO spaceship.problems (number, xs, ys)
      VALUES ($1 :: INT4, $2 :: INT4[], $3 :: INT4[])
    |]

newtype RouteID = RouteID UUID
  deriving stock (Eq, Ord, Show)

unRouteID :: RouteID -> UUID
unRouteID (RouteID u) = u

data SpaceshipRoute = SpaceshipRoute
  { problem :: ProblemNumber
  , routeID :: RouteID
  , permutation :: Array Int32
  , parent :: Maybe RouteID
  , createdAt :: UTCTime
  }

selectRoute :: RouteID -> Connection -> IO SpaceshipRoute
selectRoute u = either throwIO pure <=< Session.run do
  Session.statement u $ dimap
    unRouteID
    (\
      ( ProblemNumber -> problem, RouteID -> routeID, V.toArray -> permutation
      , fmap RouteID -> parent, createdAt ) -> SpaceshipRoute{..})
    [singletonStatement|
      SELECT
        problem :: INT4, id :: UUID, permutation :: INT4[],
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.routes WHERE id = $1 :: UUID
    |]

selectRoutes :: ProblemNumber -> Connection -> IO [SpaceshipRoute]
selectRoutes i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemNumber
    (toList . fmap \
      ( ProblemNumber -> problem, RouteID -> routeID, V.toArray -> permutation
      , fmap RouteID -> parent, createdAt ) -> SpaceshipRoute{..})
    [vectorStatement|
      SELECT
        problem :: INT4, id :: UUID, permutation :: INT4[],
        parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.routes WHERE problem = $1 :: INT4
    |]

data SpaceshipSolution = SpaceshipSolution
  { problem :: ProblemNumber
  , solutionID :: SolutionID
  , commands :: ByteString
  , route :: Maybe RouteID
  , parent :: Maybe SolutionID
  , createdAt :: UTCTime
  } deriving stock (Eq, Ord, Show, Generic)

selectSolution :: SolutionID -> Connection -> IO SpaceshipSolution
selectSolution u = either throwIO pure <=< Session.run do
  Session.statement u $ dimap
    unSolutionID
    (\
      ( ProblemNumber -> problem, SolutionID -> solutionID, commands
      , fmap RouteID -> route, fmap SolutionID -> parent, createdAt
      ) -> SpaceshipSolution{..})
    [singletonStatement|
      SELECT
        problem :: INT4, id :: UUID, commands :: BYTEA,
        route :: UUID?, parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.solutions WHERE id = $1 :: UUID
    |]

selectSolutions :: ProblemNumber -> Connection -> IO [SpaceshipSolution]
selectSolutions i = either throwIO pure <=< Session.run do
  Session.statement i $ dimap
    unProblemNumber
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, commands
      , fmap RouteID -> route, fmap SolutionID -> parent, createdAt
      ) -> SpaceshipSolution{..})
    [vectorStatement|
      SELECT
        problem :: INT4, id :: UUID, commands :: BYTEA,
        route :: UUID?, parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.solutions WHERE problem = $1 :: INT4
    |]

selectBestSolutions :: Connection -> IO [SpaceshipSolution]
selectBestSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, commands
      , fmap RouteID -> route, fmap SolutionID -> parent, createdAt
      ) -> SpaceshipSolution{..})
    [vectorStatement|
      SELECT DISTINCT ON (problem)
        problem :: INT4, id :: UUID, commands :: BYTEA,
        route :: UUID?, parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.solutions ORDER BY problem, LENGTH(commands) DESC
    |]

selectAllSolutions :: Connection -> IO [SpaceshipSolution]
selectAllSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ dimap
    id
    (toList . fmap \
      ( ProblemNumber -> problem, SolutionID -> solutionID, commands
      , fmap RouteID -> route, fmap SolutionID -> parent, createdAt
      ) -> SpaceshipSolution{..})
    [vectorStatement|
      SELECT
        problem :: INT4, id :: UUID, commands :: BYTEA,
        route :: UUID?, parent :: UUID?, created_at :: TIMESTAMPTZ
      FROM spaceship.solutions
    |]

data SpaceshipSolutionInsert = SpaceshipSolutionInsert
  { problem :: ProblemNumber
  , commands :: ByteString
  , route :: Maybe RouteID
  , parent :: Maybe SolutionID
  } deriving stock (Eq, Ord, Show, Generic)

insertSolution :: SpaceshipSolutionInsert -> Connection -> IO SolutionID
insertSolution s = either throwIO pure <=< Session.run do
  Session.statement s $ dimap
    (\SpaceshipSolutionInsert{..} ->
      ( unProblemNumber problem, commands
      , fmap unRouteID route, fmap unSolutionID parent
      ))
    SolutionID
    [singletonStatement|
      INSERT INTO spaceship.solutions (problem, program, route, parent)
      VALUES ($1 :: INT4, $2 :: BYTEA, $3 :: UUID?, $4 :: UUID?)
      RETURNING id :: UUID
    |]

