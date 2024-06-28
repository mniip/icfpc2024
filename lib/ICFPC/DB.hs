{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.DB where

import Control.Arrow
import Control.Exception.Safe
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int
import Data.Text (Text)
import Data.UUID
import GHC.Generics
import Hasql.Connection
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement
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

problemRow :: Dec.Row Problem
problemRow = do
  problemID <- Dec.column do Dec.nonNullable $ ProblemID <$> Dec.int4
  statement <- Dec.column do Dec.nonNullable Dec.text
  pure Problem{..}

selectProblem :: ProblemID -> Connection -> IO Problem
selectProblem i = either throwIO pure <=< Session.run do
  Session.statement i $ Statement
    "SELECT id, statement FROM problems WHERE id = $1"
    do Enc.param $ Enc.nonNullable $ unProblemID >$< Enc.int4
    do Dec.singleRow problemRow
    True

selectProblems :: Connection -> IO [Problem]
selectProblems = either throwIO pure <=< Session.run do
  Session.statement () $ Statement
    "SELECT id, statement FROM problems"
    do Enc.noParams
    do Dec.rowList problemRow
    True

data ProblemInsert = ProblemInsert
  { statement :: Text
  } deriving stock (Eq, Ord, Show, Generic)

insertProblem :: ProblemInsert -> Connection -> IO ProblemID
insertProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ Statement
    "INSERT INTO problems (statement) VALUES ($1) RETURNING id"
    do Enc.param $ Enc.nonNullable $ (.statement) >$< Enc.text
    do Dec.singleRow $ Dec.column $ Dec.nonNullable $ ProblemID <$> Dec.int4
    True

data ProblemUpdate = ProblemUpdate
  { problemID :: ProblemID
  , statement :: Text
  } deriving stock (Eq, Ord, Show, Generic)

updateProblem :: ProblemUpdate -> Connection -> IO Bool
updateProblem p = either throwIO pure <=< Session.run do
  Session.statement p $ Statement
    "UPDATE problems SET statement = $2 WHERE id = $1"
    do
      divide ((.problemID) &&& (.statement))
        do Enc.param $ Enc.nonNullable $ unProblemID >$< Enc.int4
        do Enc.param $ Enc.nonNullable Enc.text
    do (/= 0) <$> Dec.rowsAffected
    True

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
  } deriving stock (Eq, Ord, Show, Generic)

solutionRow :: Dec.Row Solution
solutionRow = do
  problemID <- Dec.column do Dec.nonNullable $ ProblemID <$> Dec.int4
  solutionID <- Dec.column do Dec.nonNullable $ SolutionID <$> Dec.uuid
  submission <- Dec.column do Dec.nonNullable Dec.text
  score <- Dec.column do Dec.nonNullable Dec.int4
  parent <- Dec.column do Dec.nullable $ SolutionID <$> Dec.uuid
  pure Solution{..}

selectSolution :: SolutionID -> Connection -> IO Solution
selectSolution u = either throwIO pure <=< Session.run do
  Session.statement u $ Statement
    "SELECT problem_id, id, submission, score, parent\
    \ FROM solutions WHERE id = $1"
    do Enc.param $ Enc.nonNullable $ unSolutionID >$< Enc.uuid
    do Dec.singleRow solutionRow
    True

selectSolutions :: ProblemID -> Connection -> IO [Solution]
selectSolutions u = either throwIO pure <=< Session.run do
  Session.statement u $ Statement
    "SELECT problem_id, id, submission, score, parent\
    \ FROM solutions WHERE problem_id = $1"
    do Enc.param $ Enc.nonNullable $ unProblemID >$< Enc.int4
    do Dec.rowList solutionRow
    True

selectBestSolutions :: Connection -> IO [Solution]
selectBestSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ Statement
    "SELECT DISTINCT ON (problem_id)\
    \ problem_id, id, submission, score, parent\
    \ FROM solutions ORDER BY problem_id, score DESC"
    do Enc.noParams
    do Dec.rowList solutionRow
    True

selectAllSolutions :: Connection -> IO [Solution]
selectAllSolutions = either throwIO pure <=< Session.run do
  Session.statement () $ Statement
    "SELECT problem_id, id, submission, score, parent\
    \ FROM solutions"
    do Enc.noParams
    do Dec.rowList solutionRow
    True

data SolutionInsert = SolutionInsert
  { problemID :: ProblemID
  , submission :: Text
  , score :: Int32
  , parent :: Maybe SolutionID
  } deriving stock (Eq, Ord, Show, Generic)

insertSolution :: SolutionInsert -> Connection -> IO SolutionID
insertSolution p = either throwIO pure <=< Session.run do
  Session.statement p $ Statement
    "INSERT INTO solutions (problem_id, submission, score, parent)\
    \ VALUES ($1, $2, $3, $4) RETURNING id"
    do
      divide ((.problemID) &&& (.submission) &&& (.score) &&& (.parent))
        do Enc.param $ Enc.nonNullable $ unProblemID >$< Enc.int4
        $ divide id
          do Enc.param $ Enc.nonNullable Enc.text
          $ divide id
            do Enc.param $ Enc.nonNullable Enc.int4
            do Enc.param $ Enc.nullable $ unSolutionID >$< Enc.uuid
    do Dec.singleRow $ Dec.column $ Dec.nonNullable $ SolutionID <$> Dec.uuid
    True

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
