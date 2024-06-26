{-# OPTIONS_GHC -Wno-orphans #-}
module ICFPC.DB where

import Control.Exception.Safe
import Control.Monad
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed
import Data.Int
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Hasql
import Rel8 qualified


deriving anyclass instance Exception Hasql.ConnectionError

runDB :: Hasql.Session a -> IO a
runDB act = bracket
  (Hasql.acquire settings >>= either throwIO pure)
  Hasql.release
  (Hasql.run act >=> either throwIO pure)
  where
    settings = BS8.strip $(embedStringFile "db_settings")

test :: IO [Int32]
test = runDB $ Hasql.statement () $ Rel8.run $ Rel8.select (pure 1)
