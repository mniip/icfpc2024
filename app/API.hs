import Control.Monad
import Data.Foldable
import Data.ByteString qualified as BS
import ICFPC.API
import Options.Applicative

printTeamInfo :: IO ()
printTeamInfo = do
  teamInfo <- api.getTeamInfo
  print teamInfo

printScoreboard :: IO ()
printScoreboard = do
  scoreboard <- api.getScoreboard
  print scoreboard.columns
  for_ scoreboard.rows print

communicate :: IO ()
communicate = do
  input <- BS.getContents
  output <- api.postCommunicate input
  BS.putStr output

options :: ParserInfo (IO ())
options = info
  do
    (helper <*>) $ subparser $ mconcat
      [ command "team" $ info
        (pure printTeamInfo)
        (progDesc "Retrieve team info")
      , command "scoreboard" $ info
        (pure printScoreboard)
        (progDesc "Retrieve scoreboard")
      , command "communicate" $ info
        (pure communicate)
        (progDesc "Communicate")
      ]
  mempty

main :: IO ()
main = join $ execParser options
