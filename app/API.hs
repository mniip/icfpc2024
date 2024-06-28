import Control.Monad
import Data.Foldable
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

options :: ParserInfo (IO ())
options = info
  do
    (helper <*>) $ subparser $ mconcat
      [ command "team" $ info
        (pure printTeamInfo)
        (progDesc "Retrieve team info")
      ,  command "scoreboard" $ info
        (pure printScoreboard)
        (progDesc "Retrieve scoreboard")
      ]
  mempty

main :: IO ()
main = join $ execParser options
