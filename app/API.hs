import ICFPC.API

main :: IO ()
main = do
  teamInfo <- api.getTeamInfo
  print teamInfo
