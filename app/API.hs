import Control.Monad
import Data.Foldable
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import ICFPC.API
import ICFPC.Language
import ICFPC.Language.Eval
import Options.Applicative
import System.IO


printTeamInfo :: IO ()
printTeamInfo = do
  teamInfo <- api.getTeamInfo
  print teamInfo

printScoreboard :: IO ()
printScoreboard = do
  scoreboard <- api.getScoreboard
  print scoreboard.columns
  for_ scoreboard.rows print

printEvalResult :: Expr -> IO ()
printEvalResult (EString s) = tryPutStr $ icfpToByteString s
printEvalResult x = do
  hPutStrLn stderr "\x1B[33mEvaluating term...\x1B[0m"
  case eval x of
    EString s -> tryPutStr $ icfpToByteString s
    x' -> print x'

tryPutStr :: ByteString -> IO ()
tryPutStr bs = do
  BS.putStr bs
  unless ("\n" `BS.isSuffixOf` bs) do
    hPutStrLn stderr "\x1B[31m[No newline at end of file]\x1b[0m"

send :: ByteString -> IO ()
send input = do
  output <- api.postCommunicate $ encodeExpr
    $ EString $ icfpFromByteString input
  printEvalResult $ decodeExpr output

sendFile :: FilePath -> IO ()
sendFile filename = do
  input <- BS.readFile filename
  output <- api.postCommunicate $ encodeExpr
    $ EString $ icfpFromByteString input
  printEvalResult $ decodeExpr output

sendRaw :: ByteString -> IO ()
sendRaw input = do
  output <- api.postCommunicate $ encodeExpr
    $ EString $ icfpFromByteString input
  print $ decodeExpr output

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
      , command "send" $ info
        (send <$> strArgument (metavar "string"))
        (progDesc "Send argument as string and evaluate result")
      , command "sendfile" $ info
        (sendFile <$> strArgument (metavar "file"))
        (progDesc "Send file as string and evaluate result")
      , command "sendraw" $ info
        (sendRaw <$> strArgument (metavar "string"))
        (progDesc "Send argument as string but don't evaluate result")
      ]
  mempty

main :: IO ()
main = join $ execParser options
