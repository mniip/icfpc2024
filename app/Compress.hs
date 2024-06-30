
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import System.Environment
import ICFPC.Language
import ICFPC.Language.Combinators

main :: IO ()
main = getArgs >>= \case
  [BS8.pack -> prefix, filename] -> do
    bs <- BS.readFile filename
    let factors = compress $ prefix <> bs
    BS.putStr $ encodeExpr $ factorOut factors $ prefix <> bs
  _ -> error "Usage: compress <prefix> <filename>"
