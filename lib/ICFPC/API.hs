module ICFPC.API where

import Control.Exception.Safe
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS
import Network.HTTP.Media qualified as M
import Servant.API
import Servant.Client
import System.IO.Unsafe


data TeamInfo = TeamInfo
  { email :: Text
  , name :: Text
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Scoreboard = Scoreboard
  { columns :: [Text]
  , rows :: [[Value]]
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ICFPText

instance Accept ICFPText where
    contentType _ = "text" M.// "icfp"

instance MimeRender ICFPText ByteString where
  mimeRender _ = BSL.fromStrict

instance MimeUnrender ICFPText ByteString where
  mimeUnrender _ = Right . BSL.toStrict

data API mode = API
  { getTeamInfo :: mode
    :- "team"
    :> Get '[JSON] TeamInfo
  , getScoreboard :: mode
    :- "scoreboard"
    :> Get '[JSON] Scoreboard
  , postCommunicate :: mode
    :- "communicate"
    :> ReqBody '[ICFPText] ByteString
    :> Post '[ICFPText] ByteString
  }
  deriving stock (Generic)

clientToIO :: String -> ClientM a -> IO a
clientToIO link m = runClientM m env >>= either throwIO pure
  where
    {-# NOINLINE env #-}
    env = unsafePerformIO do
      mgr <- newTlsManager
      url <- parseBaseUrl link
      pure $ mkClientEnv mgr url

newtype Token = Token Text
  deriving stock (Eq, Ord, Read, Generic)

token :: Token
token = Token $ T.strip $(embedStringFile "token")

instance ToHttpApiData Token where
  toUrlPiece (Token tk) = "Bearer " <> tk

type TokenAuth = Header' '[Required] "Authorization" Token

api :: API (AsClientT IO)
api = fromServant $ hoistClient
  (Proxy @(ToServantApi API))
  (clientToIO "https://boundvariable.space/")
  $ client (Proxy @(TokenAuth :> ToServantApi API)) token
