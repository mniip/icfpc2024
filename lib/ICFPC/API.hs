module ICFPC.API where

import Control.Exception
import Data.Aeson
import Data.FileEmbed
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import System.IO.Unsafe


data TeamInfo = TeamInfo
  { email :: Text
  , name :: Text
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Token = Token Text
  deriving stock (Eq, Ord, Read, Generic)

instance ToHttpApiData Token where
  toUrlPiece (Token tk) = "Bearer " <> tk

type TokenAuth = Header' '[Required] "Authorization" Token

data API mode = API
  { getTeamInfo :: mode
    :- "team"
    :> Get '[JSON] TeamInfo
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

api :: API (AsClientT IO)
api = fromServant $ hoistClient
  (Proxy @(ToServantApi API))
  (clientToIO "https://boundvariable.space/")
  $ client (Proxy @(TokenAuth :> ToServantApi API)) token

token :: Token
token = Token $ T.strip $(embedStringFile "token")
