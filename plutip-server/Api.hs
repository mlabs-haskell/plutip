module Api where

import Api.Handlers
  ( startClusterHandler
  , stopClusterHandler
  )
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader (runReaderT)
import Data.Kind (Type)
import Network.Wai.Middleware.Cors qualified as Cors
import Servant
  ( (:<|>)((:<|>))
  , (:>)
  , Application
  , JSON
  , Handler
  , Post
  , Proxy(Proxy)
  , ReqBody
  , Server
  , ServerT
  , err400
  , errBody
  , hoistServer
  , serve
  , throwError
  )
import Types
  ( AppM(AppM)
  , Env(Env)
  , PlutipServerError(PlutipServerError)
  , ServerOptions
  , StartClusterRequest
  , StartClusterResponse
  , StopClusterRequest
  , StopClusterResponse
  , options
  )

type Api =
  "start"
    :> ReqBody '[JSON] StartClusterRequest
    :> Post '[JSON] StartClusterResponse
  :<|> "stop"
    :> ReqBody '[JSON] StopClusterRequest
    :> Post '[JSON] StopClusterResponse

app :: Env -> Application
app = Cors.cors (const $ Just policy) . serve api . appServer
  where
    policy :: Cors.CorsResourcePolicy
    policy =
      Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = ["OPTIONS", "GET", "POST"]
        }

api :: Proxy Api
api = Proxy

server :: ServerOptions -> ServerT Api AppM
server serverOptions = startClusterHandler serverOptions
  :<|> stopClusterHandler

appServer :: Env -> Server Api
appServer env@(Env { options }) =
  hoistServer api appHandler (server options)
  where
    appHandler :: forall (a :: Type). AppM a -> Handler a
    appHandler (AppM x) = tryServer x >>= either handleError pure
      where
        tryServer ::
          ReaderT Env IO a ->
          Handler (Either PlutipServerError a)
        tryServer =
          liftIO
            . try @_ @PlutipServerError
            . flip runReaderT env

        handleError ::
          PlutipServerError ->
          Handler a
        handleError PlutipServerError =
            throwError err400 {errBody = "Server error"}
