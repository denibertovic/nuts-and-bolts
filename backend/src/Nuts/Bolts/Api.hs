{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Nuts.Bolts.Api
  ( module Nuts.Bolts.Api.Definitions
  , module Nuts.Bolts.Api.Handlers
  , app
  ) where

import           Nuts.Bolts.Api.Definitions
import           Nuts.Bolts.Api.Handlers

import           Control.Monad                 (Monad (..))
import           Control.Monad.Except
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ReaderT, runReaderT)
import           Crypto.PasswordStore          (makePassword, verifyPassword)
import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Int                      (Int64)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Data.Time.Clock               (addUTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql   (Entity (..), fromSqlKey, insert,
                                                selectList)
import           Database.Persist.Sql
import           GHC.Generics                  (Generic)
import           Network.Wai                   (Application)
import           Servant
import           Servant.Auth.Server
import           Servant.Server.Internal.ServerError (ServerError)

-- import           Database.Esqueleto
import           Data.Typeable                 (Typeable)

import           Nuts.Bolts.Api.Definitions (AppM, AuthUser (..))
import           Nuts.Bolts.Config          (Config (..))
import           Nuts.Bolts.Models
import           Nuts.Bolts.Types
import           Nuts.Bolts.Utils


nutsBoltsAPI :: Proxy (NutsBoltsAPI '[JWT])
nutsBoltsAPI = Proxy

readerToHandler :: Config -> AppM a -> Handler a
readerToHandler cfg app = runReaderT app cfg

readerToServer :: Config
               -> CookieSettings
               -> JWTSettings
               -> Server (NutsBoltsAPI auths)
readerToServer cfg cC jwtC = hoistServerWithContext nutsBoltsAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) (readerToHandler cfg) (nutsBoltsServer cC jwtC)

app :: Config -> CookieSettings -> JWTSettings -> Application
app cfg cC jwtC = serveWithContext nutsBoltsAPI ctx' (readerToServer cfg cC jwtC)
  where
    ctx' = cC :. jwtC :. EmptyContext

-- Servers
pingServer :: AuthResult AuthUser -> ServerT PingAPI AppM
pingServer au = ping au

-- Servers
accountServer :: AuthResult AuthUser -> ServerT AccountAPI AppM
accountServer au = listAccounts au
              :<|> getAccount au
              :<|> deleteAccount au
              :<|> updateAccount au
              :<|> updateAccountPassword au

unprotectedServer :: CookieSettings -> JWTSettings -> ServerT UnprotectedAPI AppM
unprotectedServer cs jwts = loginUser cs jwts :<|> registerNewAccount cs jwts :<|> hello cs jwts

nutsBoltsServer :: CookieSettings -> JWTSettings -> ServerT (NutsBoltsAPI auths) AppM
nutsBoltsServer cs jwts = accountServer :<|> pingServer :<|> unprotectedServer cs jwts
