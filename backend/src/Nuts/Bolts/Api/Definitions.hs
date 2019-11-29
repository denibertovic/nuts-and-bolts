{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Nuts.Bolts.Api.Definitions where

import           Control.Monad               (Monad (..))
import           Control.Monad.Except
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Crypto.PasswordStore        (makePassword, verifyPassword)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int                    (Int64)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Time.Clock             (addUTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectList)
import           Database.Persist.Sql
import           GHC.Generics                (Generic)
import           Network.Wai                 (Application)
import           Servant
import           Servant.Auth.Server
import           Servant.Server.Internal.ServerError (ServerError)
import           Servant.HTML.Lucid (HTML)
import           Lucid (Html)
-- import           Database.Esqueleto
import           Data.Typeable               (Typeable)
import           Nuts.Bolts.Config        (Config (..))
import           Nuts.Bolts.Models
import           Nuts.Bolts.Types
import           Nuts.Bolts.Utils


data AuthUser = AuthUser
  { uid     :: Int64
  , account :: Account
  } deriving (Eq, Show, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser

instance ToJWT AuthUser
instance FromJWT AuthUser


type UnprotectedAPI = "login" :> ReqBody '[JSON] Login
                              :> Post '[JSON] JsonToken
                    :<|> "register" :> QueryParam "code" String
                                    :> ReqBody '[JSON] Account
                                    :> Post '[JSON] Int64
                    :<|> Get '[HTML] (Html ())


type NutsBoltsAPI auths = (Auth auths AuthUser :> "accounts" :> AccountAPI)
                     :<|> (Auth auths AuthUser :> "ping" :> PingAPI)
                     :<|> UnprotectedAPI

type AppM = ReaderT Config Handler

type PingAPI = Get '[JSON] NoContent -- | Returns 200 or 401

-- type ProtectedAPI = Get '[JSON] [Account]
type AccountAPI = Get '[JSON] [Account] -- | List all accounts (SHOULD be admin only)
             :<|> Get '[JSON] Account -- | Get specific account
             :<|> ReqBody '[JSON] Password :> DeleteNoContent '[JSON] NoContent -- | Delete account
             :<|> ReqBody '[JSON] Account :> Post '[JSON] Account  -- | Create new account
             :<|> ReqBody '[JSON] Password :> Post '[JSON] NoContent -- | Update password for existing account
