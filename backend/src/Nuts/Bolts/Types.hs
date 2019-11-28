{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Nuts.Bolts.Types where

import           Cases                  (snakify)
import           Data.Aeson             (FromJSON, ToJSON, Value (..),
                                         genericParseJSON, genericToJSON,
                                         object, parseJSON, toJSON, withObject,
                                         withText, (.:), (.=))
import qualified Data.Aeson             as JSON
import           Data.Aeson.Types       (Parser, defaultOptions,
                                         fieldLabelModifier)
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           Database.Persist.TH    (derivePersistField)
import           GHC.Generics           (Generic)
import           Servant.Auth.Server    (FromJWT, ToJWT)


data JsonToken = JsonToken
  { token :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON JsonToken
instance FromJSON JsonToken

newtype Email =
  Email T.Text
  deriving (Eq, Show, Read, Generic)

derivePersistField "Email"
instance ToJSON Email
instance FromJSON Email

fromEmail :: Email -> T.Text
fromEmail (Email e) = e

data Login = Login
  { email    :: Email
  , password :: T.Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

newtype JsonError =
  JsonError String

instance ToJSON JsonError where
  toJSON (JsonError b) = object ["error" .= b]

data Password
  = Password !T.Text
  | PasswordHidden
  deriving (Eq, Show, Read, Generic)

instance ToJSON Password where
  toJSON (Password _)   = object ["password" .= toJSON PasswordHidden]
  toJSON PasswordHidden = JSON.String "***encrypted***"

instance FromJSON Password where
  parseJSON (Object o) = do
    p <- o .: "password"
    return $ Password p
  parseJSON _ = fail "Password is not an object"

newtype Url =
  Url T.Text
  deriving (Eq, Show, Read, Generic)

derivePersistField "Url"

instance ToJSON Url
instance FromJSON Url
