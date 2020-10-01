{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Nuts.Bolts.Models where

import           Control.Monad.Reader             (ReaderT, asks, liftIO)
import           Control.Monad.Reader.Class       (MonadReader (..))
import           Control.Monad.Writer
import           Data.Aeson                       (FromJSON, ToJSON, object,
                                                   toJSON, (.=))
import qualified Data.ByteString.Char8            as B8
import           Data.Text                        as T
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V4                     as UUID

import           Database.Persist.Postgresql      (SqlBackend (..),
                                                   runMigration, runSqlPool)
import           Database.Persist.Postgresql.Json (Jsonb)
import           Database.Persist.Sql
import           Database.Persist.TH              (mkMigrate, mkPersist, mkDeleteCascade,
                                                   persistLowerCase, share,
                                                   sqlSettings, sqlOnlySettings)
import           GHC.Generics                     (Generic)

import           Servant.Auth.Server              (FromJWT (..), ToJWT (..))

import           Nuts.Bolts.Types
import           Nuts.Bolts.Config


instance PersistField UUID.UUID where
  toPersistValue = PersistDbSpecific . B8.pack . UUID.toString
  fromPersistValue (PersistDbSpecific t) =
    case UUID.fromString $ B8.unpack t of
      Just x  -> Right x
      Nothing -> Left "Invalid UUID"
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID.UUID where
  sqlType _ = SqlOther "uuid"

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] $ [persistLowerCase|
Account
  email Email
  password T.Text
  firstName T.Text
  lastName T.Text
  UniqueEmail email
 deriving Eq Show Generic
|]

instance ToJSON Account where
    toJSON u = object [ "accountFirstName" .= accountFirstName u
                      , "accountLastName" .=  accountLastName u
                      , "accountEmail" .= accountEmail u
                      , "accountPassword" .= PasswordHidden
                      ]

instance FromJSON Account

instance ToJWT Account
instance FromJWT Account

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

printMigrations :: ReaderT SqlBackend IO ()
printMigrations = printMigration migrateAll

runDb :: (MonadIO m,
    Control.Monad.Reader.Class.MonadReader Config m) =>
    ReaderT SqlBackend IO b -> m b
runDb query = do
    pool <- asks connPool
    liftIO $ runSqlPool query pool
