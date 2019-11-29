{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Nuts.Bolts.Api.Handlers where

import           Control.Monad                    (Monad (..))
import           Control.Monad.Except
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Crypto.PasswordStore             (makePassword, verifyPassword)
import           Control.Exception                (try, SomeException(..), IOException(..))
import           Data.Aeson                       (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Int                         (Int64)
import           Data.List                        ((\\))
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Time.Calendar               (diffDays)
import           Data.Time.Clock                  (addUTCTime, getCurrentTime)
import           Data.Time.Clock                  (getCurrentTime, utctDay)
import qualified Data.UUID                        as UUID
import           Database.Persist
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey,
                                                   insert, selectList)
import           Database.Persist.Postgresql.Json (Jsonb (..))
import           Database.Persist.Sql
import           GHC.Generics                     (Generic)
import           Network.Wai                      (Application)
import           Servant
import           Servant.Auth.Server
import           Lucid (Html, h2_)

-- import           Database.Esqueleto
import           Data.Typeable                    (Typeable)
import           Nuts.Bolts.Api.Definitions    (AppM, AuthUser (..))
import           Nuts.Bolts.Config             (Config (..))
import           Nuts.Bolts.Models
import           Nuts.Bolts.Types
import           Nuts.Bolts.Utils

-- unprotected handlers
-- Here is the login handler
loginUser :: CookieSettings -> JWTSettings -> Login -> AppM JsonToken -- (Headers '[Header "Set-Cookie" SetCookie] NoContent))
loginUser _ jwtSettings (Login e p)
                        -- Usually you would ask a database for the account info. This is just a
                        -- regular servant handler, so you can follow your normal database access
                        -- patterns (including using 'enter').
 = do
  account <- runDb $ getBy $ UniqueEmail e
  case account of
    Nothing -> throwError err401 { errBody = jsonError "Login failed" }
    Just (Entity i a) -> do
      let valid = verifyPassword (encodeUtf8 p) (encodeUtf8 $ accountPassword a)
      if not valid then
        throwError err401 {errBody = jsonError "Login failed"}
      else do
          let act =
                AuthUser
                { uid = fromSqlKey i
                , account = a
                }
          -- mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
          -- case mcookie of
          --   Nothing     -> throwError err401
          --   Just cookie -> return $ addHeader cookie NoContent
          currentTime <- liftIO getCurrentTime
          -- expires in 24 hours from when it was issued
          let expiresIn = addUTCTime 86400 currentTime
          token <- liftIO $ makeJWT act jwtSettings (Just expiresIn)
          case token of
            Left _ -> throwError err401 { errBody = jsonError "Login failed." }
            Right t ->
              return
              JsonToken
              { token = decodeUtf8 . BSL.toStrict $ t
              }


-- Ping Handlers
ping :: AuthResult AuthUser -> AppM NoContent
ping (Authenticated au) = return NoContent
ping _                  = throwError err401

-- echo Handlers
hello :: CookieSettings -> JWTSettings -> AppM (Html ())
hello _ _ = return html
  where html :: Html ()
        html = do
          h2_ "Hi."

-- Account handlers

-- TODO: This should be an admin only handler
listAccounts :: AuthResult AuthUser -> AppM [Account]
listAccounts (Authenticated _) = do
  accounts :: [Entity Account] <- runDb $ selectList [] []
  let accs = map (\(Entity _ y) -> y) accounts
  return accs
listAccounts _ = throwError err401

getAccount :: AuthResult AuthUser -> AppM Account
getAccount (Authenticated au) = do
  account <- runDb $ get (toSqlKey $ uid au)
  case account of
    Nothing -> throwError err404
    Just a  -> return a
getAccount _ = throwError err401

registerNewAccount :: CookieSettings -> JWTSettings -> Maybe String -> Account -> AppM Int64
registerNewAccount _ _ code account = do
    existing <- runDb $ getBy $ UniqueEmail $ accountEmail account
    case existing of
      Nothing -> do
        hpass <- liftIO $ flip makePassword 17 . encodeUtf8 . accountPassword $ account
        let a =
              account
              { accountPassword = (decodeUtf8 hpass)
              }
        newAccountId <- runDb $ insert a
        return $ fromSqlKey newAccountId
      Just _ ->
        throwError
          (err400
           { errBody = jsonError "Email already exists."
           })

updateAccount :: AuthResult AuthUser -> Account -> AppM Account
updateAccount (Authenticated au) a = do
  runDb $
    update (toSqlKey $ uid au) $
    [AccountFirstName =. accountFirstName a, AccountLastName =. accountLastName a]
  return a
updateAccount _ _ = throwError err401

updateAccountPassword :: AuthResult AuthUser -> Password -> AppM NoContent
updateAccountPassword (Authenticated au) (Password p) = do
  pass <- liftIO $ flip makePassword 17 $ encodeUtf8 p
  -- FIXME: decodeUtf8 throws exceptions. Switch to safer version
  runDb $ update (toSqlKey $ uid au) $ [AccountPassword =. decodeUtf8 pass]
  return NoContent
updateAccountPassword _ _ = throwError err401

deleteAccount :: AuthResult AuthUser -> Password -> AppM NoContent
deleteAccount (Authenticated au) (Password p) = do
  account <- runDb $ getBy (UniqueEmail $ accountEmail $ account au)
  maybe (throwError err401) deleteAccount' account
  return NoContent
  where
    deleteAccount' (Entity i' a) = do
      let valid = verifyPassword (encodeUtf8 p) (encodeUtf8 $ accountPassword a)
      if valid then runDb $ delete i' else throwError err401
deleteAccount _ _ = throwError err401
