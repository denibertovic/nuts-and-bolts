{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnException,
    setPort,
  )
import System.Environment (getArgs, lookupEnv, getEnvironment)

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Exception.Extra (retry)
import Data.Aeson (decode)

import Control.Concurrent (forkIO)
import Crypto.JOSE.JWK (JWK)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Servant.Auth.Server

import Nuts.Bolts.Api (app)
import Nuts.Bolts.Config (Config(..), Environment(..), mkConfig, setLogger)
import Nuts.Bolts.Models
import Nuts.Bolts.Utils (lookupSetting)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Launching..." >> inner
    ["check"] -> putStrLn "Run successful"
    _ -> error "Invalid arguments, use either nothing or the word check"

inner :: IO ()
inner = do
  env <- getEnvironment
  config <- mkConfig env
  keyC <- BSL.readFile (jwkPath config)
  let myKey = decode keyC :: Maybe JWK
  let jwtCfg = defaultJWTSettings (fromJust myKey)
      logger = setLogger (environment config)
  retry 5 $ retryDbConnection $ runSqlPool doMigrations (connPool config)
  let settings = setPort (appPort config) $
                 defaultSettings
  runSettings settings $ logger $ app config defaultCookieSettings jwtCfg

retryDbConnection :: IO a -> IO a
retryDbConnection action =
  catch action $ \(_ :: SomeException) -> do
    putStrLn "Failed to connect to db. Retrying..."
    threadDelay 5000000
    action
