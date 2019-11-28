{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Nuts.Bolts.Config where

import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.IO                            (FilePath)

import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)


data Config = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    , getJWK  :: Environment -> FilePath
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    , getJWK = defaultJWK
    }


defaultJWK :: Environment -> FilePath
defaultJWK Test        = "keys/jwk_dev_key.json"
defaultJWK Development = "keys/jwk_dev_key.json"
defaultJWK Production  = "keys/jwk_prod_key.json"

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (connStr Test) (envPool Test)
makePool e = runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

connStr :: Environment -> ConnectionString
connStr Test = "host=localhost dbname=postgres user=postgres password=postgres port=5432"
connStr Development = "host=postgres dbname=postgres user=postgres password=postgres port=5432"
connStr Production = "host=postgres dbname=postgres user=postgres password=postgres port=5432"
