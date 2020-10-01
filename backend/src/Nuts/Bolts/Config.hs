{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Nuts.Bolts.Config where

import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Network.Wai                          (Middleware)
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.IO                            (FilePath)
import qualified Data.HashMap.Strict as HM

import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)


data Config = Config
    { connPool :: ConnectionPool
    , environment  :: Environment
    , jwkPath  :: FilePath
    , appPort :: Int
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

mkConfig :: [(String, String)] -> IO Config
mkConfig env = do
  pool <- makePool getEnv dbUrl
  return $ Config { connPool = pool
             , environment = getEnv
             , jwkPath = T.unpack $ required "JWK_PATH" hm
             , appPort = requiredDefault 8000 "APP_PORT" hm
             }
    where hm = HM.fromList env
          getEnv = requiredDefault Development "ENV" hm
          dbUrl = TE.encodeUtf8 $ required "DATABASE_URL" hm

notRequired :: Text -> HM.HashMap String String -> Maybe Text
notRequired q env = T.pack <$> HM.lookup (T.unpack q) env

required :: Text -> HM.HashMap String String -> Text
required q env = case HM.lookup (T.unpack q) env of
  Nothing -> error $ "Error. Missing config environment variable: " <> (T.unpack q)
  Just x -> T.pack x

requiredDefault :: Read a => a -> Text -> HM.HashMap String String -> a
requiredDefault def q env = case HM.lookup (T.unpack q) env of
  Nothing -> def
  Just x -> read x

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

makePool :: Environment -> ConnectionString -> IO ConnectionPool
makePool e connStr = runStdoutLoggingT $ createPostgresqlPool connStr (envPool e)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8
