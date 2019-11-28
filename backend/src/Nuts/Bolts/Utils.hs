module Nuts.Bolts.Utils where

import           Prelude
import           Data.Aeson           (encode)

import qualified Data.ByteString.Lazy as BSL
import           System.Environment          (lookupEnv)

import           Nuts.Bolts.Types

jsonError :: String -> BSL.ByteString
jsonError e = encode $ JsonError e

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
