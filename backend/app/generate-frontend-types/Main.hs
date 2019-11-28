{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Database.Persist.Postgresql (runSqlPool)
import GHC.Generics (Generic)
import Prelude
import System.Environment (getArgs, lookupEnv)
import Data.Proxy (Proxy(..))

import qualified Data.Text as T
import Language.PureScript.Bridge

main :: IO ()
main = do
  writePSTypes "/tmp/" (buildBridge defaultBridge) myTypes

data Foo = Foo
  { foo :: T.Text
  } deriving (Eq, Generic)

data Bar
  = A
  | B
  | C
  deriving (Eq, Ord, Generic)

data Baz =
  Baz String
  deriving (Generic)

-- | All types will have a `Generic` instance produced in Purescript.
myTypes :: [SumType 'Haskell]
myTypes =
  [ let p = (Proxy :: Proxy Foo)
     in equal p (mkSumType p) -- Also produce a `Eq` instance.
  , let p = (Proxy :: Proxy Bar)
     in order p (mkSumType p) -- Produce both `Eq` and `Ord`.
  , mkSumType (Proxy :: Proxy Baz) -- Just produce a `Generic` instance.
  ]
