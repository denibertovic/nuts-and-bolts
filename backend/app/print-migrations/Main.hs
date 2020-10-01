module Main where

import Database.Persist.Postgresql (runSqlPool)
import System.Environment (getArgs, lookupEnv, getEnvironment)

import Nuts.Bolts.Config
  ( Config(..)
  , Environment(..)
  , mkConfig
  )
import Nuts.Bolts.Models (printMigrations)
import Nuts.Bolts.Utils (lookupSetting)

main :: IO ()
main = do
  env <- getEnvironment
  config <- mkConfig env
  runSqlPool printMigrations (connPool config)
