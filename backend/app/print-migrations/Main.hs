module Main where

import Database.Persist.Postgresql (runSqlPool)
import System.Environment (getArgs, lookupEnv)

import Nuts.Bolts.Config
  ( Config(..)
  , Environment(..)
  , makePool
  )
import Nuts.Bolts.Models (printMigrations)
import Nuts.Bolts.Utils (lookupSetting)

main :: IO ()
main = do
  env <- lookupSetting "ENV" Development
  pool <- makePool env
  runSqlPool printMigrations pool
