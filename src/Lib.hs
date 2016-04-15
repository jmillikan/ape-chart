{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( runApp
    ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Arrow
import Data.Int (Int64)
import System.Environment

import Database.Persist hiding (get)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist.Sqlite as Sqlite
import Network.HTTP.Types.Status
import Web.Spock.Safe

import Db

runApp :: IO ()
runApp = do

  Sqlite.runSqlite "prices.db" $ Sqlite.runMigration migrateAll

  -- Go back over this in... "The future"
  runNoLoggingT $ Sqlite.withSqlitePool "prices.db" 10 $ \pool -> do
    let withDb f = runSqlPersistMPool f pool

    NoLoggingT $ runSpock 8080 $ spockT id $ do

      return ()
