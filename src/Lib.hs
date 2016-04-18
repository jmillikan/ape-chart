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

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as Sqlite
import Network.HTTP.Types.Status
import Web.Spock.Safe
import Database.Esqueleto hiding (get)

import Db

runApp :: IO ()
runApp = do

  Sqlite.runSqlite "main.db" $ Sqlite.runMigration migrateAll

  -- Go back over this in... "The future"
  runNoLoggingT $ Sqlite.withSqlitePool "main.db" 10 $ \pool -> do
    let withDb f = runSqlPersistMPool f pool

    NoLoggingT $ runSpock 8080 $ spockT id $ do
      get ("state" <//> var <//> "process" <//> var) $ \(stateId :: Int64) (processId :: Int64) -> do
        (state, cps) <- liftIO $ withDb (getProcessState (toSqlKey stateId) (toSqlKey processId))

        json (state, cps)

getProcessState :: StateId -> ProcessId -> SqlPersistM (Entity State, [(Entity Command, Entity CommandProcess)])
getProcessState stateId processId = do
  [state] <- select $ from $ \state -> where_ (state ^. StateId ==. val stateId) >> return state
  procCommands <- select $ from $ \(c `InnerJoin` cp) -> do
    on (c ^. CommandId ==. cp ^. CommandProcessCommandId)
    where_ (cp ^. CommandProcessProcessId ==. val processId)
    return (c, cp)
  return (state, procCommands)
  
