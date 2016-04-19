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
import Data.Aeson hiding (json)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM (insert)
import System.Environment

import Database.Persist (update, (=.))
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as Sqlite
import Network.HTTP.Types.Status
import Web.Spock.Safe
import Database.Esqueleto hiding (update, get, Value, (=.))

import Db

newtype StateForProcess = StateForProcess (Entity State, [(Entity Command, Maybe (Entity CommandProcess))])

instance ToJSON StateForProcess where
  toJSON (StateForProcess (state, cps)) = nest (toJSON state) "commands" jsonCps
    where jsonCps = toJSON $ map (\(c,cp) -> nest (toJSON c) "process" (toJSON cp)) cps

nest :: Value -> Text -> Value -> Value
nest (Object outerMap) name inner = Object $ HM.insert name inner outerMap
nest _ _ _ = error "Can only nest inside an object"

runApp :: IO ()
runApp = do

  Sqlite.runSqlite "main.db" $ Sqlite.runMigration migrateAll

  -- Go back over this in... "The future"
  runNoLoggingT $ Sqlite.withSqlitePool "main.db" 10 $ \pool -> do
    let withDb f = liftIO $ runSqlPersistMPool f pool

    NoLoggingT $ runSpock 8080 $ spockT id $ do

      -- These are shaped like REST endpoints but really, really aren't
      get ("state" <//> var <//> "process" <//> var) $ \stateId processId -> do
        state <- withDb $ getProcessState (toSqlKey stateId) (toSqlKey processId)

        json (StateForProcess state)

      post ("state" <//> var <//> "process" <//> var <//> "command") $ \stateId processId -> do
        (Just methodType) <- param "methodType"
        (Just method) <- param "method"
        (Just desc) <- param "desc"
        (Just note) <- param "note"
        resultStateId <- param "resultStateId"
        withDb $ do -- Transaction???
          commandId <- insert $ Command (toSqlKey stateId) methodType method desc (toSqlKey <$> read <$> resultStateId) 
          cpId <- insert $ CommandProcess (toSqlKey processId) commandId note
          return ()
        return ()

      post ("command" <//> var) $ \commandIdRaw -> do
        (Just methodType) <- param "methodType"
        (Just method) <- param "method"
        (Just desc) <- param "desc"
        resultStateId <- param "resultStateId"
         
        withDb $ update (toSqlKey commandIdRaw)
          [ CommandResultStateId =. (toSqlKey <$> read <$> resultStateId)
          , CommandMethodType =. methodType
          , CommandMethod =. method
          , CommandDescription =. desc 
          ]

        return ()

      -- Add a command to a process/edit a command-process
      post ("command" <//> var <//> "process" <//> var) $ \(commandId :: Int64) (processId :: Int64) -> do
        (Just note) <- param "note"

        withDb $ do
          found <- select $ from $ \cp -> do
            where_ (cp ^. CommandProcessCommandId ==. val (toSqlKey commandId) &&. 
                    cp ^. CommandProcessProcessId ==. val (toSqlKey processId))
            return cp

          case found of
            [cp] -> do
              update (entityKey cp) [ CommandProcessNotes =. note ]
              return ()
            [] -> do
              insert $ CommandProcess (toSqlKey processId) (toSqlKey commandId) note
              return ()

getProcessState :: StateId -> ProcessId -> SqlPersistM (Entity State, [(Entity Command, Maybe (Entity CommandProcess))])
getProcessState stateId processId = do
  [state] <- select $ from $ \state -> where_ (state ^. StateId ==. val stateId) >> return state
  procCommands <- select $ from $ \(c `LeftOuterJoin` cp) -> do
    on (just (c ^. CommandId) ==. cp ?. CommandProcessCommandId &&. cp ?. CommandProcessProcessId ==. just (val processId))
    where_ (c ^. CommandStateId ==. val stateId)
    return (c, cp)
  return (state, procCommands)

  
