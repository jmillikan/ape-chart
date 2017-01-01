{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( runApp, app
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, void, liftM)
import Control.Monad.Logger
import Data.Int (Int64)
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import qualified Data.List as L
import qualified Data.Function as F
import qualified Data.Maybe as DM

-- These four identifiers are the subject of collisions *and* confusion...
import qualified Database.Persist as P (get, update, insert, delete, selectList, (==.)) 
import qualified Database.Esqueleto as E (select)
import Database.Persist ((=.))
import Database.Esqueleto hiding (update, get, Value, (=.), select, delete, groupBy)
import qualified Database.Persist.Sqlite as Sqlite
import Web.Spock.Safe
import qualified Network.Wai.Middleware.Static as M
import Network.HTTP.Types.Status

import Data.Pool (Pool)
import Network.Wai (Middleware)

import Db

-- P.get returns State, E.select returns Entity State.
-- Trying not to intermix them for clarity

runApp :: String -> Int -> IO ()
runApp dbFilename portNum = do
  Sqlite.runSqlite (pack dbFilename) $ Sqlite.runMigration migrateAll

  -- Go back over setup stuff in... "The future"
  runNoLoggingT $ Sqlite.withSqlitePool (pack dbFilename) 10 $ \pool ->
    NoLoggingT $ runSpock portNum $ app pool

app :: Pool SqlBackend -> IO Network.Wai.Middleware
app pool = spockT id $ do
  let withDb f = liftIO $ runSqlPersistMPool f pool
  
  c <- liftIO $ M.initCaching M.NoCaching

  middleware $ M.staticPolicy' c $ M.addBase "frontend"

  get root $ file "text/html" "frontend/index.html"

  get ("app") $ do
    apps :: [Entity App] <- withDb $ P.selectList [] []
    json apps
  
  get ("app" <//> var) $ \appId -> do
    app :: Maybe App <- withDb $ P.get (toSqlKey appId)
    json app

  delete ("app" <//> var) $ \appId -> do
    withDb $ P.delete (toSqlKey appId :: Key App)
    json appId

  -- Some of these are shaped like REST endpoints but really aren't
  get ("state" <//> var) $ \stateId -> do
    state <- withDb $ getProcessState (toSqlKey stateId)
    maybe (setStatus notFound404) json state

  get ("app" <//> var <//> "state") $ \appId -> do
    states <- withDb $ P.selectList [StateAppId P.==. appId] []
    json states

  post ("app") $ do
    app <- App <$> param' "name" <*> param' "description"
    newApp <- withDb $ P.insert app
    json (newApp :: Key App)

  -- New command in a process, with optional result state
  post ("state" <//> var <//> "process" <//> var <//> "command") $ \stateId processId -> do
    Just s :: Maybe State <- withDb $ P.get $ toSqlKey $ stateId -- State exists?
                             
    note <- param' "note"

    resultStateId <- param' "resultStateId"
    getResultState <- case resultStateId of
      "" -> do
        newState <- State (stateAppId s) <$> param' "stateName" <*> param' "stateDesc" -- Is this lazy?
        return $ P.insert newState 
      n -> return $ return $ toSqlKey $ read $ resultStateId -- This is dumb
           
    command <- Command (toSqlKey stateId) <$> param' "methodType" <*> param' "method" <*> param' "desc" -- <*> stateId...
           
    commandId <- withDb $ do -- Transaction???
      resultStateKey <- getResultState
      cid <- P.insert $ command (Just resultStateKey)
      when (processId > 0) $ -- I hate sentinel values
        void $ P.insert $ CommandProcess (toSqlKey processId) cid note
      return cid
    json $ fromSqlKey commandId

  -- Add/delete "include state"
  -- TODO: Verify same app...
  post ("state" <//> var <//> "include_state" <//> var) $ \stateId includeId -> do
    i <- withDb $ insertUnique $ IncludeState (toSqlKey stateId) (toSqlKey includeId)
    json (stateId, includeId)

  delete ("state" <//> var <//> "include_state" <//> var) $ \stateId includeId -> do
    i <- withDb $ deleteBy $ UniqueIncludeState (toSqlKey stateId) (toSqlKey includeId)
    json (stateId, includeId)

  -- Delete command
  delete ("command" <//> var) $ \commandId -> do
    c <- withDb $ P.delete (toSqlKey commandId :: Key Command)
    json commandId

  -- Remove command from process
  delete ("command" <//> var <//> "process" <//> var) $ \commandId processId -> do
    cp <- withDb $ deleteBy $ UniqueCommandProcess (toSqlKey processId) (toSqlKey commandId)
    json (commandId, processId)

  get ("app" <//> var <//> "process") $ \appId -> do
    processes <- withDb $ E.select $ from $ \p -> do
      where_ $ p ^. ProcessAppId ==. val appId
      return p
    json processes

  -- Some blanket add & fetch endpoints
  -- For testing porpoises >_<
  get ("command" <//> var) $ \commandId -> do
    (command :: Maybe Command) <- withDb (P.get $ toSqlKey commandId)
    maybe (setStatus notFound404) json command

  get ("app" <//> var) $ \appId -> do
    (command :: Maybe App) <- withDb (P.get $ toSqlKey appId)
    maybe (setStatus notFound404) json command

  post ("state") $ do
    state <- State <$> param' "appId" <*> param' "name" <*> param' "description"
    stateId <- withDb $ P.insert state
    json $ fromSqlKey stateId

  post ("app") $ do
    app <- App <$> param' "name" <*> param' "description"
    appId <- withDb $ P.insert app
    json $ fromSqlKey appId

  post ("app" <//> var <//> "process") $ \appId -> do
    let appKey = toSqlKey appId
    process <- Process appKey <$> param' "name" <*> param' "description"
    (Just app) <- withDb $ P.get appKey
    processId <- withDb $ P.insert process
    json $ fromSqlKey processId

  -- Update command...
  post ("command" <//> var) $ \commandIdRaw -> do
    methodType <- param' "methodType"
    method <- param' "method"
    desc <- param' "desc"
    resultStateId <- param "resultStateId"

    withDb $ P.update (toSqlKey commandIdRaw)
      [ CommandResultStateId =. toSqlKey <$> read <$> resultStateId
      , CommandMethodType =. methodType
      , CommandMethod =. method
      , CommandDescription =. desc 
      ]

    json commandIdRaw

  -- Add or change command-in-process
  -- Don't think I've tested this yet... at all.
  post ("command" <//> var <//> "process" <//> var) $ \(commandId :: Int64) (processId :: Int64) -> do
    note <- param' "note"

    withDb $ do
      found <- E.select $ from $ \cp -> do
        where_ (cp ^. CommandProcessCommandId ==. val (toSqlKey commandId) &&. 
                cp ^. CommandProcessProcessId ==. val (toSqlKey processId))
        return cp

      case found of
        [cp] -> do
          P.update (entityKey cp) [ CommandProcessNotes =. note ]
          return ()
        [] -> do
          P.insert $ CommandProcess (toSqlKey processId) (toSqlKey commandId) note
          return ()

(cpCommandId, cpProcessId) = (CommandProcessCommandId, CommandProcessProcessId)
(isIncludedStateId, isStateId) = (IncludeStateIncludedStateId, IncludeStateStateId)

collapseChildren :: Eq a => [(a,Maybe b)] -> [(a,[b])]
collapseChildren joined = map extractParent $ L.groupBy (F.on (==) fst) joined
    where extractParent all@((p,_):_) = (p, DM.catMaybes $ map snd all)

getProcessState :: StateId -> SqlPersistM (Maybe StateForProcess)
getProcessState stateId = do
  s <- E.select $ from $ \state -> where_ (state ^. StateId ==. val stateId) >> return state
  case s of
    [] -> return Nothing
    [state] -> do
      procCommands <- E.select $ from $ \(c `LeftOuterJoin` cp) -> do
        on $ just (c ^. CommandId) ==. cp ?. cpCommandId
        where_ $ c ^. CommandStateId ==. val stateId
        return (c, cp)
      -- If this works, I'm never touching it again.
      includedCommands <- E.select $ from $ \(is `InnerJoin` c `LeftOuterJoin` cp) -> do
        on $ just (c ^. CommandId) ==. cp ?. cpCommandId
        on $ c ^. CommandStateId ==. is ^. isIncludedStateId
        where_ $ is ^. isStateId ==. val stateId
        return (c, cp)
      includedStates <- E.select $ from $ \(is `InnerJoin` s) -> do
        on (is ^. isIncludedStateId ==. s ^. StateId)
        where_ (is ^. isStateId ==. val stateId)
        return s
      return $ Just $ StateForProcess (state, includedStates, collapseChildren $ procCommands ++ includedCommands)
