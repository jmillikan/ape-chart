{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( runApp, app
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Int (Int64)
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import qualified Data.List as L
import qualified Data.Function as F
import qualified Data.Maybe as DM
import qualified Data.HashMap.Strict as HM (insert)

-- These four identifiers are the subject of collisions *and* confusion...
import qualified Database.Persist as P (get, update, insert, delete) 
import qualified Database.Esqueleto as E (select)
import Database.Persist ((=.))
import Database.Esqueleto hiding (update, get, Value, (=.), select, delete, groupBy)
import qualified Database.Persist.Sqlite as Sqlite
import Web.Spock.Safe
import qualified Network.Wai.Middleware.Static as M
import Network.HTTP.Types.Status

import Db

newtype StateForProcess = StateForProcess (Entity State, [Entity State], [(Entity Command, [Entity CommandProcess])])

instance ToJSON StateForProcess where
  toJSON (StateForProcess (state, includes, cps)) = 
    nest (nest (toJSON state) "commands" jsonCps) "includes" (toJSON includes)
    where jsonCps = toJSON $ map (\(c,cp) -> nest (toJSON c) "process" (toJSON cp)) cps

nest :: Value -> Text -> Value -> Value
nest (Object outerMap) name inner = Object $ HM.insert name inner outerMap
nest _ _ _ = error "Can only nest inside an object"

-- P.get returns State, E.select returns Entity State.
-- Trying not to intermix them for clarity

runApp :: String -> Int -> IO ()
runApp dbFilename portNum = do
  Sqlite.runSqlite (pack dbFilename) $ Sqlite.runMigration migrateAll

  -- Go back over setup stuff in... "The future"
  runNoLoggingT $ Sqlite.withSqlitePool (pack dbFilename) 10 $ \pool ->
    NoLoggingT $ runSpock portNum $ app pool
  
app pool = spockT id $ do
  let withDb f = liftIO $ runSqlPersistMPool f pool

  middleware $ M.staticPolicy $ M.addBase "frontend"

  get root $ file "text/html" "frontend/index.html"

  -- Some of these are shaped like REST endpoints but really aren't
  get ("state" <//> var) $ \stateId -> do
    state <- withDb $ getProcessState (toSqlKey stateId)
    maybe (setStatus notFound404) json state 

  get ("app" <//> var <//> "state") $ \appId -> do
    states <- withDb $ E.select $ from $ \s -> where_ (s ^. StateAppId ==. val appId) >> return s
    json states

  -- New command in a process, with optional result state
  post ("state" <//> var <//> "process" <//> var <//> "command") $ \stateId processId -> do
    methodType <- param' "methodType"
    method <- param' "method"
    desc <- param' "desc"
    note <- param' "note"
  
    -- If result state ID is blank, create new state with name and desc.
    resultStateId <- param' "resultStateId"
    resultStateName <- param' "stateName"
    resultStateDesc <- param' "stateDesc"
    commandId <- withDb $ do -- Transaction???
      (Just state) <- P.get $ toSqlKey $ stateId
      resultStateKey <- case resultStateId of
        "" -> P.insert $ State (stateAppId state) resultStateName resultStateDesc
        n -> return $ toSqlKey $ read $ resultStateId
      commandId <- P.insert $ Command (toSqlKey stateId) methodType method desc (Just resultStateKey)
      _ <- P.insert $ CommandProcess (toSqlKey processId) commandId note
      return commandId
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
    cp <- withDb $ deleteBy $ UniqueCommandProcess (toSqlKey commandId) (toSqlKey processId)
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
    appId <- param' "appId"
    name <- param' "name"
    description <- param' "description"
    stateId <- withDb $ P.insert $ State appId name description
    json $ fromSqlKey stateId

  post ("app") $ do
    name <- param' "name"
    description <- param' "description"
    appId <- withDb $ P.insert $ App name description
    json $ fromSqlKey appId

  post ("app" <//> var <//> "process") $ \appId -> do
    let appKey = toSqlKey appId
    name <- param' "name"
    description <- param' "description"
    (Just app) <- withDb $ P.get appKey
    processId <- withDb $ P.insert $ Process appKey name description
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
