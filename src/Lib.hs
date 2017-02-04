{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( runApp, app, getJWK, makeJWT
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (MonadError)
import Control.Monad (when, void)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT(..))
import Data.Int (Int64)
import Data.Aeson hiding (json)
import Data.Text (Text, pack, unpack, stripPrefix)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text as T (tail)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as DTE
import Data.String (fromString)
import qualified Data.List as L
import qualified Data.Function as F
import qualified Data.Maybe as DM
import qualified Data.ByteString.Lazy as LS

import Control.Lens (view, preview, set, (&), (.~))

import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import qualified Network.Wai.Middleware.Static as M

import Web.Spock
import Web.Spock.Config

import Crypto.JOSE.Error (Error, AsError)
import Crypto.JOSE.JWK (JWK, bestJWSAlg)
import Crypto.JOSE.JWS (Protection(Protected), newJWSHeader)
import Crypto.JOSE.Compact (encodeCompact, decodeCompact)
import Crypto.JWT
  ( JWTError
  , ClaimsSet
  , jwtClaimsSet
  , getString    
  , emptyClaimsSet
  , createJWSJWT
  , validateJWSJWT
  , defaultJWTValidationSettings
  , audiencePredicate
  , claimIss
  , claimSub
  )

import Crypto.Random.Types (MonadRandom)

import Crypto.BCrypt

import Control.Monad.Except (runExceptT)

-- These four identifiers are the subject of collisions *and* confusion...
import qualified Database.Persist as PE (get, update, insert, delete, deleteCascade, selectList, selectFirst, (==.)) 
import qualified Database.Esqueleto as E (select)
import Database.Persist ((=.))
import Database.Persist.Sql (fromSqlKey)
import Database.Esqueleto hiding (update, get, Value, (=.), select, delete, groupBy)
import Database.Persist.Sqlite (runSqlite, runMigration, withSqlitePool)

import Data.Pool (Pool)

import Db


-- Security - needs to go elsewhere
getJWK :: IO (Maybe JWK)
getJWK = decode <$> LS.readFile "app-guide.jwk"

makeJWT
  :: (Control.Monad.Error.Class.MonadError Error m,
      Crypto.Random.Types.MonadRandom m) =>
     JWK -> Key User -> m LS.ByteString
makeJWT jwk userKey = do
  alg <- bestJWSAlg jwk
  let header = newJWSHeader (Protected, alg)
  createJWSJWT jwk header (makeClaims $ userKey) >>= encodeCompact

makeClaims :: Key User -> ClaimsSet
makeClaims userId = emptyClaimsSet
  & claimIss .~ Just (fromString "https://localhost/")
  & claimSub .~ Just (fromString $ show $ fromSqlKey userId)
  -- & claimExp .~ intDate "2011-03-22 18:43:00"
  -- & over unregisteredClaims (insert "http://example.com/is_root" (Bool True))
  -- & addClaim "http://example.com/is_root" (Bool True)

validateToken jwk token = do
  jwtData <- decodeCompact (encodeUtf8 $ fromStrict token)
  validateJWSJWT defaultJWTValidationSettings jwk jwtData
  return $ jwtClaimsSet jwtData
-- End security stuff

-- PE.get returns State, E.select returns Entity State.
-- Trying not to intermix them for clarity

-- Main entry point
-- Needs some notion of configuration...
runApp :: FilePath -> Int -> IO ()
runApp dbFilename portNum = do
  runSqlite (pack dbFilename) $ runMigration migrateAll

  -- FOREIGN KEY pragma is OFF.
  -- For turning it ion, see https://github.com/yesodweb/yesod/wiki/Activate-foreign-key-checking-in-Sqlite
  -- 
  runNoLoggingT $ withSqlitePool (pack dbFilename) 1 $ \pool ->
    NoLoggingT $ runSpock portNum $ app pool

-- WAI application - bring your own pool
app :: Pool SqlBackend -> IO Network.Wai.Middleware
app pool = do
  mjwk <- getJWK
  jwk <- maybe (fail "Can't start application, no JWK") return mjwk

  cfg <- defaultSpockCfg () (PCPool pool) jwk
  spock cfg api

api :: SpockM SqlBackend () JWK ()
api = do
  c <- liftIO $ M.initCaching M.NoCaching

  middleware $ M.staticPolicy' c $ M.addBase "frontend"

  get root $ file "text/html" "frontend/index.html"

  unauthApi
  prehook authenticate authApi

authenticate :: ActionCtxT () (WebStateM SqlBackend () JWK) (Key User)
authenticate = do
  header <- maybe (fail "No authorization header") return =<< header "Authorization"
  token <- maybe (fail "Not bearer authorization") return (stripPrefix "Bearer " header)

  jwk <- getState
  claims <- either (\(_ :: JWTError) -> fail "Error decoding or validating JWT") return
          =<< runExceptT (validateToken jwk token)
  sub <- maybe (fail "No sub in JWT") return (view claimSub claims)
  subString <- maybe (fail "No string in sub") return (getString sub)
  return $ toSqlKey $ read $ unpack subString

unauthApi :: SpockCtxM () SqlBackend () JWK ()
unauthApi = do
  post ("jwt") $ do
    (uname :: Text) <- param' "username"
    (password :: Text) <- param' "password"

    let authFail = do
          setStatus status401
          text "Wrong username or password"

    mUser <- withDb $ PE.selectFirst [UserUsername PE.==. uname] []
    user <- maybe authFail return mUser

    let passHash = userPassword (entityVal user)

    when (not $ validatePassword (DTE.encodeUtf8 passHash) (DTE.encodeUtf8 password))
      authFail

    jwk <- getState
    result <- runExceptT $ makeJWT jwk (entityKey user)

    -- To lazy text via bytestring, I guess.
    jwt <- either (\(e :: Error) -> fail "Failure making JWT claims set") (return . decodeUtf8) result
    json jwt

  post ("user") $ do
    (uname :: Text) <- param' "username"
    (password :: Text) <- param' "password"

    users <- withDb $ PE.selectList [UserUsername PE.==. uname] []
    when (length users /= 0) $ fail "That username is not available."

    mhash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (DTE.encodeUtf8 password)
    h <- maybe (fail "Password hashing error") return mhash

    let user = User uname (DTE.decodeUtf8 h) -- Write a failing test for this...
    newUserId <- withDb $ PE.insert user
    
    json newUserId

getUserKey :: ActionCtxT (Key User) (WebStateM SqlBackend () JWK) (Key User)
getUserKey = getContext

-- Run a SQL action from pool
withDb f = runQuery (\conn -> runSqlPersistM f conn)

class AccessCheck a where
  accessCheck :: a -> Key User -> SqlPersistM Bool

-- Not only will these checks not be atomic with the actual action,
-- the pair checks aren't even atomic with each other
  
instance (AccessCheck a, AccessCheck b) => AccessCheck (a,b) where
  accessCheck (keyA, keyB) userKey =
    (&&) <$> accessCheck keyA userKey <*> accessCheck keyB userKey

instance AccessCheck (Key App) where
  accessCheck appKey userKey = do
    apps <- E.select $ from $ \((a :: SqlExpr (Entity App)) `InnerJoin` (aa :: SqlExpr (Entity AppAccess))) -> do
      on $ (a ^. AppId) ==. (aa ^. AppAccessAppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. a ^. AppId ==. val appKey
      return (a ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key State) where
  accessCheck stateKey userKey = do
    apps <- E.select $ from $ \(st `InnerJoin` app `InnerJoin` aa) -> do
      on $ (app ^. AppId) ==. (aa ^. AppAccessAppId)
      on $ (st ^. StateAppId) ==. (app ^. AppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. st ^. StateId ==. val stateKey
      return (app ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key Process) where
  accessCheck processKey userKey = do
    apps <- E.select $ from $ \(pr `InnerJoin` app `InnerJoin` aa) -> do
      on $ (app ^. AppId) ==. (aa ^. AppAccessAppId)
      on $ (pr ^. ProcessAppId) ==. (app ^. AppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. pr ^. ProcessId ==. val processKey
      return (app ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key Command) where
  accessCheck processKey userKey = do
    apps <- E.select $ from $ \(st `InnerJoin` com `InnerJoin` app `InnerJoin` aa) -> do
      on $ (app ^. AppId) ==. (aa ^. AppAccessAppId)
      on $ (com ^. CommandStateId) ==. (st ^. StateId)
      on $ (st ^. StateAppId) ==. (app ^. AppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. com ^. CommandId ==. val processKey
      return (app ^. AppId)

    return $ length apps /= 0

appAccess :: AccessCheck a => a -> ActionCtxT (Key User) (WebStateM SqlBackend () JWK) ()
     -> ActionCtxT (Key User) (WebStateM SqlBackend () JWK) ()
appAccess appKey f = do
  userKey <- getUserKey

  access <- withDb $ accessCheck appKey userKey
  
  if access
    then f
    else setStatus status403

authApi :: SpockCtxM (Key User) SqlBackend () JWK ()
authApi = do
  post ("app") $ do
    app <- App <$> param' "name" <*> param' "description"
    userKey <- getContext
    newApp <- withDb $ do
      newApp <- PE.insert app
      PE.insert $ AppAccess userKey newApp
      return newApp
    json newApp

  get ("app" <//> var) $ \(appKey :: Key App) -> do
    appAccess appKey $ do
      json =<< withDb (PE.get appKey)

  get ("app") $ json =<< withDb . getUserApps =<< getUserKey

  delete ("app" <//> var) $ \(appKey :: Key App) ->
    appAccess appKey $ do
      -- TODO: Delete orphaned rows...
      withDb $ PE.deleteCascade appKey
      json appKey

  get ("app" <//> var <//> "state") $ \(appKey :: Key App) -> do
    appAccess appKey $ do
      states <- withDb $ PE.selectList [StateAppId PE.==. appKey] []
      json states

  post ("app" <//> var <//> "state") $ \(appKey :: Key App) -> do
    appAccess appKey $ do
      state <- State appKey <$> param' "name" <*> param' "description"
      stateId <- withDb $ PE.insert state
      json $ fromSqlKey stateId

  get ("state" <//> var) $ \(stateKey :: Key State) -> do
    appAccess stateKey $ do
      state <- withDb $ getProcessState stateKey
      maybe (setStatus notFound404) json state

  get ("app" <//> var <//> "process") $ \(appKey :: Key App) -> do
    appAccess appKey $ do
      processes <- withDb $ E.select $ from $ \p -> do
        where_ $ p ^. ProcessAppId ==. val appKey
        return p
      json processes

  post ("app" <//> var <//> "process") $ \(appKey :: Key App) -> do
    appAccess appKey $ do
      process <- Process appKey <$> param' "name" <*> param' "description"
      (Just app) <- withDb $ PE.get appKey
      processId <- withDb $ PE.insert process
      json $ fromSqlKey processId

  -- New command in a process, with optional result state
  post ("state" <//> var <//> "process" <//> var <//> "command") $ \stateId (processId :: Key Process) -> do
    appAccess (stateId, processId) $ do
      Just s :: Maybe State <- withDb $ PE.get stateId -- State exists?
    
      note <- param' "note"

      resultStateId <- param' "resultStateId"
      getResultState <- case resultStateId of
        "" -> do
          newState <- State (stateAppId s) <$> param' "stateName" <*> param' "stateDesc" -- Is this lazy?
          return $ PE.insert newState 
        n -> return $ return $ toSqlKey $ read resultStateId -- This is dumb
           
      command <- Command stateId <$> param' "methodType" <*> param' "method" <*> param' "desc" -- <*> stateId...
           
      commandId <- withDb $ do -- Transaction???
        resultStateKey <- getResultState
        --liftIO $ putStrLn $ show $ command (Just resultStateKey)
        cid <- PE.insert $ command (Just resultStateKey)
        when (fromSqlKey processId > 0) $ do -- This needs to be designed back out by splitting the endpoint...
          --liftIO $ putStrLn $ show $ CommandProcess processId cid note
          liftIO $ putStrLn $ "New command ID: " ++ show (fromSqlKey cid)
          liftIO $ putStrLn $ "Process ID for command: " ++ show (fromSqlKey processId)
          void $ PE.insert $ CommandProcess processId cid note
        return cid
      json $ fromSqlKey commandId

  -- Add/delete "include state"
  -- TODO: Verify same app...
  post ("state" <//> var <//> "include_state" <//> var) $ \stateId includeId -> do
    appAccess (stateId, includeId) $ do
      i <- withDb $ insertUnique $ IncludeState stateId includeId
      json (stateId, includeId)

  delete ("state" <//> var <//> "include_state" <//> var) $ \stateId includeId -> do
    appAccess (stateId, includeId) $ do
      i <- withDb $ deleteBy $ UniqueIncludeState stateId includeId
      json (stateId, includeId)

  -- Delete command
  delete ("command" <//> var) $ \(commandId :: Key Command) -> do
    appAccess commandId $ do
      -- Delete command and commandprocess
      c <- withDb $ PE.deleteCascade commandId 
      json commandId

  -- Remove command from process
  delete ("command" <//> var <//> "process" <//> var) $ \commandId processId -> do
    appAccess (commandId, processId) $ do
      cp <- withDb $ deleteBy $ UniqueCommandProcess processId commandId
      json (commandId, processId)

  -- These endpoints are unused from the frontend

  -- Add or change command-in-process
  -- Tested
  post ("command" <//> var <//> "process" <//> var) $ \commandId processId -> do
    appAccess (commandId, processId) $ do
      note <- param' "note"

      withDb $ do
        found <- E.select $ from $ \cp -> do
          where_ (cp ^. CommandProcessCommandId ==. val commandId &&. 
                  cp ^. CommandProcessProcessId ==. val processId)
          return cp

        case found of
          [cp] -> do
            PE.update (entityKey cp) [ CommandProcessNotes =. note ]
            return ()
          [] -> do
            PE.insert $ CommandProcess processId commandId note
            return ()

  -- For testing porpoises >_<
  get ("command" <//> var) $ \commandId -> do
    appAccess commandId $ do
      (command :: Maybe Command) <- withDb (PE.get commandId)
      -- appAccess should 403 away non-existent rows used for authorization...
      maybe (fail "Non-existent command - shouldn't happen") json command

  -- Update command...
  -- post ("command" <//> var) $ \commandIdRaw -> do
  --   methodType <- param' "methodType"
  --   method <- param' "method"
  --   desc <- param' "desc"
  --   resultStateId <- param "resultStateId"

  --   withDb $ PE.update (toSqlKey commandIdRaw)
  --     [ CommandResultStateId =. toSqlKey <$> read <$> resultStateId
  --     , CommandMethodType =. methodType
  --     , CommandMethod =. method
  --     , CommandDescription =. desc 
  --     ]

  --   json commandIdRaw

(cpCommandId, cpProcessId) = (CommandProcessCommandId, CommandProcessProcessId)
(isIncludedStateId, isStateId) = (IncludeStateIncludedStateId, IncludeStateStateId)

collapseChildren :: Eq a => [(a,Maybe b)] -> [(a,[b])]
collapseChildren joined = map extractParent $ L.groupBy (F.on (==) fst) joined
    where extractParent all@((p,_):_) = (p, DM.catMaybes $ map snd all)

getUserApps :: Key User -> SqlPersistM [Entity App]
getUserApps userKey =
  E.select $ from $ \((a :: SqlExpr (Entity App)) `InnerJoin` (aa :: SqlExpr (Entity AppAccess))) -> do
        on $ (a ^. AppId) ==. (aa ^. AppAccessAppId)
        where_ $ aa ^. AppAccessUserId ==. val userKey
        return a

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
