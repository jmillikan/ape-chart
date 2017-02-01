{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Database.Persist as PE (get, update, insert, delete, selectList, selectFirst, (==.)) 
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
  :: (Control.Monad.Error.Class.MonadError e m,
      Crypto.JOSE.Error.AsError e,
      Crypto.Random.Types.MonadRandom m) =>
     JWK -> Key User -> m LS.ByteString
makeJWT jwk userKey = do
  alg <- bestJWSAlg jwk
  let header = newJWSHeader (Protected, alg)
  createJWSJWT jwk header (makeClaims $ userKey) >>= encodeCompact

validateToken jwk token = do
  jwtData <- decodeCompact (encodeUtf8 $ fromStrict token)
  validateJWSJWT defaultJWTValidationSettings jwk jwtData
  return $ view claimSub $ jwtClaimsSet jwtData
-- End security stuff

-- PE.get returns State, E.select returns Entity State.
-- Trying not to intermix them for clarity

-- Main entry point
-- Needs some notion of configuration...
runApp :: FilePath -> Int -> IO ()
runApp dbFilename portNum = do
  runSqlite (pack dbFilename) $ runMigration migrateAll

  runNoLoggingT $ withSqlitePool (pack dbFilename) 10 $ \pool ->
    NoLoggingT $ runSpock portNum $ app pool

withDb f = runQuery (\conn -> runSqlPersistM f conn)

-- WAI application - bring your own pool
app :: Pool SqlBackend -> IO Network.Wai.Middleware
app pool = do
  cfg <- defaultSpockCfg () (PCPool pool) ()
  spock cfg api

api :: SpockM SqlBackend () () ()
api = do
  c <- liftIO $ M.initCaching M.NoCaching

  -- Works on my machine, what's the big deal?
  mjwk <- liftIO getJWK
  jwk <- maybe (fail "Can't start application, no JWK") return mjwk

  middleware $ M.staticPolicy' c $ M.addBase "frontend"

  get root $ file "text/html" "frontend/index.html"

  unauthApi jwk
  prehook (authenticated jwk) authApi

authenticated :: JWK -> ActionCtxT () (WebStateM SqlBackend () ()) (Key User)
authenticated jwk = do
  auth <- header "Authorization"
  mtoken <- maybe (fail "No authorization header") (return . stripPrefix "Bearer ") auth
  token <- maybe (fail "Not bearer authorization") return mtoken

  jwtResult <- runExceptT $ validateToken jwk token
  msub <- either (\(_ :: JWTError) -> fail "Error decoding or validating JWT") return jwtResult
  sub <- maybe (fail "No sub in JWT") (return . getString) msub
  maybe (fail "No username in sub... Must have been a URL?") (return . toSqlKey . read . unpack) sub

unauthApi :: JWK -> SpockCtxM () SqlBackend () () ()
unauthApi jwk = do
  post ("jwt") $ do
    (uname :: Text) <- param' "username"
    (password :: Text) <- param' "password"

    let authFail = do
          setStatus status401
          text "Wrong username or password"

    mUser <- withDb $ PE.selectFirst [UserUsername PE.==. uname] []
    user <- maybe authFail return mUser

    let passHash = userPassword (entityVal user)

    when (not $ validatePassword (DTE.encodeUtf8 passHash) (DTE.encodeUtf8 password)) authFail

    result <- runExceptT $ makeJWT jwk (entityKey user)

    -- To lazy text via bytestring, I guess.
    either (\(e :: Error) -> fail "Failure making JWT claims set") (json . decodeUtf8) result

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

authApi :: SpockCtxM (Key User) SqlBackend () () ()
authApi = do
  get ("app") $ do
    apps :: [Entity App] <- withDb $ PE.selectList [] []
    json apps

  get ("app" <//> var) $ \appId -> do
    app :: Maybe App <- withDb $ PE.get (toSqlKey appId)
    json app

  delete ("app" <//> var) $ \appId -> do
    withDb $ PE.delete (toSqlKey appId :: Key App)
    json appId

  -- Some of these are shaped like REST endpoints but really aren't
  get ("state" <//> var) $ \stateId -> do
    state <- withDb $ getProcessState (toSqlKey stateId)
    maybe (setStatus notFound404) json state

  get ("app" <//> var <//> "state") $ \appId -> do
    states <- withDb $ PE.selectList [StateAppId PE.==. appId] []
    json states

  post ("app") $ do
    app <- App <$> param' "name" <*> param' "description"
    (userKey :: Key User) <- getContext
    newApp <- withDb $ do
              newApp <- PE.insert app
              PE.insert $ AppAccess userKey newApp
              return newApp
    json (newApp :: Key App)

  -- New command in a process, with optional result state
  post ("state" <//> var <//> "process" <//> var <//> "command") $ \stateId processId -> do
    Just s :: Maybe State <- withDb $ PE.get $ toSqlKey $ stateId -- State exists?
                             
    note <- param' "note"

    resultStateId <- param' "resultStateId"
    getResultState <- case resultStateId of
      "" -> do
        newState <- State (stateAppId s) <$> param' "stateName" <*> param' "stateDesc" -- Is this lazy?
        return $ PE.insert newState 
      n -> return $ return $ toSqlKey $ read $ resultStateId -- This is dumb
           
    command <- Command (toSqlKey stateId) <$> param' "methodType" <*> param' "method" <*> param' "desc" -- <*> stateId...
           
    commandId <- withDb $ do -- Transaction???
      resultStateKey <- getResultState
      cid <- PE.insert $ command (Just resultStateKey)
      when (processId > 0) $ -- I hate sentinel values
        void $ PE.insert $ CommandProcess (toSqlKey processId) cid note
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
    c <- withDb $ PE.delete (toSqlKey commandId :: Key Command)
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
    (command :: Maybe Command) <- withDb (PE.get $ toSqlKey commandId)
    maybe (setStatus notFound404) json command

  get ("app" <//> var) $ \appId -> do
    (command :: Maybe App) <- withDb (PE.get $ toSqlKey appId)
    maybe (setStatus notFound404) json command

  post ("state") $ do
    state <- State <$> param' "appId" <*> param' "name" <*> param' "description"
    stateId <- withDb $ PE.insert state
    json $ fromSqlKey stateId

  post ("app") $ do
    app <- App <$> param' "name" <*> param' "description"
    appId <- withDb $ PE.insert app
    json $ fromSqlKey appId

  post ("app" <//> var <//> "process") $ \appId -> do
    let appKey = toSqlKey appId
    process <- Process appKey <$> param' "name" <*> param' "description"
    (Just app) <- withDb $ PE.get appKey
    processId <- withDb $ PE.insert process
    json $ fromSqlKey processId

  -- Update command...
  post ("command" <//> var) $ \commandIdRaw -> do
    methodType <- param' "methodType"
    method <- param' "method"
    desc <- param' "desc"
    resultStateId <- param "resultStateId"

    withDb $ PE.update (toSqlKey commandIdRaw)
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
          PE.update (entityKey cp) [ CommandProcessNotes =. note ]
          return ()
        [] -> do
          PE.insert $ CommandProcess (toSqlKey processId) (toSqlKey commandId) note
          return ()

(cpCommandId, cpProcessId) = (CommandProcessCommandId, CommandProcessProcessId)
(isIncludedStateId, isStateId) = (IncludeStateIncludedStateId, IncludeStateStateId)

collapseChildren :: Eq a => [(a,Maybe b)] -> [(a,[b])]
collapseChildren joined = map extractParent $ L.groupBy (F.on (==) fst) joined
    where extractParent all@((p,_):_) = (p, DM.catMaybes $ map snd all)

makeClaims :: Key User -> ClaimsSet
makeClaims userId = emptyClaimsSet
  & claimIss .~ Just (fromString "https://localhost/")
  & claimSub .~ Just (fromString $ show $ fromSqlKey userId)
  -- & claimExp .~ intDate "2011-03-22 18:43:00"
  -- & over unregisteredClaims (insert "http://example.com/is_root" (Bool True))
  -- & addClaim "http://example.com/is_root" (Bool True)

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
