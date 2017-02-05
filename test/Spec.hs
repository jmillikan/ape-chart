{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.ByteString.Lazy (toStrict, fromStrict)

import Control.Monad.Logger
import System.Directory

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher (bodyEquals, ResponseMatcher)
import Test.Hspec.Wai.JSON (json)
import qualified Database.Persist.Sqlite as SQ
import Data.Aeson (Value(..), object, (.=))

import Web.Spock (spockAsApp)
import Web.Spock.Config

import Data.Monoid ((<>))

import Lib (app, getJWK, makeJWT)
import Db (migrateAll, User, Key)
import Database.Persist.Sql (toSqlKey)
import Crypto.JOSE.Error (Error)

import Control.Monad.Except (runExceptT)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI (urlEncode)

main :: IO ()
main = do
  copyFile "example.db" "test.db"

  SQ.runSqlite "test.db" $ SQ.runMigration migrateAll

  mjwk <- getJWK
  jwk <- maybe (fail "Can't start tests, no JWK") return mjwk

  -- Fudge a JWT for use with ID 1 and ID 2. This isn't very good.
  mjwt1 <- runExceptT $ makeJWT jwk (toSqlKey 1 :: Key User)
  jwt1 <- either (\(e :: Error) -> fail "Failure making JWT for test user") return mjwt1

  mjwt2 <- runExceptT $ makeJWT jwk (toSqlKey 2 :: Key User)
  jwt2 <- either (\(e :: Error) -> fail "Failure making JWT for test user") return mjwt2

  -- Ideally this would recreate the DB each request
  runNoLoggingT $ SQ.withSqlitePool "test.db" 10 $ \pool -> 
    NoLoggingT $ hspec $ spec (spockAsApp $ app pool) jwt1 jwt2

formUrlEncodeQuery = foldr (\(k,v) s -> s <> "&" <> (fromStrict $ urlEncode False k) <> "=" <> (fromStrict $ urlEncode False v)) ""

postFormWithJWT path jwt form = request methodPost path [(hContentType, "application/x-www-form-urlencoded"), (hAuthorization, "Bearer " <> jwt)] (formUrlEncodeQuery form)

getWithJWT path jwt = request methodGet path [(hAuthorization, "Bearer " <> jwt)] ""

deleteWithJWT path jwt = request methodDelete path [(hAuthorization, "Bearer " <> jwt)] ""

jsonBody = ResponseMatcher 200 [] . bodyEquals


-- De-scriptifying plan:
-- ???

-- Assume userToken is for "user" and userToken2 is for "johndoe"
spec wai userToken userToken2 = with wai $ do
  -- Scripty tests are not a spec but better than nothing
  -- test.db starts with 1 user, 3 states, 5 commands, 1 process, and *should* auto-increment cleanly
  
  describe "Users" $ do
    it "can be added" $ do
      postHtmlForm "/user"
        [ ("username", "johndoe")
        , ("password", "rosebud")
        ] `shouldRespondWith` 200

    it "can't get a token with wrong password" $ do
      postHtmlForm "/jwt"
        [ ("username", "johndoe")
        , ("password", "shibboleth")
        ] `shouldRespondWith` 401

    it "can get a token" $ do
      postHtmlForm "/jwt"
        [ ("username", "johndoe")
        , ("password", "rosebud")
        ] `shouldRespondWith` 200

  describe "States" $ do
    it "can be listed per app" $ getWithJWT "/app/1/state" userToken `shouldRespondWith` 200

    it "cannot be listed without app access" $ getWithJWT "/app/1/state" userToken2 `shouldRespondWith` 403
    
    it "can be added" $ postFormWithJWT "/app/1/state" userToken
      [ ("name", "Complete rotation")
      , ("description", "Set parameters for rotation of selected objects")
      ] `shouldRespondWith` 200

    it "can be found once added" $ getWithJWT "/state/4" userToken `shouldRespondWith` 200

    it "cannot be viewed by users without access" $ getWithJWT "/state/4" userToken2 `shouldRespondWith` 403

    it "403 if they don't exist" $ getWithJWT "/state/5" userToken `shouldRespondWith` 403

    it "cannot be added without app access" $ postFormWithJWT "/app/1/state" userToken2
      [ ("name", "Bad state")
      , ("description", "Bad state desc")
      ] `shouldRespondWith` 403

  describe "Commands" $ do
    it "can be added in a process" $ do
      postFormWithJWT "/state/1/process/1/command" userToken
        [ ("methodType", "keyboard-emacs")
        , ("method", "r")
        , ("desc", "Rotate selection")
        , ("note", "In basic situations, you will frequently need the axis modifiers x/y/z.")
        , ("resultStateId", "4")
        ] `shouldRespondWith` 200

    it "can be found once added" $ getWithJWT "/command/6" userToken `shouldRespondWith` 200

    it "cannot be viewed by users without access" $ getWithJWT "/command/6" userToken2 `shouldRespondWith` 403

    it "can be seen in the source state (BRITTLE)" $ getWithJWT "/state/1" userToken `shouldRespondWith` jsonBody [json|{"commands":[{"methodType":"keyboard-emacs","resultStateId":2,"process":[{"processId":1,"commandId":1,"id":1,"notes":"Switch modes"}],"method":"TAB","stateId":1,"id":1,"description":"Switch to Edit Mode"},{"methodType":"keyboard-emacs","resultStateId":3,"process":[{"processId":1,"commandId":2,"id":2,"notes":"For positioning objects"}],"method":"g","stateId":1,"id":2,"description":"Grab"},{"methodType":"keyboard-emacs","resultStateId":4,"process":[{"processId":1,"commandId":6,"id":4,"notes":"In basic situations, you will frequently need the axis modifiers x/y/z."}],"method":"r","stateId":1,"id":6,"description":"Rotate selection"}],"appId":1,"name":"Object Mode","includes":[],"id":1,"description":"Object Mode with 3D View acive"}|]

    it "403 if they don't exist" $ getWithJWT "/command/7" userToken `shouldRespondWith` 403

    it "cannot be added without app access" $ do
      postFormWithJWT "/state/1/process/1/command" userToken2
        [ ("methodType", "keyboard-emacs")
        , ("method", "f")
        , ("desc", "bad")
        , ("note", "...")
        , ("resultStateId", "4")
        ] `shouldRespondWith` 403

    it "can be deleted" $
      postFormWithJWT "/state/1/process/1/command" userToken
        [("methodType", "fake"), ("method", "f"), ("desc", "..."), ("note", "..."), ("resultStateId", "1")] >>
        deleteWithJWT "/command/7" userToken `shouldRespondWith` 200

    it "can still be added (TEMP)" $ postFormWithJWT "/state/1/process/1/command" userToken
        [("methodType", "fake"), ("method", "f"), ("desc", "..."), ("note", "..."), ("resultStateId", "2")] `shouldRespondWith` 200

    -- Scripty alert - re-uses an ID from above, assumptions about SQLITE id order on deletions...
    it "cannot be deleted without access" $
      --postFormWithJWT "/state/1/process/1/command" userToken
        --[("methodType", "fake"), ("method", "f"), ("desc", "..."), ("note", "..."), ("resultStateId", "2")] >>
        deleteWithJWT "/command/8" userToken2 `shouldRespondWith` 403

  describe "Apps" $ do
    it "can be listed"  $ getWithJWT "/app" userToken `shouldRespondWith` jsonBody [json|[{"name":"Blender 2.72","id":1,"description":"Full featured 3D modeling and animation program"}]|]

    it "Can be added" $ do
      postFormWithJWT "/app" userToken
        [ ("name", "ASP.NET MVC")
        , ("description", "ASP.NET MVC Web Framework in the Rails style")
        ] `shouldRespondWith` 200

    -- SCRIPTINESS ALERT
    -- Assume new app was added and has ID 3

    it "will not be listed for users without access"  $ getWithJWT "/app" userToken2 `shouldRespondWith` "[]"

    it "Can be found once added" $ getWithJWT "/app/2" userToken `shouldRespondWith` 200

    it "403 if they don't exist" $ getWithJWT "/app/3" userToken `shouldRespondWith` 403

    it "Cannot be viewed by users without access" $ getWithJWT "/app/2" userToken2 `shouldRespondWith` 403

    it "Cannot be deleted without access" $ deleteWithJWT "/app/2" userToken2 `shouldRespondWith` 403

    -- SCRIPTINESS ALERT
    -- Assume app was not just deleted by above

    it "can be deleted" $ deleteWithJWT "/app/2" userToken `shouldRespondWith` 200

    it "cannot be found once deleted" $ getWithJWT "/app/2" userToken `shouldRespondWith` 403

  describe "Process" $ do
    it "Can be added" $ do
      postFormWithJWT "/app/1/process" userToken
                   [("name", "UV Mapping")
                   ,("description", "Map points on mesh to points on ...")
                   ] `shouldRespondWith` 200

    it "Can be found once added (BRITTLE)" $ getWithJWT "/app/1/process" userToken `shouldRespondWith` jsonBody [json|[{"appId":1,"name":"Basic 3D","id":1,"description":"Simple 3D Scene and model editing"},{"appId":1,"name":"UV Mapping","id":2,"description":"Map points on mesh to points on ..."}]|]

    it "Cannot be viewed by users without access" $ getWithJWT "/app/1/process" userToken2 `shouldRespondWith` 403

    it "cannot be added without app access" $ do
      postFormWithJWT "/app/1/process" userToken2
                   [("name", "Bad process")
                   ,("description", "<...>")
                   ] `shouldRespondWith` 403

  -- More or less have to do this at end of script... This is getting hairy.
  -- These require a bunch of states to mess with...
  describe "Included states" $ do
    it "can be added" $ postFormWithJWT "/state/1/include_state/4" userToken [] `shouldRespondWith` 200

    it "cannot be added without access" $ postFormWithJWT "/state/2/include_state/4" userToken2 [] `shouldRespondWith` 403
  
    it "cannot be deleted without access" $ postFormWithJWT "/state/2/include_state/4" userToken2 [] >>
      postFormWithJWT "/state/2/include_state/4" userToken2 [] `shouldRespondWith` 403

    it "can be deleted" $ postFormWithJWT "/state/3/include_state/4" userToken [] >>
      postFormWithJWT "/state/3/include_state/4" userToken [] `shouldRespondWith` 200



  describe "Command in process" $ do
    it "can be added to process" $ postFormWithJWT "/command/1/process/2" userToken [("note", "...")] `shouldRespondWith` 200

    it "can be removed from process" $ deleteWithJWT "/command/1/process/2" userToken `shouldRespondWith` 200

    it "cannot be added without access" $ postFormWithJWT "/command/2/process/2" userToken2 [("note", "...")] `shouldRespondWith` 403

    it "cannot be removed without access" $
      postFormWithJWT "/command/3/process/2" userToken [("note", "...")] >>
      deleteWithJWT "/command/3/process/2" userToken2 `shouldRespondWith` 403
