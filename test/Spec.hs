{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.ByteString.Lazy (toStrict, fromStrict)

import Control.Monad.Logger
import System.Directory

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
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
    NoLoggingT $ hspec $ spec (spockAsApp $ app pool) (toStrict jwt1) (toStrict jwt2)

formUrlEncodeQuery = foldr (\(k,v) s -> s <> "&" <> (fromStrict $ urlEncode False k) <> "=" <> (fromStrict $ urlEncode False v)) ""

postFormWithJWT path jwt form = request methodPost path [(hContentType, "application/x-www-form-urlencoded"), (hAuthorization, "Bearer " <> jwt)] (formUrlEncodeQuery form)

getWithJWT path jwt = request methodGet path [(hAuthorization, "Bearer " <> jwt)] ""

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
    it "can be added" $ do
      let a = do
              r <- postFormWithJWT "/state" userToken
                [ ("appId", "1")
                , ("name", "Complete rotation")
                , ("description", "Set parameters for rotation of selected objects")
                ]
              return r
      a `shouldRespondWith` 200

    it "can be found once added" $ getWithJWT "/state/4" userToken `shouldRespondWith` 200

    it "cannot be viewed by users without access" $ getWithJWT "/state/4" userToken2 `shouldRespondWith` 500

    it "404 if they don't exist" $ getWithJWT "/state/5" userToken `shouldRespondWith` 404

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

    it "cannot be viewed by users without access" $ getWithJWT "/command/6" userToken2 `shouldRespondWith` 500

    it "can be seen in the source state (BRITTLE)" $ getWithJWT "/state/1" userToken `shouldRespondWith` "{\"commands\":[{\"methodType\":\"keyboard-emacs\",\"resultStateId\":2,\"process\":[{\"processId\":1,\"commandId\":1,\"id\":1,\"notes\":\"Switch modes\"}],\"method\":\"TAB\",\"stateId\":1,\"id\":1,\"description\":\"Switch to Edit Mode\"},{\"methodType\":\"keyboard-emacs\",\"resultStateId\":3,\"process\":[{\"processId\":1,\"commandId\":2,\"id\":2,\"notes\":\"For positioning objects\"}],\"method\":\"g\",\"stateId\":1,\"id\":2,\"description\":\"Grab\"},{\"methodType\":\"keyboard-emacs\",\"resultStateId\":4,\"process\":[{\"processId\":1,\"commandId\":6,\"id\":4,\"notes\":\"In basic situations, you will frequently need the axis modifiers x/y/z.\"}],\"method\":\"r\",\"stateId\":1,\"id\":6,\"description\":\"Rotate selection\"}],\"appId\":1,\"name\":\"Object Mode\",\"includes\":[],\"id\":1,\"description\":\"Object Mode with 3D View acive\"}"

    it "404 if they don't exist" $ getWithJWT "/command/7" userToken `shouldRespondWith` 404

  describe "Apps" $ do
    it "Can be added" $ do
      postFormWithJWT "/app" userToken
        [ ("name", "ASP.NET MVC")
        , ("description", "ASP.NET MVC Web Framework in the Rails style")
        ] `shouldRespondWith` 200

    it "Can be found once added" $ getWithJWT "/app/2" userToken `shouldRespondWith` 200

    it "Cannot be viewed by users without access" $ getWithJWT "/app/2" userToken2 `shouldRespondWith` 500

    it "404 if they don't exist" $ getWithJWT "/app/3" userToken `shouldRespondWith` 404

  describe "Process" $ do
    it "Can be added" $ do
      postFormWithJWT "/app/1/process" userToken
                   [("name", "UV Mapping")
                   ,("description", "Map points on mesh to points on ...")
                   ] `shouldRespondWith` 200

    it "Can be found once added (BRITTLE)" $ getWithJWT "/app/1/process" userToken `shouldRespondWith` "[{\"appId\":1,\"name\":\"Basic 3D\",\"id\":1,\"description\":\"Simple 3D Scene and model editing\"},{\"appId\":1,\"name\":\"UV Mapping\",\"id\":2,\"description\":\"Map points on mesh to points on ...\"}]"

    it "Cannot be viewed by users without access" $ getWithJWT "/app/1/process" userToken2 `shouldRespondWith` 500
