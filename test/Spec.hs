{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Control.Monad.Logger
import System.Directory

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Database.Persist.Sqlite as Sqlite
import Data.Aeson (Value(..), object, (.=))

import Web.Spock (spockAsApp)
import Web.Spock.Config

import Lib (app)

main :: IO ()
main = do
  copyFile "example.db" "test.db"
  -- Ideally this would recreate the DB each request
  runNoLoggingT $ Sqlite.withSqlitePool "test.db" 10 $ \pool -> 
    NoLoggingT $ hspec $ spec (spockAsApp $ app pool)

spec wai = with wai $ do
  -- Scripty tests are not a spec and not ideal, but better than nothing
  -- test.db starts with 3 states, 5 commands, 1 process, and *should* auto-increment cleanly

  describe "States" $ do
    it "can be added" $ do
      postHtmlForm "/state"
        [ ("appId", "1")
        , ("name", "Complete rotation")
        , ("description", "Set parameters for rotation of selected objects")
        ] `shouldRespondWith` 200

    it "can be found once added" $ get "/state/4" `shouldRespondWith` 200

    it "404 if they don't exist" $ get "/state/5" `shouldRespondWith` 404

  describe "Commands" $ do
    it "can be added in a process" $ do
      postHtmlForm "/state/1/process/1/command" 
        [ ("methodType", "keyboard-emacs")
        , ("method", "r")
        , ("desc", "Rotate selection")
        , ("note", "In basic situations, you will frequently need the axis modifiers x/y/z.")
        , ("resultStateId", "4")
        ] `shouldRespondWith` 200

    it "can be found once added" $ get "/command/6" `shouldRespondWith` 200

    it "can be seen in the source state (BRITTLE)" $ get "/state/1" `shouldRespondWith` "{\"commands\":[{\"methodType\":\"keyboard-emacs\",\"resultStateId\":2,\"process\":[{\"processId\":1,\"commandId\":1,\"id\":1,\"notes\":\"Switch modes\"}],\"method\":\"TAB\",\"stateId\":1,\"id\":1,\"description\":\"Switch to Edit Mode\"},{\"methodType\":\"keyboard-emacs\",\"resultStateId\":3,\"process\":[{\"processId\":1,\"commandId\":2,\"id\":2,\"notes\":\"For positioning objects\"}],\"method\":\"g\",\"stateId\":1,\"id\":2,\"description\":\"Grab\"},{\"methodType\":\"keyboard-emacs\",\"resultStateId\":4,\"process\":[{\"processId\":1,\"commandId\":6,\"id\":4,\"notes\":\"In basic situations, you will frequently need the axis modifiers x/y/z.\"}],\"method\":\"r\",\"stateId\":1,\"id\":6,\"description\":\"Rotate selection\"}],\"appId\":1,\"name\":\"Object Mode\",\"includes\":[],\"id\":1,\"description\":\"Object Mode with 3D View acive\"}"

    it "404 if they don't exist" $ get "/command/7" `shouldRespondWith` 404

  describe "Apps" $ do
    it "Can be added" $ do
      postHtmlForm "/app"
        [ ("name", "ASP.NET MVC")
        , ("description", "ASP.NET MVC Web Framework in the Rails style")
        ] `shouldRespondWith` 200

    it "Can be found once added" $ get "/app/2" `shouldRespondWith` 200
      
    it "404 if they don't exist" $ get "/app/3" `shouldRespondWith` 404

  describe "Process" $ do
    it "Can be added" $ do
      postHtmlForm "/app/1/process"
                   [("name", "UV Mapping")
                   ,("description", "Map points on mesh to points on ...")
                   ] `shouldRespondWith` 200

    it "Can be found once added (BRITTLE)" $ get "/app/1/process" `shouldRespondWith` "[{\"appId\":1,\"name\":\"Basic 3D\",\"id\":1,\"description\":\"Simple 3D Scene and model editing\"},{\"appId\":1,\"name\":\"UV Mapping\",\"id\":2,\"description\":\"Map points on mesh to points on ...\"}]"
      
