{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Control.Monad.Logger
import System.Directory

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Database.Persist.Sqlite as Sqlite
import Data.Aeson (Value(..), object, (.=))

import Web.Spock.Shared

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

    it "can be found once added" $ get "/state/4/process/1" `shouldRespondWith` 200

    it "404 if they don't exist" $ get "/state/5/process/1" `shouldRespondWith` 404

  describe "Commands" $ do
    it "can be added in a process" $ do
      postHtmlForm "/state/1/process/1/command" 
        [ ("methodType", "keyboard-emacs")
        , ("method", "r")
        , ("desc", "Rotate selection")
        , ("note", "In basic situations, you will frequently need the axis modifiers x/y/z.")
        , ("resulteStateId", "4")
        ] `shouldRespondWith` 200

    it "can be found once added" $ get "/command/6" `shouldRespondWith` 200

    it "404 if they don't exist" $ get "/command/7" `shouldRespondWith` 404

  describe "Apps" $ do
    it "Can be added" $ do
      postHtmlForm "/app"
        [ ("name", "ASP.NET MVC")
        , ("description", "ASP.NET MVC Web Framework in the Rails style")
        ] `shouldRespondWith` 200

    it "Can be found once added" $ get "/app/2" `shouldRespondWith` 200
      
    it "404 if they don't exist" $ get "/app/3" `shouldRespondWith` 404
