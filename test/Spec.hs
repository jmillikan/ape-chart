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
  describe "Basically runs" $ do
    it "responds with 200" $ do
      get "/state/1/process/1" `shouldRespondWith` 200

  -- Scripty tests are NOT ideal, but better than nothing
  -- test.db starts with 3 states, 5 commands, 1 process, and *should* auto-increment cleanly

  describe "Adding and retreiving states" $ do
    it "succeeds in adding a state" $ do
      postHtmlForm "/state"
        [ ("appId", "1")
        , ("name", "Complete rotation")
        , ("description", "Set parameters for rotation of selected objects")
        ] `shouldRespondWith` 200

    it "finds the resulting state" $ do
      get "/state/4/process/1" `shouldRespondWith` 200

    it "Should not find fake states" $ do
      get "/state/5/process/1" `shouldRespondWith` 404

  describe "Adding and retrieving commands in a process" $ do
    it "Adds a command" $ do
      postHtmlForm "/state/1/process/1/command" 
        [ ("methodType", "keyboard-emacs")
        , ("method", "r")
        , ("desc", "Rotate selection")
        , ("note", "In basic situations, you will frequently need the axis modifiers x/y/z.")
        , ("resulteStateId", "4")
        ] `shouldRespondWith` 200

    it "Finds the command" $ do
      get "/command/6" `shouldRespondWith` 200

    it "Should not find fake commands" $ do
      get "/command/7" `shouldRespondWith` 404
