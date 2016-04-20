{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Aeson (Value(..), object, (.=))

import Web.Spock.Shared

import Lib (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (spockAsApp $ app undefined) $ do
  describe "GET /" $ do
    it "responds with 404 because broken" $ do
      get "/" `shouldRespondWith` 404
