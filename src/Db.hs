{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses,
             EmptyDataDecls, FlexibleContexts, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Db where

import qualified Data.HashMap.Strict as HM (insert)

import Database.Persist.TH
import Database.Persist
import qualified Data.Text as T
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
App json
  name T.Text
  description T.Text
  deriving Show
IncludeState json
  stateId StateId
  includedStateId StateId
  UniqueIncludeState stateId includedStateId
State json
  appId AppId
  name T.Text
  description T.Text
  deriving Show
-- Note json
--   stateId StateId
--   text T.Text
--   deriving Show
Command json
  stateId StateId
  methodType T.Text
  method T.Text
  description T.Text
  resultStateId StateId Maybe
  deriving Eq Show
Process json
  appId AppId -- Possible for DB to contain commands linked to process for wrong app.
  name T.Text
  description T.Text
  deriving Show
CommandProcess json
  processId ProcessId
  commandId CommandId
  notes T.Text
  UniqueCommandProcess processId commandId
-- StateProcess json
--   processId ProcessId
--   stateId StateId
--   notes T.Text
|]

newtype StateForProcess = StateForProcess (Entity State, [Entity State], [(Entity Command, [Entity CommandProcess])])

instance ToJSON StateForProcess where
  toJSON (StateForProcess (state, includes, cps)) = 
    nest (nest (toJSON state) "commands" jsonCps) "includes" (toJSON includes)
    where jsonCps = toJSON $ map (\(c,cp) -> nest (toJSON c) "process" (toJSON cp)) cps

nest :: Value -> T.Text -> Value -> Value
nest (Object outerMap) name inner = Object $ HM.insert name inner outerMap
nest _ _ _ = error "Can only nest inside an object"

