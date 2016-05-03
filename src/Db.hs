{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses,
             EmptyDataDecls, FlexibleContexts, FlexibleInstances,
             GADTs, GeneralizedNewtypeDeriving #-}
module Db where

import Database.Persist.TH
import qualified Data.Text as T

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


