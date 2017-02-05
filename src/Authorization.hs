{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Authorization (AccessCheck, accessCheck) where

import Database.Esqueleto

import Db

class AccessCheck a where
  accessCheck :: a -> Key User -> SqlPersistM Bool

-- Not only will these checks not be atomic with the actual action,
-- the pair checks aren't even atomic with each other
  
instance (AccessCheck a, AccessCheck b) => AccessCheck (a,b) where
  accessCheck (keyA, keyB) userKey =
    (&&) <$> accessCheck keyA userKey <*> accessCheck keyB userKey

instance AccessCheck (Key App) where
  accessCheck appKey userKey = do
    apps <- select $ from $ \((a :: SqlExpr (Entity App)) `InnerJoin` (aa :: SqlExpr (Entity AppAccess))) -> do
      on $ (a ^. AppId) ==. (aa ^. AppAccessAppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. a ^. AppId ==. val appKey
      return (a ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key State) where
  accessCheck stateKey userKey = do
    apps <- select $ from $ \(st `InnerJoin` app `InnerJoin` aa) -> do
      on $ (app ^. AppId) ==. (aa ^. AppAccessAppId)
      on $ (st ^. StateAppId) ==. (app ^. AppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. st ^. StateId ==. val stateKey
      return (app ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key Process) where
  accessCheck processKey userKey = do
    apps <- select $ from $ \(pr `InnerJoin` app `InnerJoin` aa) -> do
      on $ (app ^. AppId) ==. (aa ^. AppAccessAppId)
      on $ (pr ^. ProcessAppId) ==. (app ^. AppId)
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. pr ^. ProcessId ==. val processKey
      return (app ^. AppId)

    return $ length apps /= 0

instance AccessCheck (Key Command) where
  accessCheck processKey userKey = do
    apps <- select $ from $ \(st `InnerJoin` com `InnerJoin` app `InnerJoin` aa) -> do
      on $ app ^. AppId ==. aa ^. AppAccessAppId
      on $ com ^. CommandStateId ==. st ^. StateId
      on $ st ^. StateAppId ==. app ^. AppId
      where_ $ aa ^. AppAccessUserId ==. val userKey &&. com ^. CommandId ==. val processKey
      return (app ^. AppId)

    return $ length apps /= 0

