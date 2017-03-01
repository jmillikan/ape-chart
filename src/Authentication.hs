{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Do some crap to get around monad-time having IO and MonadTrans but not MonadIO
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Authentication (getJWK, getString, bcryptHash, makeJWT, makeClaims, validateToken, checkPassword, getJwtUser) where


import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT, withExceptT)
import Control.Monad.IO.Class
import Control.Monad.Time
import Data.Time.Clock

import Control.Lens

import Control.Error.Util ((!?), hoistEither)

import Data.String (fromString)
import Data.Text (Text, unpack, pack)
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Encoding as DTE
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString as BS

import Data.Aeson

import Crypto.Random.Types (MonadRandom)

import Crypto.JOSE.JWK (JWK, bestJWSAlg)
import Crypto.JOSE.Error (Error, AsError)
import Crypto.JWT
  ( JWTError
  , StringOrURI
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
import Crypto.JOSE.JWS (Protection(Protected), newJWSHeader)
import Crypto.JOSE.Compact (encodeCompact, decodeCompact)

import Crypto.BCrypt

import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Db

instance (Monad m, MonadIO m) => MonadTime m where
  currentTime = liftIO $ getCurrentTime

bcryptHash :: MonadIO m => Text -> ExceptT Text m BS.ByteString
bcryptHash password = liftIO (hashPasswordUsingPolicy slowerBcryptHashingPolicy (DTE.encodeUtf8 password))
                      !? "Password hashing error"

getJWK :: IO (Maybe JWK)
getJWK = decode <$> LS.readFile "app-guide.jwk"

withTextErrors :: (Monad m) => ExceptT JWTError m a -> ExceptT Text m a
withTextErrors = withExceptT (pack . show)

makeJWT :: (Crypto.Random.Types.MonadRandom m) =>
     JWK -> Key User -> ExceptT Text m BS.ByteString
makeJWT jwk userKey = withTextErrors $ do
  alg <- bestJWSAlg jwk
  let header = newJWSHeader (Protected, alg)
  jwt <- createJWSJWT jwk header (makeClaims $ userKey) >>= encodeCompact
  return $ LS.toStrict jwt

getJwtUser :: (MonadIO m) => JWK -> Text -> ExceptT Text m (Key User)
getJwtUser jwk token =  do
  claims <- withTextErrors $ validateToken jwk token
  sub <- return (view claimSub claims) !? "No sub in JWT"
  subString <- return (getString sub) !? "No string in sub"
  return $ toSqlKey $ read $ unpack subString

validateToken :: (MonadIO m) => JWK -> Text -> ExceptT JWTError m ClaimsSet
validateToken jwk token = do
  jwtData <- decodeCompact (encodeUtf8 $ fromStrict token)
  validateJWSJWT defaultJWTValidationSettings jwk jwtData
  return $ jwtClaimsSet jwtData

checkPassword :: Text -> Text -> Bool
checkPassword hash p = validatePassword (DTE.encodeUtf8 hash) (DTE.encodeUtf8 p)

makeClaims :: Key User -> ClaimsSet
makeClaims userId = emptyClaimsSet
  & claimIss .~ Just (fromString "https://localhost/")
  & claimSub .~ Just (fromString $ show $ fromSqlKey userId)

