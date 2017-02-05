{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Authentication (getJWK, getClaimSub, getString, bcryptHash, makeJWT, makeClaims, validateToken, checkPassword) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)

import Control.Lens

import Data.String (fromString)
import Data.Text (Text)
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

import Database.Persist.Sql (fromSqlKey)

import Db

bcryptHash password = hashPasswordUsingPolicy slowerBcryptHashingPolicy (DTE.encodeUtf8 password)

getClaimSub :: ClaimsSet -> Maybe StringOrURI
getClaimSub = view claimSub

-- Security - needs to go elsewhere
getJWK :: IO (Maybe JWK)
getJWK = decode <$> LS.readFile "app-guide.jwk"

makeJWT
  :: (Control.Monad.Error.Class.MonadError Error m,
      Crypto.Random.Types.MonadRandom m) =>
     JWK -> Key User -> m BS.ByteString
makeJWT jwk userKey = do
  alg <- bestJWSAlg jwk
  let header = newJWSHeader (Protected, alg)
  jwt <- createJWSJWT jwk header (makeClaims $ userKey) >>= encodeCompact
  return $ LS.toStrict jwt

makeClaims :: Key User -> ClaimsSet
makeClaims userId = emptyClaimsSet
  & claimIss .~ Just (fromString "https://localhost/")
  & claimSub .~ Just (fromString $ show $ fromSqlKey userId)
  -- & claimExp .~ intDate "2011-03-22 18:43:00"
  -- & over unregisteredClaims (insert "http://example.com/is_root" (Bool True))
  -- & addClaim "http://example.com/is_root" (Bool True)

validateToken :: JWK -> Text -> ExceptT JWTError IO ClaimsSet
validateToken jwk token = do
  jwtData <- decodeCompact (encodeUtf8 $ fromStrict token)
  validateJWSJWT defaultJWTValidationSettings jwk jwtData
  return $ jwtClaimsSet jwtData

checkPassword :: Text -> Text -> Bool
checkPassword hash p = validatePassword (DTE.encodeUtf8 hash) (DTE.encodeUtf8 p)
-- End security stuff


