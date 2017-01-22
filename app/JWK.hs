module Main where

import qualified Data.ByteString.Lazy as L

import Data.Aeson (encode)
import Crypto.JOSE.JWK (KeyMaterialGenParam(..), genJWK)

main :: IO ()
main = L.writeFile "app-guide.jwk" =<< encode <$> (genJWK $ OctGenParam 32)
