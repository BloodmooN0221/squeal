{-# LANGUAGE OverloadedStrings #-}

module Lib ( callBreachesService, callPasswordHashService ) where

import Network.HTTP.Client (httpLbs, newManager, parseRequest, queryString, responseBody, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Monoid as M
import Data.Aeson (decode, FromJSON)
import Types

userAgent :: (CI B.ByteString, B.ByteString)
userAgent = ("User-Agent", "squeal")

truncateResponse :: B.ByteString
truncateResponse = C8.pack "truncateResponse=true"

-- callService :: FromJSON a => Service -> IO (Maybe a)
-- callService (Breach email) = decode <$> (getBreaches email)
-- callService (PasswordHash hash) = pure Nothing

callBreachesService :: FromJSON a => Breach -> IO (Maybe a)
callBreachesService (Breach email) = decode <$> (getBreaches email)

callPasswordHashService :: PasswordHash -> IO Bool
callPasswordHashService (PasswordHash hash) =
  do content <- L8.toStrict <$> getPasswordHashes hash
     let hashedLines = C8.split '\n' content
     let onlyHashes  = C8.takeWhile (/= ':') <$> hashedLines
     let hashSuffix  = suffix hash
     return $ any (== hashSuffix) onlyHashes

getBreaches :: Email -> IO (L8.ByteString)
getBreaches (Email email) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseRequest $ "https://haveibeenpwned.com/api/v2/breachedaccount/" M.<> (T.unpack email)
     let request = r1 { requestHeaders = [userAgent], queryString = truncateResponse }
     response <- httpLbs request manager
     return (responseBody response)

getPasswordHashes :: Hash -> IO (L8.ByteString)
getPasswordHashes (Hash hashPrefix _) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseRequest $ "https://api.pwnedpasswords.com/range/" M.<> (C8.unpack hashPrefix)
     let request = r1 { requestHeaders = [userAgent]}
     response <- httpLbs request manager
     return (responseBody response)
