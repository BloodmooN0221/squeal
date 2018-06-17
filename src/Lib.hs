{-# LANGUAGE OverloadedStrings #-}

module Lib ( callService ) where

import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Monoid as M
import Data.Aeson (decode, FromJSON)
import Types
import SquealJson


userAgent :: (CI B.ByteString, B.ByteString)
userAgent = ("User-Agent", "squeal")

callService :: FromJSON a => Service -> IO (Maybe a)
callService (Breach email) = decode <$> (getBreaches email)
callService (PasswordHash hash) = pure Nothing

getBreaches :: Email -> IO (L8.ByteString)
getBreaches (Email email) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseRequest $ "https://haveibeenpwned.com/api/v2/breachedaccount/" M.<> (T.unpack email)
     let request = r1 { requestHeaders = [userAgent]}
     response <- httpLbs request manager
     return (responseBody response)
