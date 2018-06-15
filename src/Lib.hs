{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as B

userAgent :: (CI B.ByteString, B.ByteString)
userAgent = ("User-Agent", "squeal")

someFunc :: IO ()
someFunc = do manager  <- newManager tlsManagerSettings
              r1      <- parseRequest "https://haveibeenpwned.com/api/v2/breachedaccount/sanjsmailbox@gmail.com"
              let request = r1 { requestHeaders = [userAgent]}
              response <- httpLbs request manager
              let body = responseBody response
              L8.putStrLn body
