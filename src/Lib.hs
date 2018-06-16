{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

data BreachedAccount = BreachedAccount { name :: !T.Text } deriving Show
data BreachedAccounts = BreachedAccounts { accounts :: [BreachedAccount] } deriving Show

instance FromJSON BreachedAccount where
  parseJSON (Object v)  = BreachedAccount <$> v .: "Name"
  parseJSON other = typeMismatch "BreachedAccount" other

instance FromJSON BreachedAccounts where
  parseJSON v = BreachedAccounts <$> (parseJSONList v)

userAgent :: (CI B.ByteString, B.ByteString)
userAgent = ("User-Agent", "squeal")

someFunc :: IO ()
someFunc = do manager  <- newManager tlsManagerSettings
              r1      <- parseRequest "https://haveibeenpwned.com/api/v2/breachedaccount/sanjsmailbox@gmail.com"
              let request = r1 { requestHeaders = [userAgent]}
              response <- httpLbs request manager
              let response2 = responseBody response
                  breachedAccounts = decode response2 :: Maybe BreachedAccounts
                  result = maybe (T.pack "No breaches") (T.intercalate (T.pack ",") . fmap name . accounts) breachedAccounts
              print result
