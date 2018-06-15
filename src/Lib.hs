module Lib
    ( someFunc
    ) where

{-# OverloadedStrings #-}

import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8


someFunc :: IO ()
someFunc = do manager  <- newManager tlsManagerSettings
              request  <- parseRequest "https://httpbin.org/get"
              response <- httpLbs request manager
              let body = responseBody response
              L8.putStrLn body
