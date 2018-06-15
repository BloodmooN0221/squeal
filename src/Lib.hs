module Lib
    ( someFunc
    ) where

{-# OverloadedStrings #-}

import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody)
import qualified Data.ByteString.Lazy.Char8 as L8


someFunc :: IO ()
someFunc = do manager  <- newManager defaultManagerSettings
              request  <- parseRequest "http://httpbin.org/get"
              response <- httpLbs request manager
              let body = responseBody response
              L8.putStrLn body
