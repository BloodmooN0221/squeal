module Main where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Types
import Printer
import Lib (callBreachesService, callPasswordHashService)
import Hasher

main :: IO ()
main = do acc <- callBreachesService (Breach $ Email $ T.pack "sanjsmailbox@gmail.com") :: IO (Maybe BreachedAccounts)
          let result = maybe (T.pack "Could not find breached accounts") printBreaches acc
          _ <- (putStrLn . T.unpack) result
          let hash                     = sha1 (C8.pack "P@ssw0rd")
          let (hashPrefix, hashSuffix) = B.splitAt 5 hash
          passResult <- callPasswordHashService $ PasswordHash (Hash { prefix = hashPrefix, suffix = hashSuffix })
          (putStrLn . T.unpack . printPasswordHash) passResult
