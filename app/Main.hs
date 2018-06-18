module Main where

import qualified Data.Text as T
import Types
import Printer
import Lib (callBreachesService)
import SquealJson

main :: IO ()
main = do accounts <- callBreachesService (Breach $ Email $ T.pack "sanjsmailbox@gmail.com") :: IO (Maybe BreachedAccounts)
          let result = maybe (T.pack "Could not find breached accounts") printBreaches accounts
          (putStrLn . T.unpack) result
