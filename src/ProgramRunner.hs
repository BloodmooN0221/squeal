module ProgramRunner (process) where

import qualified Data.Text as T
import Data.Either (either)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)
import Types (Breach(..))

process :: [String] -> IO String
process args = do let commands = getCommand args
                  either handleErrors runCommand commands

handleErrors :: CommandError -> IO String
handleErrors NoCommandsSupplied        = pure $ printf "No commands supplied.\n%s" usage
handleErrors (UnknownCommand commands) = pure $ printf "Unknown command supplied: %s\n%s" (intercalate " " commands) usage
handleErrors InvalidEmail              = pure $ printf "Invalid email supplied.\n%s" usage
handleErrors EmptyPassword             = pure $ printf "Empty password supplied.\n%s" usage

runCommand :: Command -> IO String
runCommand (LookUpBreachSites breach)        =
  do accountsE <- callBreachesService breach
     let (Breach email) = breach
     pure $ either breachErrorToString (listBreaches email) accountsE

runCommand (LookupPasswordHash passwordHash) =
  do passResultE <- callPasswordHashService passwordHash
     pure $ either passwordHashErrorToString (T.unpack . printPasswordHashStolen) passResultE

