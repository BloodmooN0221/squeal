module ProgramRunner (process) where

import qualified Data.Text as T
import Data.Either (either)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)

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
     pure $ either breachErrorToString listBreaches accountsE

runCommand (LookupPasswordHash passwordHash) =
  do passResult <- callPasswordHashService passwordHash
     (pure . T.unpack . printPasswordHash) passResult
