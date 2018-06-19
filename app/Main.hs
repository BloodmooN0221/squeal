module Main where

import qualified Data.Text as T
import Data.Either (either)
import System.Environment (getArgs)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)

main :: IO ()
main = do args <- getArgs
          let commands = getCommand args
          result <- either handleErrors runCommand commands
          putStrLn result

handleErrors :: CommandError -> IO String
handleErrors NoCommandsSupplied        = pure $ printf "No commands supplied.\n%s" usage
handleErrors (UnknownCommand commands) = pure $ printf "Unknown command supplied: %s\n%s" (intercalate " " commands) usage
handleErrors InvalidEmail              = pure $ printf "Invalid email supplied.\n%s" usage
handleErrors EmptyPassword             = pure $ printf "Empty password supplied.\n%s" usage

runCommand :: Command -> IO String
runCommand (LookUpBreachSites breach)        =
  do maybeAccounts <- callBreachesService breach
     pure $ T.unpack $ maybe couldNotFindBreachedAccounts listBreaches maybeAccounts

runCommand (LookupPasswordHash passwordHash) =
  do passResult <- callPasswordHashService passwordHash
     (pure . T.unpack . printPasswordHash) passResult