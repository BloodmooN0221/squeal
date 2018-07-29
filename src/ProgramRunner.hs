module ProgramRunner (process) where

import qualified Data.Text as T
import Data.Either (either)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)
import Types
import Time (MicroSeconds(..), Seconds(..), secondsToMicro)
import Control.Exception (IOException, tryJust)
import Control.Applicative (liftA)
import Control.Concurrent (threadDelay)

process :: [String] -> IO String
process args = do let commands = getCommand args
                  either handleErrors runCommand commands

handleErrors :: CommandError -> IO String
handleErrors NoCommandsSupplied          = pure $ printf "No commands supplied.\n%s" usage
handleErrors (UnknownCommand commands)   = pure $ printf "Unknown command supplied: %s\n%s" (intercalate " " commands) usage
handleErrors InvalidEmail                = pure $ printf "Invalid email supplied.\n%s" usage
handleErrors EmptyPassword               = pure $ printf "Empty password supplied.\n%s" usage
handleErrors (InvalidEmailFile (FileReadError reason)) = pure $ printf "Could not read file:%s" reason

runCommand :: Command -> IO String
runCommand (LookUpBreachSites breach)        =
  do accountsE <- callBreachesService breach
     let (Breach email) = breach
     pure $ either (handleBreachErrors email) (listBreaches email) accountsE

runCommand (LookupPasswordHash passwordHash) =
  do passResultE <- callPasswordHashService passwordHash
     pure $ either handlePasswordHashErrors printPasswordHashStolen passResultE

runCommand (LookUpBreachesFromFile fileName) =
  do emailsE <- readEmailFromFile fileName
     either (pure . show) runBatchEmailLookup2 emailsE

runEmail :: Email -> IO (Either BreachErrorWithEmail String)
runEmail email = do accountsE <- callBreachesService $ Breach email
                    pure $ either (\be -> Left $ BreachErrorWithEmail be email) (\ba -> Right $ listBreaches email ba) accountsE

-- TODO: May have to use tailRecM here
-- TODO: Move printing functions to Printer
runBatchEmailLookup2 :: [Email] -> IO String
runBatchEmailLookup2 [] = pure "done"
runBatchEmailLookup2 (email:rest) =
  do _ <- putStrLn $ lookingUpEmail email
     emailResultE <- runEmail email
     either (handleBreachError2 rest)
            (\result -> do _ <- putStrLn result
                           _ <- waitTos
                           runBatchEmailLookup2 rest
            ) emailResultE

handleBreachError2 :: [Email] -> BreachErrorWithEmail -> IO String
handleBreachError2 rest (BreachErrorWithEmail (BreachApiError (ApiCallError (NotFound _))) em) = emailNotFound
  where emailNotFound :: IO String
        emailNotFound =
          do _ <- putStrLn $ noBreaches em
             waitTos
             runBatchEmailLookup2 rest

handleBreachError2 _ (BreachErrorWithEmail otherError (Email em)) = pure $ printf "error retrieving email: %s due to %s" em (breachErrorToString otherError)

delay :: Seconds
delay = Seconds 2

waitTos :: IO ()
waitTos = do _ <- putStrLn $ waitingForDelay delay
             threadDelay $ (getMicro . secondsToMicro) delay

readEmailFromFile :: String -> IO (Either FileReadError [Email])
readEmailFromFile fileName = do contentsE <- tryJust handleFileErrors (readFile fileName)
                                let textEmailsE      = (T.lines . T.pack) <$> contentsE
                                    validTextEmailsE = (filter emailTextValidation) <$> textEmailsE
                                    emails = liftA (Email <$>) validTextEmailsE
                                return emails

handleFileErrors :: IOException -> Maybe FileReadError
handleFileErrors = Just . FileReadError . show