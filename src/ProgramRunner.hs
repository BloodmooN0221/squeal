module ProgramRunner (process) where

import qualified Data.Text as T
import Data.Either (either)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)
import Types
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
  do _ <- putStrLn $ printf "Looking up email: %s" (getEmail email)
     emailResultE <- runEmail email
     either handleBreachError2
            (\result -> do _ <- putStrLn result
                           _ <- waitTos
                           runBatchEmailLookup2 rest
            ) emailResultE
    where handleBreachError2 :: BreachErrorWithEmail -> IO String
          handleBreachError2 (BreachErrorWithEmail (BreachApiError (ApiCallError (NotFound _))) (Email em)) = (putStrLn $ printf "No breaches for email: %s" em) >> waitTos >> (runBatchEmailLookup2 rest)
          handleBreachError2 (BreachErrorWithEmail otherError (Email em)) = pure $ printf "error retrieving email: %s due to %s" em (breachErrorToString otherError)

          waitTos :: IO ()
          waitTos = do _ <- putStrLn $ printf "waiting for %d seconds" (getSeconds delay)
                       threadDelay $ (getMicro . secondsToMicro) delay


newtype Seconds = Seconds { getSeconds :: Int } deriving Show

newtype MicroSeconds = MicroSeconds { getMicro :: Int } deriving Show

delay :: Seconds
delay = Seconds 2

secondsToMicro :: Seconds -> MicroSeconds
secondsToMicro seconds = MicroSeconds $ (getSeconds seconds) * 1000000

readEmailFromFile :: String -> IO (Either FileReadError [Email])
readEmailFromFile fileName = do contentsE <- tryJust handleFileErrors (readFile fileName)
                                let textEmailsE      = (T.lines . T.pack) <$> contentsE
                                    validTextEmailsE = (filter emailTextValidation) <$> textEmailsE
                                    emails = liftA (Email <$>) validTextEmailsE
                                return emails

handleFileErrors :: IOException -> Maybe FileReadError
handleFileErrors = Just . FileReadError . show