module ProgramRunner (process) where

import qualified Data.Text as T
import Data.Either (either)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)
import Types (Breach(..), Email(..), BreachErrorWithEmail(..))
import Control.Exception (IOException, tryJust)
import Control.Applicative (liftA)

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

-- runCommand (LookUpBreachesFromFile fileName) =
--   do emailsE <- readEmailFromFile fileName
--      let breachLooksE = liftA (LookUpBreachSites . Breach <$>) emailsE
--          results = either ((\x -> [x]) . pure . show) ( ((++ "\n") <$>) . runCommand <$>) breachLooksE
--      mconcat results

runCommand (LookUpBreachesFromFile fileName) =
  do emailsE <- readEmailFromFile fileName
     either (pure . show) runBatchEmailLookup emailsE

runEmail :: Email -> IO (Either BreachErrorWithEmail String)
runEmail email = do accountsE <- callBreachesService $ Breach email
                    pure $ either (\be -> Left $ BreachErrorWithEmail be email) (\ba -> Right $ listBreaches email ba) accountsE

runBatchEmailLookup :: [Email] -> IO String
runBatchEmailLookup emails = let listOfResults = runAllEmails emails
                                 resultsIOE = (sequence <$>) $ sequence listOfResults -- IO (Either BreachErrorWithEmail [String])
                              in (either (\(BreachErrorWithEmail e email) -> handleBreachErrors email e) (intercalate "\n")) <$> resultsIOE
                              where runAllEmails :: [Email] -> [IO (Either BreachErrorWithEmail String)]
                                    runAllEmails = (runEmail <$>)


readEmailFromFile :: String -> IO (Either FileReadError [Email])
readEmailFromFile fileName = do contentsE <- tryJust handleFileErrors (readFile fileName)
                                let textEmailsE      = (T.lines . T.pack) <$> contentsE
                                    validTextEmailsE = (filter emailTextValidation) <$> textEmailsE
                                    emails = liftA (Email <$>) validTextEmailsE
                                return emails

handleFileErrors :: IOException -> Maybe FileReadError
handleFileErrors = Just . FileReadError . show