module ProgramRunner (process, readPassword) where

import qualified Data.Text as T
import Data.Either (either)
import Data.Maybe (maybe)
import Printer
import Lib (callBreachesService, callPasswordHashService)
import ProgramArgs
import Text.Printf (printf)
import Data.List (intercalate)
import Types
import Time (MilliSeconds(..), MicroSeconds(..), milliToMicro)
import HIBP (defaultDelay)
import Control.Exception (IOException, tryJust)
import Control.Applicative (liftA)
import Control.Concurrent (threadDelay)
import Paths_squeal (version)
import Data.Version (showVersion)
import Hasher (parsePasswordHash)
import System.IO (hSetEcho, stdin, stdout, hSetBuffering, BufferMode(NoBuffering))

process :: [String] -> IO String
process args = do let commands = getCommand args
                  either handleErrors runCommand commands

handleErrors :: CommandError -> IO String
handleErrors NoCommandsSupplied          = pure $ printf "%s" usage
handleErrors (UnknownCommand commands)   = pure $ printf "Unknown command supplied: %s\n%s" (intercalate " " commands) usage
handleErrors InvalidEmail                = pure $ printf "Invalid email supplied.\n%s" usage
handleErrors (InvalidEmailFile (FileReadError reason)) = pure $ printf "Could not read file:%s" reason

runCommand :: Command -> IO String
runCommand Version                           = pure $ showVersion version
runCommand (LookUpBreachSites breach)        =
  do accountsE <- callBreachesService breach
     let (Breach email) = breach
     pure $ either (handleBreachErrors email) (listBreaches email) accountsE

runCommand EnterPassword =
  do hSetBuffering stdin NoBuffering
     hSetBuffering stdout NoBuffering
     _        <- putStrLn "please enter your password: "
     password <- readPassword stars ""
     _ <- putStrLn ""
     result <- lookupPasswordHash (LookupPasswordHash $ parsePasswordHash password)
     _ <- putStrLn result
     _ <- putStrLn "press q quit or any other key to enter another password"
     response <- getCh
     if (response == 'q') then pure ""
     else (runCommand EnterPassword)

runCommand (LookUpBreachesFromFile fileName maybeDelay) =
  do emailsE <- readEmailFromFile fileName
     either (pure . show) (\email -> runBatchEmailLookup2 email maybeDelay) emailsE

stars :: Char -> Char
stars = \_ -> '*'

getCh :: IO Char
getCh =
  do hSetEcho stdin False
     x <- getChar
     hSetEcho stdin True
     return x

readPassword :: (Char -> Char) -> String -> IO String
readPassword f xs =
  do c <- getCh
     r <- if (c == '\n') then (pure xs)
          else putChar (f c) >> (readPassword f (xs ++ [c]))
     return r

lookupPasswordHash :: LookupPasswordHash -> IO String
lookupPasswordHash (LookupPasswordHash passwordHash) =
  do passResultE <- callPasswordHashService passwordHash
     pure $ either handlePasswordHashErrors printPasswordHashStolen passResultE

runEmail :: Email -> IO (Either BreachErrorWithEmail String)
runEmail email = do accountsE <- callBreachesService $ Breach email
                    pure $ either (\be -> Left $ BreachErrorWithEmail be email) (\ba -> Right $ listBreaches email ba) accountsE

-- TODO: May have to use tailRecM here
-- TODO: Move printing functions to Printer
runBatchEmailLookup2 :: [Email] -> (Maybe BreachesApiDelay) -> IO String
runBatchEmailLookup2 [] _ = pure "done"
runBatchEmailLookup2 (email:rest) maybeDelay =
  do _ <- putStrLn $ lookingUpEmail email
     emailResultE <- runEmail email
     either (handleBreachError2 rest maybeDelay)
            (\result -> do _ <- putStrLn result
                           _ <- waitTos maybeDelay
                           runBatchEmailLookup2 rest maybeDelay
            ) emailResultE

handleBreachError2 :: [Email] -> (Maybe BreachesApiDelay) -> BreachErrorWithEmail -> IO String
handleBreachError2 rest maybeDelay (BreachErrorWithEmail (BreachApiError (ApiCallError (NotFound _))) em) = emailNotFound
  where emailNotFound :: IO String
        emailNotFound =
          do _ <- putStrLn $ noBreaches em
             waitTos maybeDelay
             runBatchEmailLookup2 rest maybeDelay

handleBreachError2 _  _ (BreachErrorWithEmail otherError (Email em)) = pure $ printf "error retrieving email: %s due to %s" em (breachErrorToString otherError)

waitTos :: (Maybe BreachesApiDelay) -> IO ()
waitTos maybeDelay =
  do let apiDelay = maybe defaultDelay (\(BreachesApiDelay millis) -> millis) maybeDelay
     _ <- putStrLn $ waitingForDelay apiDelay
     threadDelay $ (getMicro . milliToMicro) apiDelay

readEmailFromFile :: String -> IO (Either FileReadError [Email])
readEmailFromFile fileName = do contentsE <- tryJust handleFileErrors (readFile fileName)
                                let textEmailsE      = (T.lines . T.pack) <$> contentsE
                                    validTextEmailsE = (filter emailTextValidation) <$> textEmailsE
                                    emails = liftA (Email <$>) validTextEmailsE
                                return emails

handleFileErrors :: IOException -> Maybe FileReadError
handleFileErrors = Just . FileReadError . show