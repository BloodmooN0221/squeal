module ProgramArgs (getCommand, CommandError(..), Command(..), FileReadError(..), emailTextValidation) where

import qualified Data.Text as T
import Data.Char (isDigit)
import Types
import Time (MilliSeconds(..))
import HIBP (defaultDelay)

data Command = Version
             | LookUpBreachSites Breach
             | LookUpBreachesFromFile String (Maybe BreachesApiDelay)
             | EnterPassword deriving Show

data CommandError = NoCommandsSupplied
                  | UnknownCommand [String]
                  | InvalidEmail
                  | InvalidEmailFile FileReadError
                  deriving Show

data FileReadError = FileReadError String deriving Show

getCommand :: [String] -> Either CommandError Command
getCommand [] = Left NoCommandsSupplied
getCommand ("-v": [])              = Right Version
getCommand ("-e" : email : [])     =
  boolToEither (emailValidation email) InvalidEmail (LookUpBreachSites $ parseBreach email)
getCommand ("-ef" : fileName : []) = Right $ LookUpBreachesFromFile fileName Nothing
getCommand ("-ef" : fileName : apiDelay : []) =
  Right $ LookUpBreachesFromFile fileName (toApiDelay apiDelay)
  where
    toApiDelay :: String -> Maybe BreachesApiDelay
    toApiDelay adelay =
      if (all isDigit adelay) then
        let value = (read adelay :: Int) in
        if (value > (getMillis defaultDelay)) then (Just $ BreachesApiDelay $ MilliSeconds value)
        else Nothing
      else Nothing

getCommand ("-p": [])  = Right $ EnterPassword
getCommand other = Left $ UnknownCommand other

parseBreach :: String -> Breach
parseBreach email = Breach $ Email $ T.pack email

emailValidation :: String -> Bool
emailValidation []    = False
emailValidation email = length email > 5 &&  any (== '@') email && any (== '.') email

emailTextValidation :: T.Text -> Bool
emailTextValidation email = T.length email > 5 &&  T.any (== '@') email && T.any (== '.') email

boolToEither :: Bool -> a -> b -> Either a b
boolToEither bool left right = if bool then Right right else Left left