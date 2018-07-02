module ProgramArgs (getCommand, CommandError(..), Command(..), FileReadError(..), emailTextValidation) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Types
import Hasher (sha1)

data Command = LookUpBreachSites Breach
             | LookUpBreachesFromFile String
             | LookupPasswordHash PasswordHash deriving Show

data CommandError = NoCommandsSupplied
                  | UnknownCommand [String]
                  | InvalidEmail
                  | InvalidEmailFile FileReadError
                  | EmptyPassword
                  deriving Show

data FileReadError = FileReadError String deriving Show

getCommand :: [String] -> Either CommandError Command
getCommand [] = Left NoCommandsSupplied
getCommand ("-e" : email : [])     =
  boolToEither (emailValidation email) InvalidEmail (LookUpBreachSites $ parseBreach email)
getCommand ("-ef" : fileName : []) = Right $ LookUpBreachesFromFile fileName
getCommand ("-p" : password : [])  =
  boolToEither (passwordValidation password) EmptyPassword (LookupPasswordHash $ parsePasswordHash password)
getCommand other = Left $ UnknownCommand other

parseBreach :: String -> Breach
parseBreach email = Breach $ Email $ T.pack email

parsePasswordHash :: String -> PasswordHash
parsePasswordHash password = let hashed = sha1 (C8.pack password)
                                 (hashedPrefix, hashedSuffix) = B.splitAt 5 hashed
                             in PasswordHash $ Hash { prefix = hashedPrefix, suffix = hashedSuffix }

emailValidation :: String -> Bool
emailValidation []    = False
emailValidation email = length email > 5 &&  any (== '@') email && any (== '.') email

emailTextValidation :: T.Text -> Bool
emailTextValidation email = T.length email > 5 &&  T.any (== '@') email && T.any (== '.') email

passwordValidation :: String -> Bool
passwordValidation [] = False
passwordValidation _  = True

boolToEither :: Bool -> a -> b -> Either a b
boolToEither bool left right = if bool then Right right else Left left
