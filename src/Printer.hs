module Printer where

import qualified Data.Text as T
import Data.List (intercalate, maximumBy, replicate, concat)
import Data.Monoid
import Text.Printf (printf)
import Types (BreachedAccounts(..))
import Types (BreachedAccount)
import Types (BreachError(..))
import Types (Email(..))
import Types (EndpointCallError(..))
import Types (HttpError(..))
import Types (PasswordHashError(..))
import Types (name)
import Time (Seconds(..))
import Paths_squeal (version)
import Data.Version (showVersion)

newtype OptionName  = OptionName String deriving Show

newtype Description = Description String deriving Show

newtype FirstDescription = FirstDescription String deriving Show

data Option = Option OptionName FirstDescription [Description]

options :: [Option]
options = [Option (OptionName "-v") (FirstDescription "Version information") [],
           Option (OptionName "-e  <email>") (FirstDescription "Verifies whether the supplied <email> address has been pwned") [],
           Option (OptionName "-ef <email_address_file>") (FirstDescription "Verifies multiple email addresses, specified in the") [
                                                           Description "<email_address_file> have been pwned.",
                                                           Description "<email_address_file> should have one email address per line"],
           Option (OptionName "-p") (FirstDescription "Verifies whether a password has been stolen") []
          ]

optionName :: Option -> String
optionName (Option (OptionName opName) _ _) = opName

optionNameMaxLength :: [Option] -> Int
optionNameMaxLength ops = maximumBy compare $ (length . optionName) <$> ops

getPadding :: Int -> String
getPadding n = replicate n ' '

betweenLength :: Int
betweenLength = 4

optionMaxNamePadding :: [Option] -> String
optionMaxNamePadding = getPadding . optionNameMaxLength

betweenPadding :: String
betweenPadding = getPadding $ betweenLength

usage :: T.Text
usage = let descPadding = (optionMaxNamePadding options) <> betweenPadding

            optionNameLength = length

            optionNameLengthDiff ops opName = (optionNameMaxLength ops) - (optionNameLength opName)

            optionNameLengthDiffPadding ops opName = getPadding (optionNameLengthDiff ops opName)

            descriptions :: [Description] -> String
            descriptions []    = ""
            descriptions descs = concat $
                                  (\(Description desc) ->
                                    "\n"        <>
                                    descPadding <>
                                    desc
                                  ) <$> descs

            optionOutput = concat $
                            (\(Option (OptionName opName) (FirstDescription fd) descs) ->
                              "\n"                                         <>
                              opName                                       <>
                              (optionNameLengthDiffPadding options opName) <>
                              betweenPadding                               <>
                              fd                                           <>
                              (descriptions descs)
                            ) <$> options
            usageLine = "Usage: squeal " <> (intercalate " | " (optionName <$> options))
            usageOutput = headerLine <> "\n" <> usageLine <> "\n" <> optionOutput

        in T.pack $ usageOutput

headerLine :: String
headerLine = "Squeal version: " <> (showVersion version)

couldNotFindBreachedAccounts :: String
couldNotFindBreachedAccounts = "Could not find breached accounts"

printEndpointCallError :: String -> EndpointCallError -> String
printEndpointCallError ctx (ApiCallError httpError)     = printf "%s request error: http error: %s" ctx (show httpError)
printEndpointCallError ctx (InvalidUrl url reason)      = printf "%s request error: invalid url: %s due to: %s" ctx (show url) reason
printEndpointCallError ctx (InvalidContext req context) = printf "%s request error: invalid invocation for req: %s and context: %s" ctx (show req) (show context)

listBreaches :: Email -> BreachedAccounts -> String
listBreaches (Email email) (BreachedAccounts []) = printf "No Breached Accounts for: %s" (T.unpack email)
listBreaches (Email email) (BreachedAccounts xs) = printf "Breached Accounts for %s:\n%s" (T.unpack email) (bulleted xs)
  where bulleted :: [BreachedAccount] -> String
        bulleted = intercalate ("\n") . fmap ((" - " <>) . T.unpack . name)

handleBreachErrors :: Email -> BreachError -> String
handleBreachErrors email (BreachApiError (ApiCallError (NotFound _))) = listBreaches email (BreachedAccounts [])
handleBreachErrors _ otherError = breachErrorToString otherError

waitingForDelay :: Seconds -> String
waitingForDelay (Seconds d) = printf "waiting for %d seconds" $ d

lookingUpEmail :: Email -> String
lookingUpEmail (Email em) = printf "Looking up email: %s" em

noBreaches :: Email -> String
noBreaches (Email em) = printf "No breaches for email: %s" em

breachErrorToString :: BreachError -> String
breachErrorToString (BreachApiError endpointCallError)  = printEndpointCallError "breaches" endpointCallError
breachErrorToString (InvalidResponse decodeError res)   = printf "breaches decode error: %s with response: %s" (show decodeError) (show res)

printPasswordHashStolen :: Bool -> String
printPasswordHashStolen = ("Password stolen: " <>) . show

handlePasswordHashErrors :: PasswordHashError -> String
handlePasswordHashErrors (PasswordHashApiError (ApiCallError (NotFound _))) = printPasswordHashStolen False
handlePasswordHashErrors otherError = passwordHashErrorToString otherError

passwordHashErrorToString :: PasswordHashError -> String
passwordHashErrorToString (PasswordHashApiError endpointCallError) = printEndpointCallError "passwordhash" endpointCallError
