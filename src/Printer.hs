module Printer where

import qualified Data.Text as T
import Data.List (intercalate)
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

usage :: T.Text
usage = T.pack "usage: squeal -e <email_address> | -p <password> | -ef <email_address_file>"

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
