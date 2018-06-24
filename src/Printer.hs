module Printer where

import qualified Data.Text as T
import Data.List (intercalate)
import Data.Monoid
import Text.Printf (printf)
import Types (BreachedAccounts(..), BreachError(..), name)

listBreaches :: BreachedAccounts -> String
listBreaches (BreachedAccounts []) = "No Breached Accounts"
listBreaches (BreachedAccounts xs) = ("Breached Accounts:\n" <>) . intercalate ("\n") . fmap ((" - " <>) . T.unpack . name) $ xs

printPasswordHash :: Bool -> T.Text
printPasswordHash = T.pack . ("Password stolen: " <>) . show

usage :: T.Text
usage = T.pack "usage: squeal -e <email_address> | -p <password>"

couldNotFindBreachedAccounts :: String
couldNotFindBreachedAccounts = "Could not find breached accounts"

breachErrorToString :: BreachError -> String
breachErrorToString (ApiCallError httpError)          = printf "http error when calling the api: %s" (show httpError)
breachErrorToString (InvalidUrl url reason)           = printf "invalid url: %s due to: %s" (show url) reason
breachErrorToString (InvalidContext req context)      = printf "invalid invocation, req: %s, context: %s" (show req) (show context)
breachErrorToString (InvalidResponse decodeError res) = printf "breaches decode error: %s with response: %s" (show decodeError) (show res)


