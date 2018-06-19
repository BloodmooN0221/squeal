module Printer where

import qualified Data.Text as T
import Data.Monoid
import Types (BreachedAccounts(..), name)

listBreaches :: BreachedAccounts -> T.Text
listBreaches (BreachedAccounts []) = T.pack "No Breached Accounts"
listBreaches (BreachedAccounts xs) = (T.pack "Breached Accounts:\n" <>) . T.intercalate (T.pack "\n") . fmap ((T.pack " - " <>) . name) $ xs

printPasswordHash :: Bool -> T.Text
printPasswordHash = T.pack . ("Password stolen: " <>) . show

usage :: String
usage = "usage: squeal -e <email_address> | -p <password>"

couldNotFindBreachedAccounts :: T.Text
couldNotFindBreachedAccounts = T.pack "Could not find breached accounts"

