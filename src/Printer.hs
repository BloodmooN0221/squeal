module Printer where

import qualified Data.Text as T
import Data.Monoid
import Types (BreachedAccounts(..), name)

printBreaches :: BreachedAccounts -> T.Text
printBreaches (BreachedAccounts []) = T.pack "No Breached Accounts"
printBreaches (BreachedAccounts xs) = (T.pack "Breached Accounts:\n" <>) . T.intercalate (T.pack "\n") . fmap ((T.pack " - " <>) . name) $ xs