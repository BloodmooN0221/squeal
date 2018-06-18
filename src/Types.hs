module Types (BreachedAccounts(..),
              BreachedAccount(..),
              Service(..),
              Hash(..),
              Email(..)
              ) where

import qualified Data.Text as T

data BreachedAccount =
  BreachedAccount { name :: !T.Text } deriving Show

data BreachedAccounts =
  BreachedAccounts { accounts :: [BreachedAccount] } deriving Show

data Service = Breach Email | PasswordHash Hash

data Email = Email T.Text

data Hash = Hash { prefix :: !T.Text, suffix :: !T.Text }