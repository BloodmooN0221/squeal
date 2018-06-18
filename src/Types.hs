module Types (BreachedAccounts(..),
              BreachedAccount(..),
              Breach(..),
              PasswordHash(..),
              Hash(..),
              Email(..)
              ) where

import qualified Data.Text as T

data BreachedAccount =
  BreachedAccount { name :: !T.Text } deriving Show

data BreachedAccounts =
  BreachedAccounts { accounts :: [BreachedAccount] } deriving Show

-- data Service = Breach Email | PasswordHash Hash
data Breach = Breach Email
data PasswordHash = PasswordHash Hash

data Email = Email T.Text

data Hash = Hash { prefix :: !T.Text, suffix :: !T.Text }