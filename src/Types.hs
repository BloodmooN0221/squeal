module Types (BreachedAccounts(..),
              BreachedAccount(..),
              Breach(..),
              PasswordHash(..),
              Hash(..),
              Email(..),
              HttpError(..),
              BreachError(..),
              BreachErrorWithEmail(..),
              PasswordHashError(..),
              EndpointCallError(..),
              Url(..),
              RequestString(..),
              ResponseString(..),
              ContextString(..),
              DecodeError(..)
              ) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

data BreachedAccount =
  BreachedAccount { name :: !T.Text } deriving Show

data BreachedAccounts =
  BreachedAccounts { accounts :: [BreachedAccount] } deriving Show

-- data Service = Breach Email | PasswordHash Hash
data Breach = Breach Email deriving Show
data PasswordHash = PasswordHash Hash deriving Show

data Email = Email T.Text deriving Show

data Hash = Hash { prefix :: !B.ByteString, suffix :: !B.ByteString } deriving Show

data HttpError = BadRequest String | Forbidden String | NotFound String | TooManyRequests String | OtherError Int !B.ByteString deriving Show

data Url = Url String deriving Show

data RequestString = RequestString String deriving Show

data ResponseString = ResponseString String deriving Show

data ContextString = ContextString String deriving Show

data DecodeError = DecodeError String deriving Show

data EndpointCallError = ApiCallError HttpError | InvalidUrl Url String | InvalidContext RequestString ContextString deriving Show

data BreachError = BreachApiError EndpointCallError | InvalidResponse DecodeError ResponseString deriving Show

data BreachErrorWithEmail = BreachErrorWithEmail BreachError Email

data PasswordHashError = PasswordHashApiError EndpointCallError deriving Show

instance FromJSON BreachedAccount where
  parseJSON (Object v)  = BreachedAccount <$> v .: (T.pack "Name")
  parseJSON other = typeMismatch "BreachedAccount" other

instance FromJSON BreachedAccounts where
  parseJSON v = BreachedAccounts <$> (parseJSONList v)