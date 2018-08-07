module Types (BreachedAccounts(..),
              BreachedAccount(..),
              Breach(..),
              PasswordHash(..),
              LookupPasswordHash(..),
              Hash(..),
              Email(..),
              HttpError(..),
              BreachError(..),
              BreachErrorWithEmail(..),
              BreachesApiDelay(..),
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
import Time (MilliSeconds(..))

data BreachedAccount =
  BreachedAccount { name :: !T.Text } deriving Show

data BreachedAccounts =
  BreachedAccounts { accounts :: [BreachedAccount] } deriving Show

data Breach = Breach Email deriving Show

newtype BreachesApiDelay = BreachesApiDelay { delay :: MilliSeconds } deriving Show

data PasswordHash = PasswordHash Hash deriving Show

newtype LookupPasswordHash = LookupPasswordHash PasswordHash

data Email = Email { getEmail :: T.Text } deriving Show

data Hash = Hash { prefix :: !B.ByteString, suffix :: !B.ByteString } deriving Show

data HttpError = BadRequest String | Forbidden String | NotFound String | TooManyRequests String | OtherError Int !B.ByteString deriving Show

data Url = Url String deriving Show

data RequestString = RequestString String deriving Show

data ResponseString = ResponseString String deriving Show

data ContextString = ContextString String deriving Show

data DecodeError = DecodeError String deriving Show

data EndpointCallError = ApiCallError HttpError | InvalidUrl Url String | InvalidContext RequestString ContextString deriving Show

data BreachError = BreachApiError EndpointCallError | InvalidResponse DecodeError ResponseString deriving Show

data BreachErrorWithEmail = BreachErrorWithEmail BreachError Email deriving Show

data PasswordHashError = PasswordHashApiError EndpointCallError deriving Show

instance FromJSON BreachedAccount where
  parseJSON (Object v)  = BreachedAccount <$> v .: (T.pack "Name")
  parseJSON other = typeMismatch "BreachedAccount" other

instance FromJSON BreachedAccounts where
  parseJSON v = BreachedAccounts <$> (parseJSONList v)
