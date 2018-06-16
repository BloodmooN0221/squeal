{-# LANGUAGE OverloadedStrings #-}

module SquealJson where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Types

instance FromJSON BreachedAccount where
  parseJSON (Object v)  = BreachedAccount <$> v .: "Name"
  parseJSON other = typeMismatch "BreachedAccount" other

instance FromJSON BreachedAccounts where
  parseJSON v = BreachedAccounts <$> (parseJSONList v)
