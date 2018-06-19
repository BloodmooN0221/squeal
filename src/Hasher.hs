module Hasher where

import Data.Char (toUpper)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash

sha1 :: B.ByteString -> B.ByteString
sha1  = C8.pack . (toUpper <$>) . show . (hashWith SHA1)
