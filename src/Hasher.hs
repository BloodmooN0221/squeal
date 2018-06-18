module Hasher where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Numeric (showHex)
import Crypto.Hash.SHA1 (hash)

sha1 :: B.ByteString -> B.ByteString
sha1  = C8.pack . (B.foldr showHex "") . hash
