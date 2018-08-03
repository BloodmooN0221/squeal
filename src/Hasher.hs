module Hasher (parsePasswordHash) where

import Data.Char (toUpper)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash
import Types

sha1 :: B.ByteString -> B.ByteString
sha1  = C8.pack . (toUpper <$>) . show . (hashWith SHA1)

parsePasswordHash :: String -> PasswordHash
parsePasswordHash password = let hashed = sha1 (C8.pack password)
                                 (hashedPrefix, hashedSuffix) = B.splitAt 5 hashed
                             in PasswordHash $ Hash { prefix = hashedPrefix, suffix = hashedSuffix }
