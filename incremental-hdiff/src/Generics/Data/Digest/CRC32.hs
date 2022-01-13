
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Data.Digest.CRC32 where

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (toLazyByteString, word32LE)
import qualified Data.ByteString.Char8   as BS8
import           Data.ByteString.Lazy    (unpack)
import           Data.Digest.CRC32
import           Data.List               (foldl', splitAt)
import           Data.Word               (Word32, Word64, Word8)

-- TYPES / INSTANCES

newtype Digest
  = Digest { getCRC32 :: Word32 }
  deriving (Eq , Show)

class Digestible v where
  digest :: v -> Digest

instance Show a => Digestible a where
  digest = hashStr . show

encodeWord32 :: Word32 -> [Word8]
encodeWord32 = unpack . toLazyByteString . word32LE

instance CRC32 Digest where
  crc32Update w = crc32Update w . encodeWord32 . getCRC32

-- UTILITY FUNCTIONS

hashCRC32 :: BS.ByteString -> Digest
hashCRC32 = Digest . crc32

-- | Auxiliar hash functions for strings
hashStr :: String -> Digest
hashStr = hashCRC32 . BS8.pack

combineCRC32 :: CRC32 a => Digest -> a -> Digest
combineCRC32 dig x = dig'
  where
    h = getCRC32 dig
    dig' = Digest $ crc32Update h x

-- | Concatenates digests together and hashes the result.
digestConcat :: [Digest] -> Digest
digestConcat []     = error "No Empty List for digestConcat"
digestConcat (x:xs) = foldl' (flip combineCRC32) x xs
