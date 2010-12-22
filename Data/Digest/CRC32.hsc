{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}
------------------------------------------------------------
-- |
-- Copyright    :   (c) 2008 Eugene Kirpichov
-- License      :   BSD-style
--
-- Maintainer   :   ekirpichov@gmail.com
-- Stability    :   experimental
-- Portability  :   portable (H98 + FFI)
--
-- CRC32 wrapper
--
------------------------------------------------------------

module Data.Digest.CRC32 (
    CRC32, crc32, crc32Update
) where

import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr ()
import GHC.Ptr ()

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

#include "zlib.h"

-- | The class of values for which CRC32 may be computed
class CRC32 a where
    -- | Compute CRC32 checksum
    crc32 :: a -> Word32
    crc32 = crc32Update 0

    -- | Given the CRC32 checksum of a string, compute CRC32 of its
    -- concatenation with another string (t.i., incrementally update 
    -- the CRC32 hash value)
    crc32Update :: Word32 -> a -> Word32

instance CRC32 S.ByteString where
    crc32Update = crc32_s_update

instance CRC32 L.ByteString where
    crc32Update = crc32_l_update

instance CRC32 [Word8] where
    crc32Update n = (crc32Update n) . L.pack


crc32_s_update :: Word32 -> S.ByteString -> Word32
crc32_s_update n s = crc32_l_update n (LI.Chunk s LI.Empty)

crc32_l_update :: Word32 -> L.ByteString -> Word32
crc32_l_update n = LI.foldlChunks updateCRC n
    where updateCRC crc bs = fromIntegral $ crc32_c (fromIntegral crc) buf (fromIntegral len)
              where (ptr, offset, len) = BI.toForeignPtr bs
                    buf = (unsafeForeignPtrToPtr ptr) `plusPtr` offset

foreign import ccall unsafe "zlib.h crc32"
    crc32_c :: CInt -> Ptr Word8 -> CInt -> CInt -- crc, buf, len -> crc'


