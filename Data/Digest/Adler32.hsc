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
-- Adler32 wrapper
--
------------------------------------------------------------

module Data.Digest.Adler32 (
    Adler32, adler32, adler32Update
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

-- | The class of values for which Adler32 may be computed
class Adler32 a where
    -- | Compute Adler32 checksum
    adler32 :: a -> Word32
    adler32 = adler32Update 1

    -- | Given the Adler32 checksum of a string, compute Adler32 of its
    -- concatenation with another string (t.i., incrementally update the 
    -- Adler32 hash value).
    adler32Update :: Word32 -> a -> Word32

instance Adler32 S.ByteString where
    adler32Update = adler32_s_update

instance Adler32 L.ByteString where
    adler32Update = adler32_l_update

instance Adler32 [Word8] where
    adler32Update n = (adler32Update n) . L.pack


adler32_s_update :: Word32 -> S.ByteString -> Word32
adler32_s_update n s = adler32_l_update n (LI.Chunk s LI.Empty)

adler32_l_update :: Word32 -> L.ByteString -> Word32
adler32_l_update n = LI.foldlChunks updateAdler n
    where updateAdler adler bs = fromIntegral $ adler32_c (fromIntegral adler) buf (fromIntegral len)
              where (ptr, offset, len) = BI.toForeignPtr bs
                    buf = (unsafeForeignPtrToPtr ptr) `plusPtr` offset

foreign import ccall unsafe "zlib.h adler32"
    adler32_c :: CInt -> Ptr Word8 -> CInt -> CInt -- adler, buf, len -> adler'


