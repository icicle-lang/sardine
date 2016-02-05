{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Runtime.Primitive (
  -- * Unsigned
    encodeWord8
  , encodeWord16le
  , encodeWord32le
  , encodeWord64le
  , decodeWord8
  , decodeWord16le
  , decodeWord32le
  , decodeWord64le

  -- * Signed
  , encodeInt8
  , encodeInt16le
  , encodeInt32le
  , encodeInt64le
  , decodeInt8
  , decodeInt16le
  , decodeInt32le
  , decodeInt64le

  -- * Floating Point
  , encodeFloat32le
  , encodeFloat64le
  , decodeFloat32le
  , decodeFloat64le

  -- * Bytes
  , encodeBytes
  , decodeBytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Function (flip)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Void (Void)
import           Data.Word (Word8, Word16, Word32, Word64)

import           Foreign.ForeignPtr (withForeignPtr)

import           GHC.Storable (readWord8OffPtr, writeWord8OffPtr)
import           GHC.Storable (readWord16OffPtr, writeWord16OffPtr)
import           GHC.Storable (readWord32OffPtr, writeWord32OffPtr)
import           GHC.Storable (readWord64OffPtr, writeWord64OffPtr)
import           GHC.Storable (readInt8OffPtr, writeInt8OffPtr)
import           GHC.Storable (readInt16OffPtr, writeInt16OffPtr)
import           GHC.Storable (readInt32OffPtr, writeInt32OffPtr)
import           GHC.Storable (readInt64OffPtr, writeInt64OffPtr)
import           GHC.Storable (readFloatOffPtr, writeFloatOffPtr)
import           GHC.Storable (readDoubleOffPtr, writeDoubleOffPtr)

import           GHC.Exts (Ptr(..), Int(..))
import           GHC.Prim (plusAddr#)

import           Prelude (Float, Double, ($))

import           Sardine.Runtime.Internal

------------------------------------------------------------------------
-- Unsigned

encodeWord8 :: Encode Word8
encodeWord8 =
  unsafeEncodeFixed 1 (flip writeWord8OffPtr 0)
{-# INLINE encodeWord8 #-}

encodeWord16le :: Encode Word16
encodeWord16le =
  unsafeEncodeFixed 2 (flip writeWord16OffPtr 0)
{-# INLINE encodeWord16le #-}

encodeWord32le :: Encode Word32
encodeWord32le =
  unsafeEncodeFixed 4 (flip writeWord32OffPtr 0)
{-# INLINE encodeWord32le #-}

encodeWord64le :: Encode Word64
encodeWord64le =
  unsafeEncodeFixed 8 (flip writeWord64OffPtr 0)
{-# INLINE encodeWord64le #-}

decodeWord8 :: Decode Void Word8
decodeWord8 =
  unsafeDecodeFixed 1 (flip readWord8OffPtr 0)
{-# INLINE decodeWord8 #-}

decodeWord16le :: Decode Void Word16
decodeWord16le =
  unsafeDecodeFixed 2 (flip readWord16OffPtr 0)
{-# INLINE decodeWord16le #-}

decodeWord32le :: Decode Void Word32
decodeWord32le =
  unsafeDecodeFixed 4 (flip readWord32OffPtr 0)
{-# INLINE decodeWord32le #-}

decodeWord64le :: Decode Void Word64
decodeWord64le =
  unsafeDecodeFixed 8 (flip readWord64OffPtr 0)
{-# INLINE decodeWord64le #-}

------------------------------------------------------------------------
-- Signed

encodeInt8 :: Encode Int8
encodeInt8 =
  unsafeEncodeFixed 1 (flip writeInt8OffPtr 0)
{-# INLINE encodeInt8 #-}

encodeInt16le :: Encode Int16
encodeInt16le =
  unsafeEncodeFixed 2 (flip writeInt16OffPtr 0)
{-# INLINE encodeInt16le #-}

encodeInt32le :: Encode Int32
encodeInt32le =
  unsafeEncodeFixed 4 (flip writeInt32OffPtr 0)
{-# INLINE encodeInt32le #-}

encodeInt64le :: Encode Int64
encodeInt64le =
  unsafeEncodeFixed 8 (flip writeInt64OffPtr 0)
{-# INLINE encodeInt64le #-}

decodeInt8 :: Decode Void Int8
decodeInt8 =
  unsafeDecodeFixed 1 (flip readInt8OffPtr 0)
{-# INLINE decodeInt8 #-}

decodeInt16le :: Decode Void Int16
decodeInt16le =
  unsafeDecodeFixed 2 (flip readInt16OffPtr 0)
{-# INLINE decodeInt16le #-}

decodeInt32le :: Decode Void Int32
decodeInt32le =
  unsafeDecodeFixed 4 (flip readInt32OffPtr 0)
{-# INLINE decodeInt32le #-}

decodeInt64le :: Decode Void Int64
decodeInt64le =
  unsafeDecodeFixed 8 (flip readInt64OffPtr 0)
{-# INLINE decodeInt64le #-}

------------------------------------------------------------------------
-- Floating Point

encodeFloat32le :: Encode Float
encodeFloat32le =
  unsafeEncodeFixed 4 (flip writeFloatOffPtr 0)
{-# INLINE encodeFloat32le #-}

encodeFloat64le :: Encode Double
encodeFloat64le =
  unsafeEncodeFixed 8 (flip writeDoubleOffPtr 0)
{-# INLINE encodeFloat64le #-}

decodeFloat32le :: Decode Void Float
decodeFloat32le =
  unsafeDecodeFixed 4 (flip readFloatOffPtr 0)
{-# INLINE decodeFloat32le #-}

decodeFloat64le :: Decode Void Double
decodeFloat64le =
  unsafeDecodeFixed 8 (flip readDoubleOffPtr 0)
{-# INLINE decodeFloat64le #-}

------------------------------------------------------------------------
-- Bytes

encodeBytes :: Encode Strict.ByteString
encodeBytes =
  unsafeEncodeSized B.length $ \(Ptr dst) (ByteString.PS fp (I# off) (I# len)) ->
    withForeignPtr fp $ \(Ptr bs) ->
      ByteString.memcpy (Ptr dst) (Ptr (plusAddr# bs off)) (I# len)
{-# INLINE encodeBytes #-}

decodeBytes :: Int -> Decode Void Strict.ByteString
decodeBytes len =
  unsafeDecodeFixed len $ \(Ptr src) ->
    ByteString.create len $ \(Ptr dst) ->
      ByteString.memcpy (Ptr dst) (Ptr src) len
{-# INLINE decodeBytes #-}
