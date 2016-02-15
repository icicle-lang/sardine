{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Runtime.VarInt (
  -- * Data
    VarIntError(..)

  -- * Unsigned
  , encodeVarWord16
  , encodeVarWord32
  , encodeVarWord64
  , decodeVarWord16
  , decodeVarWord32
  , decodeVarWord64

  -- * Signed
  , encodeVarInt16
  , encodeVarInt32
  , encodeVarInt64
  , decodeVarInt16
  , decodeVarInt32
  , decodeVarInt64
  ) where

import           Control.Monad (return, (>>))

import           Data.Bifunctor (Bifunctor(..))
import           Data.Bits (Bits(..))
import           Data.Eq (Eq(..))
import           Data.Function ((.))
import           Data.Functor ((<$>))
import           Data.Functor.Contravariant ((>$<))
import           Data.Int (Int, Int16, Int32, Int64)
import           Data.Ord (Ord(..))
import           Data.Void (absurd)
import           Data.Word (Word16, Word32, Word64)

import           GHC.Storable (writeWord8OffPtr)

import           GHC.Exts (Ptr(..), Int(..), Word(..), isTrue#)
import           GHC.Prim (Addr#, plusAddr#, minusAddr#)
import           GHC.Prim (Int#, (+#), (*#), (-#))
import           GHC.Prim (geWord#, (>=#))

import           Prelude (Num(..), Integral(..), ($), ($!), fromIntegral)

import           Sardine.Runtime.Internal
import           Sardine.Runtime.Primitive
import           Sardine.Runtime.ZigZag

import           System.IO (IO)

import           Text.Show (Show(..))

------------------------------------------------------------------------
-- Data

data VarIntError =
    VarInt16TooLong
  | VarInt32TooLong
  | VarInt64TooLong
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Unsigned

encodeVarWord16 :: Encode Word16
encodeVarWord16 =
  unsafeEncodeVar 3 $
    encodeStep7 0# .
    encodeStep7 1# $
    encodeLast7 2#
{-# INLINE encodeVarWord16 #-}

encodeVarWord32 :: Encode Word32
encodeVarWord32 =
  unsafeEncodeVar 5 $
    encodeStep7 0# .
    encodeStep7 1# .
    encodeStep7 2# .
    encodeStep7 3# $
    encodeLast7 4#
{-# INLINE encodeVarWord32 #-}

encodeVarWord64 :: Encode Word64
encodeVarWord64 =
  unsafeEncodeVar 10 $
    encodeStep7 0# .
    encodeStep7 1# .
    encodeStep7 2# .
    encodeStep7 3# .
    encodeStep7 4# .
    encodeStep7 5# .
    encodeStep7 6# .
    encodeStep7 7# .
    encodeStep7 8# $
    encodeLast7 9#
{-# INLINE encodeVarWord64 #-}

decodeVarWord16 :: Decode VarIntError Word16
decodeVarWord16 =
  (decodeStep7 0 .
   decodeStep7 1 $
   decodeLast7 2 VarInt16TooLong) 0
{-# INLINE decodeVarWord16 #-}

decodeVarWord32 :: Decode VarIntError Word32
decodeVarWord32 =
  (decodeStep7 0 .
   decodeStep7 1 .
   decodeStep7 2 .
   decodeStep7 3 $
   decodeLast7 4 VarInt32TooLong) 0
{-# INLINE decodeVarWord32 #-}

decodeVarWord64 :: Decode VarIntError Word64
decodeVarWord64 =
  (decodeStep7 0 .
   decodeStep7 1 .
   decodeStep7 2 .
   decodeStep7 3 .
   decodeStep7 4 .
   decodeStep7 5 .
   decodeStep7 6 .
   decodeStep7 7 .
   decodeStep7 8 $
   decodeLast7 9 VarInt64TooLong) 0
{-# INLINE decodeVarWord64 #-}

------------------------------------------------------------------------
-- Signed

encodeVarInt16 :: Encode Int16
encodeVarInt16 =
  zigZag16 >$< encodeVarWord16
{-# INLINE encodeVarInt16 #-}

encodeVarInt32 :: Encode Int32
encodeVarInt32 =
  zigZag32 >$< encodeVarWord32
{-# INLINE encodeVarInt32 #-}

encodeVarInt64 :: Encode Int64
encodeVarInt64 =
  zigZag64 >$< encodeVarWord64
{-# INLINE encodeVarInt64 #-}

decodeVarInt16 :: Decode VarIntError Int16
decodeVarInt16 =
  unZigZag16 <$> decodeVarWord16
{-# INLINE decodeVarInt16 #-}

decodeVarInt32 :: Decode VarIntError Int32
decodeVarInt32 =
  unZigZag32 <$> decodeVarWord32
{-# INLINE decodeVarInt32 #-}

decodeVarInt64 :: Decode VarIntError Int64
decodeVarInt64 =
  unZigZag64 <$> decodeVarWord64
{-# INLINE decodeVarInt64 #-}

------------------------------------------------------------------------
-- Encoding Utils

unsafeEncodeVar :: Int -> (Addr# -> DstAddr -> a -> (DstAddr -> IO ()) -> IO ()) -> Encode a
unsafeEncodeVar (I# maxLen) writeVar =
  Encode $ \x k ->
  EncodeIO $ \push dst0 end0 ->
    let
      dstLen = minusAddr# end0 dst0
    in
      if isTrue# (dstLen >=# maxLen) then
        writeVar dst0 dst0 x $ \dst1 ->
        runEncodeIO k push dst1 end0
      else
        push dst0 maxLen $ \dst1 end1 ->
        writeVar dst1 dst1 x $ \dst2 ->
        runEncodeIO k push dst2 end1
{-# INLINE unsafeEncodeVar #-}

encodeStep7 ::
  (Show a, Bits a, Integral a) =>
  Int# ->
  (Addr# -> DstAddr -> a -> (DstAddr -> IO ()) -> IO ()) ->
  Addr# ->
  DstAddr ->
  a ->
  (DstAddr -> IO ()) ->
  IO ()
encodeStep7 byte next dst0 dst x k =
  let
    !shbits = I# (byte *# 7#)
    !xx = x `shiftR` shbits
    !(I# write1) = zeroOrOne (fromIntegral (xx .&. complement 0x7F))
    !mask = 0x80 * fromIntegral (I# write1)
  in
    writeWord8OffPtr (Ptr dst0) (I# byte) (mask .|. (fromIntegral xx .&. 0x7F)) >>
    next dst0 (plusAddr# dst write1) x k
{-# INLINE encodeStep7 #-}

encodeLast7 ::
  (Bits a, Integral a) =>
  Int# ->
  Addr# ->
  DstAddr ->
  a ->
  (DstAddr -> IO ()) ->
  IO ()
encodeLast7 byte dst0 dst x k =
  let
    !shbits = I# (byte *# 7#)
    !xx = x `shiftR` shbits
    !(I# write1) = zeroOrOne (fromIntegral (xx .&. complement 0x7F))
  in
    writeWord8OffPtr (Ptr dst0) (I# byte) (fromIntegral xx) >>
    k (plusAddr# dst (write1 +# 1#))
{-# INLINE encodeLast7 #-}

zeroOrOne :: Word -> Int
zeroOrOne !(W# w) =
  I# (1# -# (geWord# 0## w))
{-# INLINE zeroOrOne #-}

------------------------------------------------------------------------
-- Decoding Utils

-- TODO fast path

decodeStep7 :: (Bits a, Num a) => Int -> (a -> Decode x a) -> a -> Decode x a
decodeStep7 !byte next !val = do
  let
    !shbits = byte * 7
  !x <- first absurd decodeWord8
  if testBit x 7 then
    next $! val .|. shiftL (fromIntegral (x .&. 0x7F)) shbits
  else
    return $! val .|. shiftL (fromIntegral x) shbits
{-# INLINE decodeStep7 #-}

decodeLast7 :: (Bits a, Num a) => Int -> VarIntError -> a -> Decode VarIntError a
decodeLast7 !byte !err !val = do
  !b <- first absurd decodeWord8
  if testBit b 7 then
    decodeFail err
  else
    let
      !shbits = byte * 7
    in
      return $! val .|. shiftL (fromIntegral b) shbits
{-# INLINE decodeLast7 #-}
