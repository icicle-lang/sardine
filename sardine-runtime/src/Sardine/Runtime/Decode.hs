{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime.Decode (
  -- * Decoding
    Decode
  , decodeStrictByteString

  -- ** Fixed-width unsigned integers
  , decodeWord8
  , decodeWord16le
  , decodeWord32le
  , decodeWord64le

  -- ** Fixed-width signed integers
  , decodeInt8
  , decodeInt16le
  , decodeInt32le
  , decodeInt64le

  -- ** Floating-point numbers
  , decodeFloat32le
  , decodeFloat64le

  -- ** Variable-width unsigned integers
  , decodeVarWord16
  , decodeVarWord32
  , decodeVarWord64

  -- ** Variable width signed integers
  , decodeVarInt16
  , decodeVarInt32
  , decodeVarInt64

  -- ** Failure
  , DecodeError(..)
  , decodeFail

  -- * Thrift
  , decodeBytes
  , decodeList
  , decodeMap

  -- ** Skipping
  , skipType
  , skipList
  , skipMap
  , skipStruct
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Exception (Exception, try, throwIO)
import           Control.Monad (Monad(..), replicateM_)

import           Data.Bits (Bits(..))
import qualified Data.ByteString as Strict
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as ByteString
import           Data.Data (Data)
import           Data.Either (Either(..))
import           Data.Eq (Eq(..))
import           Data.Function ((.))
import           Data.Functor (Functor(..))
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Ord (Ord(..))
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Vector.Generic (Mutable)
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Hybrid as Hybrid
import           Data.Word (Word8, Word16, Word32, Word64)

import           Foreign.Marshal.Unsafe (unsafeLocalState)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.ForeignPtr (withForeignPtr)

import           GHC.Storable (readWord8OffPtr, readWord16OffPtr)
import           GHC.Storable (readWord32OffPtr, readWord64OffPtr)
import           GHC.Storable (readInt8OffPtr, readInt16OffPtr)
import           GHC.Storable (readInt32OffPtr, readInt64OffPtr)
import           GHC.Storable (readFloatOffPtr, readDoubleOffPtr)

import           Prelude (Num(..), Float, Double, ($!), fromIntegral)

import           Sardine.Runtime.Base
import           Sardine.Runtime.Data
import           Sardine.Runtime.ZigZag

import           System.IO (IO)

import           Text.Show (Show(..))

------------------------------------------------------------------------

data DecodeError =
    DecodeVarInt16TooLarge
  | DecodeVarInt32TooLarge
  | DecodeVarInt64TooLarge
  | DecodeNotEnoughBytes !Int !Int
  | DecodeInvalidType !TypeId
  | DecodeInvalidEnumValue !Text !Int32
  | DecodeInvalidFieldType !Text !Text !TypeId
  | DecodeMissingField !Text !Text
  | DecodeMissingUnionAlt !Text
  | DecodeMultipleUnionAlts !Text
  | DecodeUnknownUnionAlt !Text !Int16
    deriving (Eq, Ord, Show, Data, Typeable)

instance Exception DecodeError

------------------------------------------------------------------------
-- Decode

type Offset = Int
type Length = Int
type BytesRequired = Int

data DecodeState =
  DecodeState !Offset !Length

data DecodeResult a =
  DecodeResult !a !DecodeState

unDecodeResult :: DecodeResult a -> a
unDecodeResult (DecodeResult val _) =
  val
{-# INLINE unDecodeResult #-}

data DecodeBuffer =
  DecodeBuffer {
      _decodePtr :: !(Ptr Word8)
    , _decodePull :: !(BytesRequired -> DecodeState -> IO DecodeState)
    }

newtype Decode a =
  Decode {
      runDecode :: DecodeBuffer -> DecodeState -> IO (DecodeResult a)
    }

unsafeIO :: IO a -> Decode a
unsafeIO !io =
  Decode $! \ !_ !s -> do
    !x <- io
    return $! DecodeResult x s

instance Functor Decode where
  fmap !f (Decode !k) =
    Decode $! \ !p !s0 -> do
      DecodeResult !x !s1 <- k p s0
      return $! DecodeResult (f x) s1
  {-# INLINE fmap #-}

instance Applicative Decode where
  pure !x =
    Decode $! \ !_ !s0 ->
      return $! DecodeResult x s0
  {-# INLINE pure #-}

  (<*>) (Decode !mf) (Decode !mx) =
    Decode $! \ !p !s0 -> do
      DecodeResult !f !s1 <- mf p s0
      DecodeResult !x !s2 <- mx p s1
      return $! DecodeResult (f x) s2
  {-# INLINE (<*>) #-}

instance Monad Decode where
  return !x =
    Decode $! \ !_ !s0 ->
      return $! DecodeResult x s0
  {-# INLINE return #-}

  (>>=) !m !f =
    Decode $! \ !p !s0 -> do
      DecodeResult !x !s1 <- runDecode m p s0
      runDecode (f x) p s1
  {-# INLINE (>>=) #-}

------------------------------------------------------------------------
-- ByteString

decodeStrictByteString :: Decode a -> Strict.ByteString -> Either DecodeError a
decodeStrictByteString !decode (PS !fptr !off0 !len0) =
  unsafeLocalState . withForeignPtr fptr $! \ !ptr -> do
    let
      norefill !_ !s = return s
      !ds = DecodeState off0 len0
      !dp = DecodeBuffer (castPtr ptr) norefill
    try $! unDecodeResult <$!> runDecode decode dp ds
{-# INLINE decodeStrictByteString #-}

------------------------------------------------------------------------
-- Util

decodeFail :: DecodeError -> Decode a
decodeFail !err =
  Decode $! \ !_ !_ ->
    throwIO err
{-# NOINLINE decodeFail #-}

decodeAny :: Length -> (Ptr a -> Offset -> IO a) -> Decode a
decodeAny !size !readOffPtr =
  Decode $! \(DecodeBuffer dpPtr dpRefill) (DecodeState off0 len0) -> do
    let
      !off1 = off0 + size
    if off1 <= len0 then do
      !x <- readOffPtr (castPtr dpPtr) off0
      return $! DecodeResult x (DecodeState off1 len0)
    else do
      DecodeState !off2 !len1 <- dpRefill size (DecodeState off0 len0)
      let
        !off3 = off2 + size
      if off3 <= len1 then do
        !x <- readOffPtr (castPtr dpPtr) off2
        return $! DecodeResult x (DecodeState off3 len1)
      else do
        throwIO $! DecodeNotEnoughBytes size (len1 - off2)
{-# INLINE decodeAny #-}

------------------------------------------------------------------------
-- Word

decodeWord8 :: Decode Word8
decodeWord8 =
  decodeAny 1 readWord8OffPtr
{-# INLINE decodeWord8 #-}

decodeWord16le :: Decode Word16
decodeWord16le =
  decodeAny 2 readWord16OffPtr
{-# INLINE decodeWord16le #-}

decodeWord32le :: Decode Word32
decodeWord32le =
  decodeAny 4 readWord32OffPtr
{-# INLINE decodeWord32le #-}

decodeWord64le :: Decode Word64
decodeWord64le =
  decodeAny 8 readWord64OffPtr
{-# INLINE decodeWord64le #-}

------------------------------------------------------------------------
-- Int

decodeInt8 :: Decode Int8
decodeInt8 =
  decodeAny 1 readInt8OffPtr
{-# INLINE decodeInt8 #-}

decodeInt16le :: Decode Int16
decodeInt16le =
  decodeAny 2 readInt16OffPtr
{-# INLINE decodeInt16le #-}

decodeInt32le :: Decode Int32
decodeInt32le =
  decodeAny 4 readInt32OffPtr
{-# INLINE decodeInt32le #-}

decodeInt64le :: Decode Int64
decodeInt64le =
  decodeAny 8 readInt64OffPtr
{-# INLINE decodeInt64le #-}

------------------------------------------------------------------------
-- Float

decodeFloat32le :: Decode Float
decodeFloat32le =
  decodeAny 4 readFloatOffPtr
{-# INLINE decodeFloat32le #-}

decodeFloat64le :: Decode Double
decodeFloat64le =
  decodeAny 8 readDoubleOffPtr
{-# INLINE decodeFloat64le #-}

------------------------------------------------------------------------
-- VarInt

step :: (Bits a, Num a) => Int -> (a -> Decode a) -> a -> Decode a
step !shbits !next !val = do
  !x <- decodeWord8
  if testBit x 7 then
    next $! val .|. (fromIntegral (x .&. 0x7F) `shiftL` shbits)
  else
    return $! val .|. (fromIntegral x `shiftL` shbits)
{-# INLINE step #-}

stepLast :: (Bits b, Num b) => DecodeError -> Int -> b -> Decode b
stepLast !err !shbits !val = do
  !b <- decodeWord8
  if testBit b 7 then
    decodeFail err
  else
    return $! val .|. (fromIntegral b `shiftL` shbits)
{-# INLINE stepLast #-}

decodeVarWord16 :: Decode Word16
decodeVarWord16 =
  let v16 :: (Int -> b -> b) -> (Int -> b) -> b
      v16 !s !sl =
        s 0 . s 7 $! sl 14
      {-# INLINE v16 #-}
  in v16 step (stepLast DecodeVarInt16TooLarge) 0
{-# INLINE decodeVarWord16 #-}

decodeVarWord32 :: Decode Word32
decodeVarWord32 =
  let v32 :: (Int -> b -> b) -> (Int -> b) -> b
      v32 !s !sl =
        s 0 . s 7 . s 14 . s 21 $! sl 28
      {-# INLINE v32 #-}
  in v32 step (stepLast DecodeVarInt32TooLarge) 0
{-# INLINE decodeVarWord32 #-}

decodeVarWord64 :: Decode Word64
decodeVarWord64 =
  let v64 :: (Int -> b -> b) -> (Int -> b) -> b
      v64 !s !sl =
        s  0 . s  7 . s 14 . s 21 . s  28 .
        s 35 . s 42 . s 49 . s 56 $! sl 63
      {-# INLINE v64 #-}
  in v64 step (stepLast DecodeVarInt64TooLarge) 0
{-# INLINE decodeVarWord64 #-}

decodeVarInt16 :: Decode Int16
decodeVarInt16 =
  unZigZag16 <$!> decodeVarWord16
{-# INLINE decodeVarInt16 #-}

decodeVarInt32 :: Decode Int32
decodeVarInt32 =
  unZigZag32 <$!> decodeVarWord32
{-# INLINE decodeVarInt32 #-}

decodeVarInt64 :: Decode Int64
decodeVarInt64 =
  unZigZag64 <$!> decodeVarWord64
{-# INLINE decodeVarInt64 #-}

------------------------------------------------------------------------
-- Thrift

decodeBytes :: Decode Strict.ByteString
decodeBytes = do
  !len <- fromIntegral <$!> decodeVarWord32
  decodeAny len $! \ !dptr !off ->
    ByteString.create len $! \ !bptr ->
      ByteString.memcpy bptr (dptr `plusPtr` off) len
{-# INLINE decodeBytes #-}

decodeList ::
  Generic.Vector vv v =>
  MGeneric.MVector (Mutable vv) v =>
  (TypeId -> Decode ()) ->
  Decode v ->
  Decode (vv v)
decodeList !checkType !decodeElem = do
  !tag <- decodeWord8

  let
    !lsize = tag `shiftR` 4

  !size <-
    if lsize == 0xF then
      fromIntegral <$!> decodeVarWord32
    else
      return $! fromIntegral lsize

  !mv <- unsafeIO $! MGeneric.unsafeNew size

  let
    !tid = TypeId $! tag .&. 0x0F

  !_ <- checkType tid

  replicateIx_ size $! \ !ix -> do
    !elem <- decodeElem
    unsafeIO $! MGeneric.unsafeWrite mv ix elem
  unsafeIO $! Generic.unsafeFreeze mv
{-# INLINE decodeList #-}

decodeMap ::
  Generic.Vector vk k =>
  Generic.Vector vv v =>
  MGeneric.MVector (Mutable vk) k =>
  MGeneric.MVector (Mutable vv) v =>
  (TypeId -> Decode ()) ->
  (TypeId -> Decode ()) ->
  Decode k ->
  Decode v ->
  Decode (Hybrid.Vector vk vv (k, v))
decodeMap !checkKeyType !checkValType !decodeKey !decodeVal = do
  !size <- fromIntegral <$!> decodeVarWord32
  if size == 0 then
    return Hybrid.empty
  else do
    !tag <- decodeWord8

    !mvk <- unsafeIO $! MGeneric.unsafeNew size
    !mvv <- unsafeIO $! MGeneric.unsafeNew size

    let
      !ktid = TypeId $! tag `shiftR` 4
      !vtid = TypeId $! tag .&. 0x0F

    !_ <- checkKeyType ktid
    !_ <- checkValType vtid

    replicateIx_ size $! \ !ix -> do
      !k <- decodeKey
      unsafeIO $! MGeneric.unsafeWrite mvk ix k
      !v <- decodeVal
      unsafeIO $! MGeneric.unsafeWrite mvv ix v
    unsafeIO $! do
      !vk <- Generic.unsafeFreeze mvk
      !vv <- Generic.unsafeFreeze mvv
      return $! Hybrid.unsafeZip vk vv
{-# INLINE decodeMap #-}

------------------------------------------------------------------------
-- Thrift - Skipping

skipType :: TypeId -> Decode ()
skipType !tid =
  case unTypeId tid of
    0x1 ->
      return () -- bool/true
    0x2 ->
      return () -- bool/false
    0x3 -> do
      !_ <- decodeWord8
      return ()
    0x4 -> do
      !_ <- decodeVarInt16
      return ()
    0x5 -> do
      !_ <- decodeVarInt32
      return ()
    0x6 -> do
      !_ <- decodeVarInt64
      return ()
    0x7 -> do
      !_ <- decodeFloat64le
      return ()
    0x8 -> do
      !_ <- decodeBytes
      return ()
    0x9 ->
      skipList -- list
    0xA ->
      skipList -- set
    0xB ->
      skipMap
    0xC ->
      skipStruct
    _ ->
      decodeFail $! DecodeInvalidType tid
{-# INLINE skipType #-}

skipList :: Decode ()
skipList = do
  !tag <- decodeWord8
  let
    !tid = TypeId $! tag .&. 0x0F
    !lsize = tag `shiftR` 4
  !size <-
    if lsize == 0xF then
      fromIntegral <$!> decodeVarInt32
    else
      return $! fromIntegral lsize
  replicateM_ size $! do
    !_ <- skipType tid
    return ()
{-# INLINE skipList #-}

skipMap :: Decode ()
skipMap = do
  !size <- fromIntegral <$!> decodeVarInt32
  if size == 0 then
    return ()
  else do
    !tag <- decodeWord8
    let
      !kt = TypeId $! tag `shiftR` 4
      !vt = TypeId $! tag .&. 0x0F
    replicateM_ size $! do
      !_ <- skipType kt
      !_ <- skipType vt
      return ()
{-# INLINE skipMap #-}

skipStruct :: Decode ()
skipStruct =
  let
    loop !last = do
      !tag <- decodeWord8
      if tag == 0 then
        return ()
      else do
        let
          !tid = TypeId $! tag .&. 0x0F
          !mod = tag `shiftR` 4
        !ident <-
          if mod /= 0 then
            return $! last + fromIntegral mod
          else
            decodeInt16le
        !_ <- skipType tid
        loop ident
  in
    loop 0
{-# INLINE skipStruct #-}
