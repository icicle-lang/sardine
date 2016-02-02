{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime.Encode (
  -- * Encoding
    Encode
  , EncodeBounded

  , (<+>)
  , widen
  , fromBounded

  , encodeIO
  , encodeStrictByteString
  , encodeStrictByteString'

  -- ** Fixed-width unsigned integers
  , encodeWord8
  , encodeWord16le
  , encodeWord32le
  , encodeWord64le

  -- ** Fixed-width signed integers
  , encodeInt8
  , encodeInt16le
  , encodeInt32le
  , encodeInt64le

  -- ** Floating-point numbers
  , encodeFloat32le
  , encodeFloat64le

  -- ** Variable-width unsigned integers
  , encodeVarWord16
  , encodeVarWord32
  , encodeVarWord64

  -- ** Variable width signed integers
  , encodeVarInt16
  , encodeVarInt32
  , encodeVarInt64

  -- ** Thrift
  , encodeBytes
  , encodeList
  , encodeMap
  ) where

import           Control.Exception (bracketOnError)
import           Control.Monad (Monad(..))

import           Data.Bits (Bits(..))
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as ByteString
import           Data.Eq (Eq(..))
import           Data.Function ((.))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Monoid ((<>))
import           Data.Monoid (Monoid(..))
import           Data.Ord (Ord(..))
import           Data.Tuple (fst)
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Hybrid as Hybrid
import           Data.Word (Word8, Word16, Word32, Word64)

import           Foreign.Marshal.Unsafe (unsafeLocalState)
import           Foreign.Marshal.Alloc (mallocBytes, reallocBytes, allocaBytes)
import           Foreign.Marshal.Alloc (free, finalizerFree)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.ForeignPtr (newForeignPtr, withForeignPtr)

import           GHC.Storable (writeFloatOffPtr, writeDoubleOffPtr)
import           GHC.Storable (writeInt32OffPtr, writeInt64OffPtr)
import           GHC.Storable (writeInt8OffPtr, writeInt16OffPtr)
import           GHC.Storable (writeWord32OffPtr, writeWord64OffPtr)
import           GHC.Storable (writeWord8OffPtr, writeWord16OffPtr)

import           Prelude (Num(..), Integral(..), Float, Double, ($), ($!), div, fromIntegral)

import           Sardine.Runtime.Base
import           Sardine.Runtime.Data
import           Sardine.Runtime.ZigZag

import           System.IO (IO)

import           Text.Show (Show(..))

------------------------------------------------------------------------
-- Encode

type Offset = Int
type Length = Int

data PtrOff =
  PtrOff !(Ptr Word8) !Offset

newtype EncodeBounded (maxsize :: Nat) =
  EncodeBounded {
      runBounded :: Ptr Word8 -> Offset -> IO Offset
    }

newtype Encode (bufsize :: Nat) =
  Encode {
      runEncode :: (Length -> IO (Ptr Word8)) -> PtrOff -> IO PtrOff
    }

infixr 6 <+>

-- can't be a Data.Monoid because of the extra type parameter
(<+>) :: EncodeBounded n -> EncodeBounded m -> EncodeBounded (n + m)
(<+>) x y =
  EncodeBounded $ \ !eb !off0 -> do
    !off1 <- runBounded x eb off0
    runBounded y eb off1
{-# INLINE (<+>) #-}

widen :: EncodeBounded n -> EncodeBounded (n + m)
widen (EncodeBounded x) =
  EncodeBounded x
{-# INLINE widen #-}

instance Monoid (Encode bufsize) where
  mempty =
    Encode $ \ _ (PtrOff !ptr !off) ->
      return $! PtrOff ptr off
  {-# INLINE mempty #-}

  mappend x y =
    Encode $ \ flush (PtrOff !ptr0 !off0) -> do
      PtrOff !ptr1 !off1 <- runEncode x flush (PtrOff ptr0 off0)
      runEncode y flush $! PtrOff ptr1 off1
  {-# INLINE mappend #-}


------------------------------------------------------------------------
-- ByteString

encodeIO :: forall bufsize. KnownNat bufsize => Encode bufsize -> (Ptr Word8 -> Length -> IO ()) -> IO ()
encodeIO !encode !io0 =
  let
    !size = fromIntegral $! natVal (Proxy :: Proxy bufsize)
  in
    allocaBytes size $ \ !ptr0 -> do
      let
        io1 len = do
          !_ <- io0 ptr0 len
          return $! ptr0
      PtrOff !ptr1 !off1 <- runEncode encode io1 (PtrOff ptr0 0)
      io0 ptr1 off1
{-# INLINE encodeIO #-}

encodeStrictByteString :: forall bufsize. KnownNat bufsize => Encode bufsize -> Strict.ByteString
encodeStrictByteString !encode =
  let
    !size = fromIntegral $! natVal (Proxy :: Proxy bufsize)
  in
    unsafeLocalState . withGrowBuffer size $ \ !gb !ptr -> do
        PtrOff !_ !off <- runEncode encode (putGrowBuffer gb) (PtrOff ptr 0)
        !_ <- putGrowBuffer gb off
        return ()
{-# INLINE encodeStrictByteString #-}

encodeStrictByteString' :: forall maxsize. KnownNat maxsize => EncodeBounded maxsize -> Strict.ByteString
encodeStrictByteString' !capped =
  let
    !size = fromNat (Proxy :: Proxy maxsize)
  in
    unsafeLocalState . ByteString.createUptoN size $ \ !ptr ->
        runBounded capped ptr 0
{-# INLINE encodeStrictByteString' #-}

------------------------------------------------------------------------
-- GrowBuffer

data GBState =
  GBState {
      _gbPtr :: !(Ptr Word8)
    , _gbOffset :: !Offset
    , _gbLength :: !Length
    , _gbMinLength :: !Length
    }

newtype GrowBuffer =
  GrowBuffer {
      _unGrowBuffer :: IORef GBState
    }

newGrowBuffer :: Length -> IO (GrowBuffer, Ptr Word8)
newGrowBuffer len = do
  !ptr <- mallocBytes len
  !ref <- newIORef (GBState ptr 0 len len)
  return $! (GrowBuffer ref, ptr)
{-# INLINE newGrowBuffer #-}

putGrowBuffer :: GrowBuffer -> Length -> IO (Ptr Word8)
putGrowBuffer (GrowBuffer !ref) !slen = do
  GBState !dptr0 !doff0 !dlen0 !dmin <- readIORef ref
  let
    !doff1 = doff0 + slen
    !dreq = doff1 + dmin
  if dreq <= dlen0 then do
    writeIORef ref (GBState dptr0 doff1 dlen0 dmin)
    return $! dptr0 `plusPtr` doff1
  else do
    let
      !dlen1 = max dreq (dlen0 + dlen0 `div` 2)
    !dptr1 <- reallocBytes dptr0 dlen1
    writeIORef ref (GBState dptr1 doff1 dlen1 dmin)
    return $! dptr1 `plusPtr` doff1
{-# INLINE putGrowBuffer #-}

closeGrowBuffer :: GrowBuffer -> IO Strict.ByteString
closeGrowBuffer (GrowBuffer !ref) = do
  GBState !ptr !off !_ !_ <- readIORef ref
  !ptr' <- reallocBytes ptr off
  !fp <- newForeignPtr finalizerFree ptr'
  return $! ByteString.fromForeignPtr fp 0 off
{-# INLINE closeGrowBuffer #-}

freeGrowBuffer :: GrowBuffer -> IO ()
freeGrowBuffer (GrowBuffer !ref) = do
  GBState !ptr !_ !_ !_ <- readIORef ref
  free ptr
{-# INLINE freeGrowBuffer #-}

withGrowBuffer :: Int -> (GrowBuffer -> Ptr Word8 -> IO ()) -> IO Strict.ByteString
withGrowBuffer !len !io =
  bracketOnError (newGrowBuffer len) (freeGrowBuffer . fst) (\ (!gb, !ptr) -> do
    !_ <- io gb ptr
    closeGrowBuffer gb)
{-# INLINE withGrowBuffer #-}

------------------------------------------------------------------------
-- Util

encodeFixed ::
  forall v maxsize.
  KnownNat maxsize =>
  (Ptr v -> Offset -> v -> IO ()) ->
  v ->
  EncodeBounded maxsize
encodeFixed writeOffPtr !val =
  EncodeBounded $ \ !ptr !off0 -> do
    let
      !off1 = off0 + fromNat (Proxy :: Proxy maxsize)
    !_ <- writeOffPtr (castPtr ptr) off0 val
    return off1
{-# INLINE encodeFixed #-}

-- refinement:
--   (Ptr v -> (n : Offset) -> v -> IO (m : Offset | m <= n + maxsize)) -> v -> Encode (maxsize : Int)
encodeBounded ::
  forall v maxsize.
  KnownNat maxsize =>
  (Ptr Word8 -> Offset -> v -> IO Length) ->
  v ->
  EncodeBounded maxsize
encodeBounded writeOffPtr !val =
  EncodeBounded $ \ !ptr off0 -> do
    !vsize <- writeOffPtr ptr off0 val
    return $! off0 + vsize
{-# INLINE encodeBounded #-}

fromBounded ::
  forall maxsize bufsize.
  AtLeast maxsize bufsize =>
  EncodeBounded maxsize ->
  Encode bufsize
fromBounded capped =
  Encode $ \ flush (PtrOff !ptr0 !off0) -> do
     let
       !vbufsize = fromNat (Proxy :: Proxy bufsize)
       !vmaxsize = fromNat (Proxy :: Proxy maxsize)
       !maxOff = off0 + vmaxsize
     if maxOff <= vbufsize then do
       !off1 <- runBounded capped ptr0 off0
       return $! PtrOff ptr0 off1
     else do
       !ptr1 <- flush off0
       !off1 <- runBounded capped ptr1 0
       return $! PtrOff ptr1 off1
{-# INLINE fromBounded #-}

------------------------------------------------------------------------
-- Word

encodeWord8 :: Word8 -> EncodeBounded 1
encodeWord8 =
  encodeFixed writeWord8OffPtr
{-# INLINE encodeWord8 #-}

encodeWord16le :: Word16 -> EncodeBounded 2
encodeWord16le =
  encodeFixed writeWord16OffPtr
{-# INLINE encodeWord16le #-}

encodeWord32le :: Word32 -> EncodeBounded 4
encodeWord32le =
  encodeFixed writeWord32OffPtr
{-# INLINE encodeWord32le #-}

encodeWord64le :: Word64 -> EncodeBounded 8
encodeWord64le =
  encodeFixed writeWord64OffPtr
{-# INLINE encodeWord64le #-}

------------------------------------------------------------------------
-- Int

encodeInt8 :: Int8 -> EncodeBounded 1
encodeInt8 =
  encodeFixed writeInt8OffPtr
{-# INLINE encodeInt8 #-}

encodeInt16le :: Int16 -> EncodeBounded 2
encodeInt16le =
  encodeFixed writeInt16OffPtr
{-# INLINE encodeInt16le #-}

encodeInt32le :: Int32 -> EncodeBounded 4
encodeInt32le =
  encodeFixed writeInt32OffPtr
{-# INLINE encodeInt32le #-}

encodeInt64le :: Int64 -> EncodeBounded 8
encodeInt64le =
  encodeFixed writeInt64OffPtr
{-# INLINE encodeInt64le #-}

------------------------------------------------------------------------
-- Float

encodeFloat32le :: Float -> EncodeBounded 4
encodeFloat32le =
  encodeFixed writeFloatOffPtr
{-# INLINE encodeFloat32le #-}

encodeFloat64le :: Double -> EncodeBounded 8
encodeFloat64le =
  encodeFixed writeDoubleOffPtr
{-# INLINE encodeFloat64le #-}

------------------------------------------------------------------------
-- VarInt

step ::
  (Show a, Bits a, Integral a) =>
  Offset ->
  (Ptr Word8 -> Offset -> a -> IO Length) ->
  Ptr Word8 -> Offset -> a -> IO Length
step !byte !next !ptr !off !val = do
  let
    !shbits = byte * 7
    !xx = val `shiftR` shbits
  if xx .&. complement 0x7F == 0 then do
    writeWord8OffPtr ptr (off + byte) (fromIntegral xx)
    return $! byte + 1
  else do
    writeWord8OffPtr ptr (off + byte) (0x80 .|. (fromIntegral xx .&. 0x7F))
    next ptr off val
{-# INLINE step #-}

stepLast :: (Bits a, Integral a) => Offset -> Ptr Word8 -> Offset -> a -> IO Length
stepLast !byte !ptr !off !val = do
  let
    !shbits = byte * 7
    !xx = val `shiftR` shbits
  writeWord8OffPtr ptr (off + byte) (fromIntegral xx)
  return $! byte + 1
{-# INLINE stepLast #-}

encodeVarWord16 :: Word16 -> EncodeBounded 3
encodeVarWord16 =
  encodeBounded .
  step 0 . step 1 $ stepLast 2
{-# INLINE encodeVarWord16 #-}

encodeVarWord32 :: Word32 -> EncodeBounded 5
encodeVarWord32 =
  encodeBounded .
  step 0 . step 1 . step 2 . step 3 $ stepLast 4
{-# INLINE encodeVarWord32 #-}

encodeVarWord64 :: Word64 -> EncodeBounded 10
encodeVarWord64 =
  encodeBounded .
  step 0 . step 1 . step 2 . step 3 . step 4 .
  step 5 . step 6 . step 7 . step 8 $ stepLast 9
{-# INLINE encodeVarWord64 #-}

encodeVarInt16 :: Int16 -> EncodeBounded 3
encodeVarInt16 =
  encodeVarWord16 . zigZag16
{-# INLINE encodeVarInt16 #-}

encodeVarInt32 :: Int32 -> EncodeBounded 5
encodeVarInt32 =
  encodeVarWord32 . zigZag32
{-# INLINE encodeVarInt32 #-}

encodeVarInt64 :: Int64 -> EncodeBounded 10
encodeVarInt64 =
  encodeVarWord64 . zigZag64
{-# INLINE encodeVarInt64 #-}

------------------------------------------------------------------------
-- Thrift

encodeBytes' :: forall bufsize. KnownNat bufsize => Strict.ByteString -> Encode bufsize
encodeBytes' (ByteString.PS !fp !bsoff0 !bslen0) =
  Encode $ \ flush (PtrOff !ptr0 !off0) ->
    withForeignPtr fp $ \ !bsptr ->
      let
        !vbufsize =
          fromNat (Proxy :: Proxy bufsize)

        loop !bsoff1 !bslen1 !ptr1 !off1 =
          let
            !remains = vbufsize - off1
          in
            if remains >= bslen1 then do
              ByteString.memcpy
                (ptr1 `plusPtr` off1)
                (bsptr `plusPtr` bsoff1)
                bslen1
              return $! PtrOff ptr1 (off1 + bslen1)
            else do
              ByteString.memcpy
                (ptr1 `plusPtr` off1)
                (bsptr `plusPtr` bsoff1)
                remains
              !ptr2 <- flush vbufsize
              loop (bsoff0 + remains) (bslen0 - remains) ptr2 0
      in
        loop bsoff0 bslen0 ptr0 off0
{-# INLINE encodeBytes' #-}

encodeBytes :: AtLeast 10 bufsize => Strict.ByteString -> Encode bufsize
encodeBytes bs =
  let
    !len = fromIntegral $! Strict.length bs
  in
    fromBounded (encodeVarWord64 len) <>
    encodeBytes' bs
{-# INLINE encodeBytes #-}

encodeList ::
  forall vv v bufsize.
  AtLeast 6 bufsize =>
  Generic.Vector vv v =>
  TypeId ->
  (v -> Encode bufsize) ->
  vv v ->
  Encode bufsize
encodeList (TypeId !tid) encodeElem !elems =
  Encode $ \ flush (PtrOff !ptr0 !off0) -> do
    let
      !len = Generic.length elems

      header :: Encode bufsize
      header =
        if len < 15 then
          fromBounded $
            encodeWord8 (fromIntegral len `shiftL` 4 .|. tid)
        else
          fromBounded $
            encodeWord8 (0xF0 .|. tid) <+>
            encodeVarWord32 (fromIntegral len)

      loop (PtrOff !ptr1 !off1) !x = do
        runEncode (encodeElem x) flush $! PtrOff ptr1 off1

    PtrOff !ptr1 !off1 <- runEncode header flush (PtrOff ptr0 off0)
    Generic.foldM loop (PtrOff ptr1 off1) elems
{-# INLINE encodeList #-}

encodeMap ::
  forall vk vv k v bufsize.
  AtLeast 6 bufsize =>
  Generic.Vector vk k =>
  Generic.Vector vv v =>
  TypeId ->
  TypeId ->
  (k -> Encode bufsize) ->
  (v -> Encode bufsize) ->
  Hybrid.Vector vk vv (k, v) ->
  Encode bufsize
encodeMap (TypeId !ktid) (TypeId !vtid) encodeKey encodeVal !kvs =
  Encode $ \ flush (PtrOff !ptr0 !off0) ->
    let
      !len = fromIntegral $! Generic.length kvs

      empty :: Encode bufsize
      empty =
        fromBounded $
          encodeWord8 0x0

      nonEmpty :: Encode bufsize
      nonEmpty =
        fromBounded $
          encodeVarWord32 len <+>
          encodeWord8 (ktid `shiftL` 4 .|. vtid)

      loop (PtrOff !ptr1 !off1) (!k, !v) = do
        PtrOff !ptr2 !off2 <- runEncode (encodeKey k) flush $! PtrOff ptr1 off1
        PtrOff !ptr3 !off3 <- runEncode (encodeVal v) flush $! PtrOff ptr2 off2
        return $! PtrOff ptr3 off3
    in
      if len == 0 then
        runEncode empty flush (PtrOff ptr0 off0)
      else do
        PtrOff !ptr1 !off1 <- runEncode nonEmpty flush (PtrOff ptr0 off0)
        Generic.foldM loop (PtrOff ptr1 off1) kvs
{-# INLINE encodeMap #-}
