{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime.Thrift (
  -- * Data
    TypeId(..)
  , ThriftError(..)

  -- * Primitives
  , encodeThriftBool
  , encodeThriftByte
  , encodeThriftI16
  , encodeThriftI32
  , encodeThriftI64
  , encodeThriftDouble
  , decodeThriftBool
  , decodeThriftByte
  , decodeThriftI16
  , decodeThriftI32
  , decodeThriftI64
  , decodeThriftDouble

  -- * Binary
  , encodeThriftBinary
  , decodeThriftBinary

  -- * String
  , encodeThriftString
  , decodeThriftString

  -- * List
  , encodeThriftList
  , decodeThriftList

  -- * Map
  , encodeThriftMap
  , decodeThriftMap

  -- * Skipping
  , skipThriftType
  , skipThriftList
  , skipThriftMap
  , skipThriftStruct
  ) where

import           Control.Monad (return, replicateM_)

import           Data.Bifunctor (Bifunctor(..))
import           Data.Bits (Bits(..))
import           Data.Bool (Bool(..), bool)
import qualified Data.ByteString as B
import qualified Data.ByteString as Strict (ByteString)
import           Data.Data (Data)
import           Data.Eq (Eq(..))
import           Data.Function ((.), id)
import           Data.Functor ((<$>))
import           Data.Functor.Contravariant ((>$<))
import           Data.Int (Int16, Int32, Int64)
import           Data.Ord (Ord(..))
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           Data.Vector.Generic (Mutable)
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Hybrid as Hybrid
import           Data.Void (absurd)
import           Data.Word (Word8)

import           GHC.Exts (Int(..), isTrue#)
import           GHC.Prim ((+#), (==#), (<#))

import           Prelude (Num(..), Double, ($), ($!), fromIntegral)

import           Sardine.Runtime.Internal
import           Sardine.Runtime.Primitive
import           Sardine.Runtime.VarInt

import           Text.Show (Show(..))
import           Text.Read (Read(..))

------------------------------------------------------------------------
-- Data

newtype TypeId =
  TypeId {
      unTypeId :: Word8
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

data ThriftError =
    ThriftVarIntError !VarIntError
  | ThriftInvalidType !TypeId
  -- Errors below used by generated code --
  | ThriftInvalidEnumValue !Strict.Text !Int32
  | ThriftInvalidBoolValue !Word8
  | ThriftInvalidFieldType !Strict.Text !Strict.Text !TypeId
  | ThriftMissingField !Strict.Text !Strict.Text
  | ThriftMissingUnionAlt !Strict.Text
  | ThriftMultipleUnionAlts !Strict.Text
  | ThriftUnknownUnionAlt !Strict.Text !Int16
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Primitives

encodeThriftBool :: Encode Bool
encodeThriftBool =
  bool 0x2 0x1 >$< encodeWord8
{-# INLINE encodeThriftBool #-}

decodeThriftBool :: Decode ThriftError Bool
decodeThriftBool = do
  !w8 <- first absurd decodeWord8
  case w8 of
    0x1 ->
      return True
    0x2 ->
      return False
    _ ->
      decodeFail $ ThriftInvalidBoolValue w8
{-# INLINE decodeThriftBool #-}

encodeThriftByte :: Encode Word8
encodeThriftByte =
  encodeWord8
{-# INLINE encodeThriftByte #-}

decodeThriftByte :: Decode ThriftError Word8
decodeThriftByte =
  first absurd decodeWord8
{-# INLINE decodeThriftByte #-}

encodeThriftI16 :: Encode Int16
encodeThriftI16 =
  encodeVarInt16
{-# INLINE encodeThriftI16 #-}

decodeThriftI16 :: Decode ThriftError Int16
decodeThriftI16 =
  first ThriftVarIntError decodeVarInt16
{-# INLINE decodeThriftI16 #-}

encodeThriftI32 :: Encode Int32
encodeThriftI32 =
  encodeVarInt32
{-# INLINE encodeThriftI32 #-}

decodeThriftI32 :: Decode ThriftError Int32
decodeThriftI32 =
  first ThriftVarIntError decodeVarInt32
{-# INLINE decodeThriftI32 #-}

encodeThriftI64 :: Encode Int64
encodeThriftI64 =
  encodeVarInt64
{-# INLINE encodeThriftI64 #-}

decodeThriftI64 :: Decode ThriftError Int64
decodeThriftI64 =
  first ThriftVarIntError decodeVarInt64
{-# INLINE decodeThriftI64 #-}

encodeThriftDouble :: Encode Double
encodeThriftDouble =
  encodeFloat64le
{-# INLINE encodeThriftDouble #-}

decodeThriftDouble :: Decode ThriftError Double
decodeThriftDouble =
  first absurd decodeFloat64le
{-# INLINE decodeThriftDouble #-}

------------------------------------------------------------------------
-- Binary

encodeThriftBinary :: Encode Strict.ByteString
encodeThriftBinary =
  Encode $ \x ->
    let
      !len = fromIntegral (B.length x)
    in
      runEncode encodeVarWord32 len .
      runEncode encodeBytes x
{-# INLINE encodeThriftBinary #-}

decodeThriftBinary :: Decode ThriftError Strict.ByteString
decodeThriftBinary = do
  !len <- first ThriftVarIntError decodeVarWord32
  first absurd $ decodeBytes (fromIntegral len)
{-# INLINE decodeThriftBinary #-}

------------------------------------------------------------------------
-- String

encodeThriftString :: Encode Strict.Text
encodeThriftString =
  T.encodeUtf8 >$< encodeThriftBinary
{-# INLINE encodeThriftString #-}

decodeThriftString :: Decode ThriftError Strict.Text
decodeThriftString =
  T.decodeUtf8 <$> decodeThriftBinary
{-# INLINE decodeThriftString #-}

------------------------------------------------------------------------
-- List

encodeThriftList :: Generic.Vector vv v => TypeId -> Encode v -> Encode (vv v)
encodeThriftList (TypeId tid) encodeElem =
  Encode $ \xs ->
    let
      !(I# len) = Generic.length xs

      header =
        if isTrue# (len <# 15#) then
          let
            !tag = (fromIntegral (I# len) `shiftL` 4) .|. tid
          in
            runEncode encodeWord8 tag .
            loop 0#
        else
          let
            !tag = 0xF0 .|. tid
            !size = fromIntegral (I# len)
          in
            runEncode encodeWord8 tag .
            runEncode encodeVarWord32 size .
            loop 0#

      loop ix =
        if isTrue# (ix ==# len) then
          id
        else
          let
            !x = Generic.unsafeIndex xs (I# ix)
          in
            runEncode encodeElem x .
            loop (ix +# 1#)
    in
      header
{-# INLINE encodeThriftList #-}

decodeThriftList ::
  Generic.Vector vv v =>
  MGeneric.MVector (Mutable vv) v =>
  (TypeId -> Decode ThriftError ()) ->
  Decode ThriftError v ->
  Decode ThriftError (vv v)
decodeThriftList checkType decodeElem = do
  !tag <- first absurd decodeWord8

  let
    !lsize = tag `shiftR` 4

  !size <-
    if lsize == 0xF then
      fromIntegral <$> first ThriftVarIntError decodeVarWord32
    else
      return $! fromIntegral lsize

  !mv <- MGeneric.unsafeNew size

  let
    !tid = TypeId $ tag .&. 0x0F

  checkType tid

  replicateIx_ size $ \ix -> do
    !elem <- decodeElem
    MGeneric.unsafeWrite mv ix elem
  Generic.unsafeFreeze mv
{-# INLINE decodeThriftList #-}

------------------------------------------------------------------------
-- Map

encodeThriftMap ::
  Generic.Vector vk k =>
  Generic.Vector vv v =>
  TypeId ->
  TypeId ->
  Encode k ->
  Encode v ->
  Encode (Hybrid.Vector vk vv (k, v))
encodeThriftMap (TypeId ktid) (TypeId vtid) encodeKey encodeVal =
  Encode $ \kvs ->
    let
      !(I# len) = Hybrid.length kvs
      !ks = Hybrid.projectFst kvs
      !vs = Hybrid.projectSnd kvs

      header =
        if isTrue# (len ==# 0#) then
          let
            !size = 0
          in
            runEncode encodeWord8 size
        else
          let
            !size = fromIntegral (I# len)
            !tag = (ktid `shiftL` 4) .|. vtid
          in
            runEncode encodeVarWord32 size .
            runEncode encodeWord8 tag .
            loop 0#

      loop ix =
        if isTrue# (ix ==# len) then
          id
        else
          let
            !k = Generic.unsafeIndex ks (I# ix)
            !v = Generic.unsafeIndex vs (I# ix)
          in
            runEncode encodeKey k .
            runEncode encodeVal v .
            loop (ix +# 1#)
    in
      header
{-# INLINE encodeThriftMap #-}

decodeThriftMap ::
  Generic.Vector vk k =>
  Generic.Vector vv v =>
  MGeneric.MVector (Mutable vk) k =>
  MGeneric.MVector (Mutable vv) v =>
  (TypeId -> Decode ThriftError ()) ->
  (TypeId -> Decode ThriftError ()) ->
  Decode ThriftError k ->
  Decode ThriftError v ->
  Decode ThriftError (Hybrid.Vector vk vv (k, v))
decodeThriftMap checkKeyType checkValType decodeKey decodeVal = do
  !size <- fromIntegral <$> first ThriftVarIntError decodeVarWord32
  if size == 0 then
    return Hybrid.empty
  else do
    !tag <- first absurd decodeWord8

    !mvk <- MGeneric.unsafeNew size
    !mvv <- MGeneric.unsafeNew size

    let
      !ktid = TypeId $! tag `shiftR` 4
      !vtid = TypeId $! tag .&. 0x0F

    checkKeyType ktid
    checkValType vtid

    replicateIx_ size $! \ !ix -> do
      !k <- decodeKey
      MGeneric.unsafeWrite mvk ix k
      !v <- decodeVal
      MGeneric.unsafeWrite mvv ix v

    !vk <- Generic.unsafeFreeze mvk
    !vv <- Generic.unsafeFreeze mvv
    return $! Hybrid.unsafeZip vk vv
{-# INLINE decodeThriftMap #-}

------------------------------------------------------------------------
-- Skipping

skipThriftType :: TypeId -> Decode ThriftError ()
skipThriftType (TypeId tid) =
  case tid of
    0x1 ->
      return () -- bool/true
    0x2 ->
      return () -- bool/false
    0x3 -> do
      _ <- first absurd decodeWord8
      return ()
    0x4 -> do
      _ <- first ThriftVarIntError decodeVarInt16
      return ()
    0x5 -> do
      _ <- first ThriftVarIntError decodeVarInt32
      return ()
    0x6 -> do
      _ <- first ThriftVarIntError decodeVarInt64
      return ()
    0x7 -> do
      _ <- first absurd decodeFloat64le
      return ()
    0x8 -> do
      _ <- decodeThriftBinary
      return ()
    0x9 ->
      skipThriftList -- list
    0xA ->
      skipThriftList -- set
    0xB ->
      skipThriftMap
    0xC ->
      skipThriftStruct
    _ ->
      decodeFail (ThriftInvalidType (TypeId tid))
{-# INLINE skipThriftType #-}

skipThriftList :: Decode ThriftError ()
skipThriftList = do
  !tag <- first absurd decodeWord8
  let
    !tid = TypeId $! tag .&. 0x0F
    !lsize = tag `shiftR` 4
  !size <-
    if lsize == 0xF then
      fromIntegral <$> first ThriftVarIntError decodeVarInt32
    else
      return $! fromIntegral lsize
  replicateM_ size $ do
    skipThriftType tid
{-# INLINE skipThriftList #-}

skipThriftMap :: Decode ThriftError ()
skipThriftMap = do
  !size <- fromIntegral <$> first ThriftVarIntError decodeVarInt32
  if size == 0 then
    return ()
  else do
    !tag <- first absurd decodeWord8
    let
      !kt = TypeId (tag `shiftR` 4)
      !vt = TypeId (tag .&. 0x0F)
    replicateM_ size $ do
      skipThriftType kt
      skipThriftType vt
{-# INLINE skipThriftMap #-}

skipThriftStruct :: Decode ThriftError ()
skipThriftStruct =
  let
    loop !last = do
      !tag <- first absurd decodeWord8
      if tag == 0 then
        return ()
      else do
        let
          !tid = TypeId (tag .&. 0x0F)
          !mod = tag `shiftR` 4
        !ident <-
          if mod /= 0 then
            return $! last + fromIntegral mod
          else
            first absurd decodeInt16le
        skipThriftType tid
        loop ident
  in
    loop 0
{-# INLINE skipThriftStruct #-}
