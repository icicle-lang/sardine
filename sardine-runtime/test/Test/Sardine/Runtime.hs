{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Sardine.Runtime where

import           Data.ByteString (ByteString)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Hybrid as Hybrid
import qualified Data.Vector.Unboxed as Unboxed

import           Disorder.Core.Tripping

import           Sardine.Runtime

import           P

import           Test.Sardine.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_Word8 =
  trippingEncodeBounded encodeWord8 decodeWord8

prop_Word16 =
  trippingEncodeBounded encodeWord16le decodeWord16le

prop_Word32 =
  trippingEncodeBounded encodeWord32le decodeWord32le

prop_Word64 =
  trippingEncodeBounded encodeWord64le decodeWord64le

prop_Int8 =
  trippingEncodeBounded encodeInt8 decodeInt8

prop_Int16 =
  trippingEncodeBounded encodeInt16le decodeInt16le

prop_Int32 =
  trippingEncodeBounded encodeInt32le decodeInt32le

prop_Int64 =
  trippingEncodeBounded encodeInt64le decodeInt64le

prop_VarWord16 =
  trippingEncodeBounded encodeVarWord16 decodeVarWord16

prop_VarWord32 =
  trippingEncodeBounded encodeVarWord32 decodeVarWord32

prop_VarWord64 =
  trippingEncodeBounded encodeVarWord64 decodeVarWord64

prop_VarInt16 =
  trippingEncodeBounded encodeVarInt16 decodeVarInt16

prop_VarInt32 =
  trippingEncodeBounded encodeVarInt32 decodeVarInt32

prop_VarInt64 =
  trippingEncodeBounded encodeVarInt64 decodeVarInt64

prop_Bytes =
  trippingEncode encodeBytes decodeBytes

prop_List :: TypeId -> Unboxed.Vector Int64 -> Property
prop_List tid =
  trippingEncode
    (encodeList tid (fromBounded . encodeVarInt64))
    (decodeList (checkTid tid) decodeVarInt64)

prop_Map :: TypeId -> TypeId -> Boxed.Vector (ByteString, Int64) -> Property
prop_Map ktid vtid kvs =
  trippingEncode
    (encodeMap ktid vtid encodeBytes (fromBounded . encodeVarInt64))
    (decodeMap (checkTid ktid) (checkTid vtid) decodeBytes decodeVarInt64)
    (Hybrid.unsafeZip (fmap fst kvs) (fmap snd kvs))

checkTid :: TypeId -> TypeId -> Decode ()
checkTid tid otid =
  unless (tid == otid) $
    decodeFail (DecodeInvalidType otid)

trippingEncode :: (Show a, Eq a) => (a -> Encode 1024) -> Decode a -> a -> Property
trippingEncode enc dec =
  tripping (encodeStrictByteString . buffer1024 . enc) (decodeStrictByteString dec)

trippingEncodeBounded ::
  AtLeast n 1024 =>
  (Arbitrary a, Show a, Eq a) =>
  (a -> EncodeBounded n) ->
  Decode a ->
  Property
trippingEncodeBounded enc dec =
  tripping (encodeStrictByteString . buffer1024 . fromBounded . enc) (decodeStrictByteString dec) .&&.
  tripping (encodeStrictByteString' . enc) (decodeStrictByteString dec)

buffer1024 :: Encode 1024 -> Encode 1024
buffer1024 =
  id

return []
tests =
  $quickCheckAll
