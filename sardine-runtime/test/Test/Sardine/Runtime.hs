{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Sardine.Runtime where

import qualified Data.ByteString as Strict
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed

import           Disorder.Core.Tripping

import           Sardine.Runtime

import           P hiding (Bounded)

import           Test.Sardine.Arbitrary

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_Word8 =
  trippingEncode encodeWord8 decodeWord8

prop_Word16 =
  trippingEncode encodeWord16le decodeWord16le

prop_Word32 =
  trippingEncode encodeWord32le decodeWord32le

prop_Word64 =
  trippingEncode encodeWord64le decodeWord64le

prop_Int8 =
  trippingEncode encodeInt8 decodeInt8

prop_Int16 =
  trippingEncode encodeInt16le decodeInt16le

prop_Int32 =
  trippingEncode encodeInt32le decodeInt32le

prop_Int64 =
  trippingEncode encodeInt64le decodeInt64le

prop_VarWord16 =
  trippingEncode encodeVarWord16 decodeVarWord16

prop_VarWord32 =
  trippingEncode encodeVarWord32 decodeVarWord32

prop_VarWord64 =
  trippingEncode encodeVarWord64 decodeVarWord64

prop_VarInt16 =
  trippingEncode encodeVarInt16 decodeVarInt16

prop_VarInt32 =
  trippingEncode encodeVarInt32 decodeVarInt32

prop_VarInt64 =
  trippingEncode encodeVarInt64 decodeVarInt64

prop_Binary x =
  trippingEncode encodeThriftBinary decodeThriftBinary x

prop_String =
  trippingEncode encodeThriftString decodeThriftString

prop_List :: TypeId -> ThriftList Unboxed.Vector Int64 -> Property
prop_List tid (ThriftList xs) =
  let
    encode = encodeThriftList tid encodeVarInt64
    decode = decodeThriftList (checkTid tid) (first ThriftVarIntError decodeVarInt64)
  in
    trippingEncode encode decode xs

prop_BytesList :: TypeId -> ThriftList Boxed.Vector Strict.ByteString -> Property
prop_BytesList tid (ThriftList xs) =
  let
    encode = encodeThriftList tid encodeThriftBinary
    decode = decodeThriftList (checkTid tid) decodeThriftBinary
  in
    trippingEncode encode decode xs

prop_Map :: TypeId -> TypeId -> ThriftMap Boxed.Vector Unboxed.Vector Strict.ByteString Int64 -> Property
prop_Map ktid vtid (ThriftMap kvs) =
  let
    encode = encodeThriftMap ktid vtid encodeThriftBinary encodeVarInt64
    decode = decodeThriftMap (checkTid ktid) (checkTid vtid) decodeThriftBinary (first ThriftVarIntError decodeVarInt64)
  in
    trippingEncode encode decode kvs

checkTid :: TypeId -> TypeId -> Decode ThriftError ()
checkTid tid otid =
  unless (tid == otid) $
    decodeFail (ThriftInvalidType otid)

trippingEncode :: (Show x, Show a, Eq x, Eq a) => Encode a -> Decode x a -> a -> Property
trippingEncode enc dec =
  tripping (runEncodeStrict' enc) (runDecodeStrict dec)

return []
tests =
  $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
