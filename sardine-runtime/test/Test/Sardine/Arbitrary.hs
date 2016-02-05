{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Sardine.Arbitrary (
    ThriftList(..)
  , ThriftMap(..)
  ) where

import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Hybrid as Hybrid

import           Sardine.Runtime.Thrift

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


newtype ThriftList vv v =
  ThriftList {
      unThriftList :: vv v
    } deriving (Eq, Show)

newtype ThriftMap kv vv k v =
  ThriftMap {
      unThriftMap :: Hybrid.Vector kv vv (k, v)
    } deriving (Eq, Show)

instance Arbitrary TypeId where
  arbitrary =
    TypeId <$> choose (0x1, 0xC)

  shrink = \case
    TypeId 0x1 ->
      []
    TypeId _ ->
      [TypeId 0x1]

instance (Generic.Vector vv v, Arbitrary v) => Arbitrary (ThriftList vv v) where
  arbitrary =
    ThriftList <$> (Generic.fromList <$> arbitrary)

  shrink (ThriftList vs) =
    [ThriftList (Generic.fromList vs') | vs' <- shrink (Generic.toList vs)]

instance (Generic.Vector kv k, Generic.Vector vv v, Arbitrary k, Arbitrary v) => Arbitrary (ThriftMap kv vv k v) where
  arbitrary =
    ThriftMap <$> (Hybrid.fromList <$> arbitrary)

  shrink (ThriftMap kvs) =
    [ThriftMap (Hybrid.fromList kvs') | kvs' <- shrink (Hybrid.toList kvs)]
