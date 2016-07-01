{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.TypeId (
    typeIdOfThriftType
  , typeIdOfTypeReference
  , typeIdsOfTypeReference
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Language.Thrift.AST (TypeReference(..))

import           P hiding (Enum, Alt, exp)

import           Sardine.Compiler.Analysis
import           Sardine.Compiler.Error
import           Sardine.Compiler.Monad


typeIdOfThriftType :: ThriftType a -> Integer
typeIdOfThriftType = \case
  ThriftEnum _ ->
    0x5
  ThriftStruct _ ->
    0xC
  ThriftUnion _ ->
    0xC

typeIdOfTypeReference :: TypeReference a -> Compiler a Integer
typeIdOfTypeReference = \case
  SListType _ annot ->
    hoistCE (SListDeprecated annot)
  BoolType _ _ ->
    pure $ 0x1
  ByteType _ _ ->
    pure $ 0x3
  I16Type _ _ ->
    pure $ 0x4
  I32Type _ _ ->
    pure $ 0x5
  I64Type _ _ ->
    pure $ 0x6
  DoubleType _ _ ->
    pure $ 0x7
  StringType _ _ ->
    pure $ 0x8
  BinaryType _ _ ->
    pure $ 0x8
  ListType _ _ _ ->
    pure $ 0x9
  SetType _ _ _ ->
    pure $ 0xA
  MapType _ _ _ _ ->
    pure $ 0xB
  DefinedType nam annot ->
    typeIdOfThriftType <$> typeLookup nam annot

typeIdsOfTypeReference :: TypeReference a -> Compiler a (NonEmpty Integer)
typeIdsOfTypeReference = \case
  SListType _ annot ->
    hoistCE (SListDeprecated annot)
  BoolType _ _ ->
    pure $ 0x1 :| [0x2]
  ByteType _ _ ->
    pure $ 0x3 :| []
  I16Type _ _ ->
    pure $ 0x4 :| []
  I32Type _ _ ->
    pure $ 0x5 :| []
  I64Type _ _ ->
    pure $ 0x6 :| []
  DoubleType _ _ ->
    pure $ 0x7 :| []
  StringType _ _ ->
    pure $ 0x8 :| []
  BinaryType _ _ ->
    pure $ 0x8 :| []
  ListType _ _ _ ->
    pure $ 0x9 :| []
  SetType _ _ _ ->
    pure $ 0xA :| []
  MapType _ _ _ _ ->
    pure $ 0xB :| []
  DefinedType nam annot ->
    (:|[]) . typeIdOfThriftType <$> typeLookup nam annot
