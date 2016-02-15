{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Analysis (
    ThriftType(..)
  , typeEnvOfProgram
  ) where

import           Language.Thrift.Types (Program(..), Definition(..))
import           Language.Thrift.Types (Type(..))
import           Language.Thrift.Types (Enum(..), Union(..), Struct(..))
import qualified Language.Thrift.Types as Thrift

import           P hiding (Enum)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Environment


data ThriftType a =
    ThriftEnum (Enum a)
  | ThriftStruct (Struct a)
  | ThriftUnion (Union a)
    deriving (Eq, Ord, Show)

typeEnvOfType :: Thrift.Type a -> Either (CompilerError a) (Env (ThriftType a))
typeEnvOfType = \case
  TypedefType x ->
    Left (TypedefNotSupported x)
  EnumType x ->
    pure $ envSingleton (enumName x) (ThriftEnum x)
  StructType x ->
    pure $ envSingleton (structName x) (ThriftStruct x)
  UnionType x ->
    pure $ envSingleton (unionName x) (ThriftUnion x)
  SenumType x ->
    Left (SenumDeprecated x)
  ExceptionType x ->
    Left (ExceptionNotSupported x)

typeEnvOfDefinition :: Definition a -> Either (CompilerError a) (Env (ThriftType a))
typeEnvOfDefinition = \case
  ConstDefinition x ->
    Left (ConstNotSupported x)
  ServiceDefinition x ->
    Left (ServiceNotSupported x)
  TypeDefinition x ->
    typeEnvOfType x

typeEnvOfProgram :: Program a -> Either (CompilerError a) (Env (ThriftType a))
typeEnvOfProgram = \case
  Program _ defs ->
    envUnions <$> traverse typeEnvOfDefinition defs
