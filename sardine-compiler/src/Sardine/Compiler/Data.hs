{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Data (
    dataOfDefinition
  ) where

import           Control.Lens ((^.))

import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.SrcLoc (noLoc)
import qualified Language.Haskell.Exts.Syntax as Haskell

import           Language.Thrift.Types (Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Union, Struct, Enum)
import           Language.Thrift.Types (EnumDef, Field, FieldRequiredness(..))
import           Language.Thrift.Types (HasName(..))
import           Language.Thrift.Types (fields, valueType, values)
import qualified Language.Thrift.Types as Thrift

import           P hiding (Enum)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Util


stdDeriving :: [Deriving]
stdDeriving =
  [ (UnQual (Ident "Eq"), [])
  , (UnQual (Ident "Ord"), [])
  , (UnQual (Ident "Read"), [])
  , (UnQual (Ident "Show"), [])
  ]

haskellOfTypeReference :: TypeReference a -> Either (CompilerError a) Haskell.Type
haskellOfTypeReference = \case
  DefinedType txt _ ->
    pure $ tyCon txt
  StringType _ _ ->
    pure $ tyCon "Text"
  BinaryType _ _ ->
    pure $ tyCon "ByteString"
  SListType _ annot ->
    Left (SListDeprecated annot)
  BoolType _ _ ->
    pure $ tyCon "Bool"
  ByteType _ _ ->
    pure $ tyCon "Word8"
  I16Type _ _ ->
    pure $ tyCon "Int16"
  I32Type _ _ ->
    pure $ tyCon "Int32"
  I64Type _ _ ->
    pure $ tyCon "Int64"
  DoubleType _ _ ->
    pure $ tyCon "Double"
  MapType ttk ttv _ _ -> do
    tk <- haskellOfTypeReference ttk
    tv <- haskellOfTypeReference ttv
    pure $ tyCon "Map" `TyApp` tk `TyApp` tv
  SetType tt _ _ -> do
    t <- haskellOfTypeReference tt
    pure $ tyCon "Set" `TyApp` t
  ListType tt _ _ -> do
    t <- haskellOfTypeReference tt
    pure $ tyCon "Vector" `TyApp` t

recFieldOfStructField :: Struct a -> Field a -> Either (CompilerError a) ([Name], Haskell.Type)
recFieldOfStructField struct field = do
  ftype <- haskellOfTypeReference (field ^. valueType)

  let
    ftype' =
      case requiredness' field of
        Required ->
          ftype
        Optional ->
          tyCon "Maybe" `TyApp` ftype

    fname =
      nameOfRecordField' struct field

  pure ([fname], ftype')

dataOfStruct :: Struct a -> Either (CompilerError a) Decl
dataOfStruct struct = do
  flds <- traverse (recFieldOfStructField struct) (struct ^. fields)

  let
    (fieldMod, dataOrNew) =
      if length (struct ^. fields) == 1 then
        (id, NewType)
      else
        (TyBang BangedTy, DataType)

    recName =
      nameOfType (struct ^. name)

    conDecl =
      QualConDecl noLoc [] [] . RecDecl recName $ fmap (second fieldMod) flds

  pure $
    DataDecl noLoc dataOrNew [] recName [] [conDecl] stdDeriving

qconOfEnumDef :: Enum a -> EnumDef a -> QualConDecl
qconOfEnumDef enum def =
  QualConDecl noLoc [] [] $ ConDecl (nameOfAlt enum def) []

dataOfEnum :: Enum a -> Either (CompilerError a) Decl
dataOfEnum enum =
  if null (enum ^. values) then
    Left (EnumIsUninhabited enum)
  else do
    let
      tyName = nameOfType (enum ^. name)
      qcons = fmap (qconOfEnumDef enum) (enum ^. values)
    pure $
      DataDecl noLoc DataType [] tyName [] qcons stdDeriving

qconOfUnionField :: Union a -> Field a -> Either (CompilerError a) QualConDecl
qconOfUnionField union field = do
  ty <- haskellOfTypeReference $ field ^. valueType
  pure $
    QualConDecl noLoc [] [] $ ConDecl (nameOfAlt union field) [TyBang BangedTy ty]

dataOfUnion :: Union a -> Either (CompilerError a) Decl
dataOfUnion union =
  if null (union ^. fields) then
    Left (UnionIsUninhabited union)
  else do
    let
      tyName = nameOfType (union ^. name)
    qcons <- traverse (qconOfUnionField union) (union ^. fields)
    pure $
      DataDecl noLoc DataType [] tyName [] qcons stdDeriving

dataOfType :: Thrift.Type a -> Either (CompilerError a) Decl
dataOfType = \case
  TypedefType x ->
    Left (TypedefNotSupported x)
  EnumType x ->
    dataOfEnum x
  StructType x ->
    dataOfStruct x
  UnionType x ->
    dataOfUnion x
  SenumType x ->
    Left (SenumDeprecated x)
  ExceptionType x ->
    Left (ExceptionNotSupported x)

dataOfDefinition :: Definition a -> Either (CompilerError a) Decl
dataOfDefinition = \case
  ConstDefinition x ->
    Left (ConstNotSupported x)
  ServiceDefinition x ->
    Left (ServiceNotSupported x)
  TypeDefinition x ->
    dataOfType x
