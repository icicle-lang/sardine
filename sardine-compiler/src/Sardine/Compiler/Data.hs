{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sardine.Compiler.Data (
    dataOfDefinition
  , boxedOfTypeReference
  , vectorOfTypeReference
  , emptyVectorOfTypeReference
  ) where

import           Control.Lens ((^.))

import           Language.Haskell.Exts.QQ (hs, ty)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Syntax as Haskell

import           Language.Thrift.Types (Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Union, Struct, Enum)
import           Language.Thrift.Types (EnumDef, Field, FieldRequiredness(..))
import           Language.Thrift.Types (fields, valueType, values)
import qualified Language.Thrift.Types as Thrift

import           P hiding (Enum)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Monad
import           Sardine.Compiler.Names
import           Sardine.Compiler.Util
import           Sardine.Haskell.Combinators


boxedOfTypeReference :: TypeReference a -> Compiler a Boxed
boxedOfTypeReference = \case
  DefinedType _ _ ->
    -- TODO lookup in env, maybe it's an unboxable struct
    pure Boxed
  StringType _ _ ->
    pure Boxed
  BinaryType _ _ ->
    pure Boxed
  SListType _ annot ->
    hoistCE (SListDeprecated annot)
  BoolType _ _ ->
    pure Unboxed
  ByteType _ _ ->
    pure Unboxed
  I16Type _ _ ->
    pure Unboxed
  I32Type _ _ ->
    pure Unboxed
  I64Type _ _ ->
    pure Unboxed
  DoubleType _ _ ->
    pure Unboxed
  MapType _ _ _ _ -> do
    pure Boxed
  SetType _ _ _ -> do
    pure Boxed
  ListType _ _ _ -> do
    pure Boxed

vectorOfTypeReference :: TypeReference a -> Compiler a Haskell.Type
vectorOfTypeReference tref = do
  boxed <- boxedOfTypeReference tref
  case boxed of
    Unboxed ->
      pure [ty| Unboxed.Vector |]
    Boxed ->
      pure [ty| Boxed.Vector |]

emptyVectorOfTypeReference :: TypeReference a -> Compiler a Haskell.Exp
emptyVectorOfTypeReference tref = do
  boxed <- boxedOfTypeReference tref
  case boxed of
    Unboxed ->
      pure [hs| Unboxed.empty |]
    Boxed ->
      pure [hs| Boxed.empty |]

haskellOfTypeReference :: TypeReference a -> Compiler a Haskell.Type
haskellOfTypeReference = \case
  DefinedType txt _ ->
    pure $ conT txt
  StringType _ _ ->
    pure $ conT "Text"
  BinaryType _ _ ->
    pure $ conT "ByteString"
  SListType _ annot ->
    hoistCE (SListDeprecated annot)
  BoolType _ _ ->
    pure $ conT "Bool"
  ByteType _ _ ->
    pure $ conT "Word8"
  I16Type _ _ ->
    pure $ conT "Int16"
  I32Type _ _ ->
    pure $ conT "Int32"
  I64Type _ _ ->
    pure $ conT "Int64"
  DoubleType _ _ ->
    pure $ conT "Double"
  MapType ttk ttv _ _ -> do
    tvk <- vectorOfTypeReference ttk
    tk <- haskellOfTypeReference ttk
    tvv <- vectorOfTypeReference ttv
    tv <- haskellOfTypeReference ttv
    pure $ [ty| Hybrid.Vector ((tvk)) ((tvv)) ( ((tk)) , ((tv)) ) |]
  SetType tt _ _ -> do
    tv <- vectorOfTypeReference tt
    t <- haskellOfTypeReference tt
    pure $ [ty| ((tv)) ((t)) |]
  ListType tt _ _ -> do
    tv <- vectorOfTypeReference tt
    t <- haskellOfTypeReference tt
    pure $ [ty| ((tv)) ((t)) |]

recFieldOfStructField :: Struct a -> Field a -> Compiler a ([Name], Haskell.Type)
recFieldOfStructField struct field = do
  ftype <- haskellOfTypeReference (field ^. valueType)

  let
    ftype' =
      case requiredness' field of
        Required ->
          ftype
        Optional ->
          conT "Maybe" `appT` ftype

    fname =
      nameOfStructField struct field

  pure ([fname], ftype')

stdDeriving :: [Deriving]
stdDeriving =
  [ (UnQual (Ident "Eq"), [])
  , (UnQual (Ident "Ord"), [])
  , (UnQual (Ident "Read"), [])
  , (UnQual (Ident "Show"), [])
  ]

dataOfStruct :: Struct a -> Compiler a Decl
dataOfStruct struct = do
  flds <- traverse (recFieldOfStructField struct) (struct ^. fields)

  let
    (fieldMod, dataOrNew) =
      if length (struct ^. fields) == 1 then
        (id, NewType)
      else
        (TyBang BangedTy, DataType)

    name =
      nameOfStruct struct

    conDecl =
      QualConDecl noLoc [] [] . RecDecl name $ fmap (second fieldMod) flds

  pure $
    DataDecl noLoc dataOrNew [] name [] [conDecl] stdDeriving

qconOfEnumDef :: Enum a -> EnumDef a -> QualConDecl
qconOfEnumDef enum def =
  QualConDecl noLoc [] [] $ ConDecl (nameOfEnumAlt enum def) []

dataOfEnum :: Enum a -> Compiler a Decl
dataOfEnum enum =
  if null (enum ^. values) then
    hoistCE (EnumIsUninhabited enum)
  else do
    let
      name = nameOfEnum enum
      qcons = fmap (qconOfEnumDef enum) (enum ^. values)
    pure $
      DataDecl noLoc DataType [] name [] qcons stdDeriving

qconOfUnionField :: Union a -> Field a -> Compiler a QualConDecl
qconOfUnionField union field =
  case requiredness' field of
    Optional ->
      hoistCE (UnionFieldsCannotBeOptional union field)
    Required -> do
      typ <- haskellOfTypeReference $ field ^. valueType
      pure $
        QualConDecl noLoc [] [] $ ConDecl (nameOfUnionAlt union field) [TyBang BangedTy typ]

dataOfUnion :: Union a -> Compiler a Decl
dataOfUnion union =
  if null (union ^. fields) then
    hoistCE (UnionIsUninhabited union)
  else do
    let
      tyName = nameOfUnion union
    qcons <- traverse (qconOfUnionField union) (union ^. fields)
    pure $
      DataDecl noLoc DataType [] tyName [] qcons stdDeriving

dataOfType :: Thrift.Type a -> Compiler a Decl
dataOfType = \case
  TypedefType x ->
    hoistCE (TypedefNotSupported x)
  EnumType x ->
    dataOfEnum x
  StructType x ->
    dataOfStruct x
  UnionType x ->
    dataOfUnion x
  SenumType x ->
    hoistCE (SenumDeprecated x)
  ExceptionType x ->
    hoistCE (ExceptionNotSupported x)

dataOfDefinition :: Definition a -> Compiler a Decl
dataOfDefinition = \case
  ConstDefinition x ->
    hoistCE (ConstNotSupported x)
  ServiceDefinition x ->
    hoistCE (ServiceNotSupported x)
  TypeDefinition x ->
    dataOfType x
