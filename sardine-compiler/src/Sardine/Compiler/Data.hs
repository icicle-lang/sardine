{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Data (
    dataOfProgram
  ) where

import           Control.Lens ((^.))

import           Language.Thrift.Types (Program(..), Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Enum, Union, Struct)
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (fields, requiredness, valueType, values)

import           P hiding (Enum)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Ident
import           Sardine.Compiler.Pretty


dataOfTypeReference :: TypeReference a -> Either (CompilerError a) Doc
dataOfTypeReference = \case
  DefinedType txt annot ->
    conidOfText (Just annot) txt
  StringType _ _ ->
    Right $ text "Text"
  BinaryType _ _ ->
    Right $ text "ByteString"
  SListType _ annot ->
    Left (SListDeprecated annot)
  BoolType _ _ ->
    Right $ text "Bool"
  ByteType _ _ ->
    Right $ text "Word8"
  I16Type _ _ ->
    Right $ text "Int16"
  I32Type _ _ ->
    Right $ text "Int32"
  I64Type _ _ ->
    Right $ text "Int64"
  DoubleType _ _ ->
    Right $ text "Double"
  MapType trk trv _ _ -> do
    tk <- withParens <$> dataOfTypeReference trk
    tv <- withParens <$> dataOfTypeReference trv
    Right $ text "Map" <+> tk <+> tv
  SetType tr _ _ -> do
    t <- withParens <$> dataOfTypeReference tr
    Right $ text "Set" <+> t
  ListType tr _ _ -> do
    t <- withParens <$> dataOfTypeReference tr
    Right $ text "Vector" <+> t

dataOfStructField :: Eval -> Doc -> Field a -> Either (CompilerError a) Doc
dataOfStructField eval prefix x = do
  varid <- varidOfNamed prefix x
  ftype <- dataOfTypeReference (x ^. valueType)
  Right $
    case x ^. requiredness of
      Nothing ->
        varid <+> ":: " <> withEval eval ftype
      Just Required ->
        varid <+> ":: " <> withEval eval ftype
      Just Optional ->
        varid <+> ":: " <> withEval eval ("Maybe" <+> withParens ftype <> "")

dataOfStruct :: Struct a -> Either (CompilerError a) Doc
dataOfStruct x = do
  let
    (eval, style) =
      if length (x ^. fields) == 1 then
        (Default, "newtype")
      else
        (Strict, "data")
  conid <- conidOfNamed x
  flds  <- traverse (dataOfStructField eval conid) (x ^. fields)
  Right $
    style <+> conid <+> "=" <&>
    "  " <> conid <+> "{" <&>
    "    " <> encloseVSep "  " "} deriving (Eq, Ord, Read, Show)" ", " flds

dataOfEnum :: Enum a -> Either (CompilerError a) Doc
dataOfEnum x =
  if null (x ^. values) then
    Left (EnumIsUninhabited x)
  else do
    conid <- conidOfNamed x
    cons  <- traverse (conidOfNamed' conid) (x ^. values)
    Right $
      "data" <+> conid <+> "=" <&>
      "  " <> encloseVSep "  " ("  deriving (Eq, Ord, Read, Show)") "| " cons

dataOfUnionField :: Doc -> Field a -> Either (CompilerError a) Doc
dataOfUnionField prefix x = do
  conid <- conidOfNamed' prefix x
  ftype <- dataOfTypeReference (x ^. valueType)
  Right $
    conid <+> withEval Strict ftype

dataOfUnion :: Union a -> Either (CompilerError a) Doc
dataOfUnion x =
  if null (x ^. fields) then
    Left (UnionIsUninhabited x)
  else do
    conid <- conidOfNamed x
    cons  <- traverse (dataOfUnionField conid) (x ^. fields)
    Right $
      "data" <+> conid <+> "=" <&>
      "  " <> encloseVSep "  " ("  deriving (Eq, Ord, Read, Show)") "| " cons

dataOfType :: Type a -> Either (CompilerError a) Doc
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

dataOfDefinition :: Definition a -> Either (CompilerError a) Doc
dataOfDefinition = \case
  ConstDefinition x ->
    Left (ConstNotSupported x)
  ServiceDefinition x ->
    Left (ServiceNotSupported x)
  TypeDefinition x ->
    dataOfType x

dataOfProgram :: Program a -> Either (CompilerError a) Doc
dataOfProgram = \case
  Program _ defs ->
    vsep . punctuate line <$> traverse dataOfDefinition defs
