{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Names (
    withPrefix

  , defaultE
  , decodeT
  , decodeE

  , nameOfStruct
  , typeOfStruct
  , conOfStruct
  , nameOfStructDefault
  , nameOfStructDecode
  , nameOfStructField

  , nameOfEnum
  , typeOfEnum
  , nameOfEnumDefault
  , nameOfEnumDecode
  , nameOfEnumAlt
  , conOfEnumAlt

  , nameOfUnion
  , typeOfUnion
  , nameOfUnionDefault
  , nameOfUnionDecode
  , nameOfUnionAlt
  , conOfUnionAlt
  ) where

import           Control.Lens ((^.))

import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Syntax as Haskell

import           Language.Thrift.Types (Struct, Enum, Union, Field, EnumDef)
import           Language.Thrift.Types (HasName(..))

import           P hiding (Enum)

import           Sardine.Haskell.Combinators

import           Text.Printf (printf)

--------------------------------------------------------------------------------

withPrefix :: Text -> Haskell.Name -> Haskell.Name
withPrefix prefix = \case
  Ident nam ->
    Ident $ T.unpack prefix <> nam
  Symbol sym ->
    -- prefixing symbols, savage
    Ident $ T.unpack prefix <> concatMap (printf "%02X" . ord) sym

nameOfField :: Text -> Text -> Haskell.Name
nameOfField ty field =
  Ident . T.unpack $ acronymOfText ty <> pascalOfText field

nameOfAlt :: Text -> Text -> Haskell.Name
nameOfAlt ty al =
  Ident . T.unpack $ pascalOfText ty <> "_" <> pascalOfText al

--------------------------------------------------------------------------------

defaultE :: Text -> Haskell.Exp
defaultE =
  Var . UnQual . nameOfDefaultN . conN

nameOfDefaultN :: Haskell.Name -> Haskell.Name
nameOfDefaultN =
  withPrefix "default"

decodeT :: Haskell.Type -> Haskell.Type
decodeT ty =
  conT "Decode" `appT` ty

decodeE :: Text -> Haskell.Exp
decodeE =
  Var . UnQual . nameOfDecodeN . conN

nameOfDecodeN :: Haskell.Name -> Haskell.Name
nameOfDecodeN =
  withPrefix "decode"

--------------------------------------------------------------------------------

nameOfStruct :: Struct a -> Haskell.Name
nameOfStruct struct =
  conN (struct ^. name)

typeOfStruct :: Struct a -> Haskell.Type
typeOfStruct =
  TyCon . UnQual . nameOfStruct

conOfStruct :: Struct a -> Haskell.Exp
conOfStruct struct =
  Con . UnQual $ nameOfStruct struct

nameOfStructDefault :: Struct a -> Haskell.Name
nameOfStructDefault =
  nameOfDefaultN . nameOfStruct

nameOfStructDecode :: Struct a -> Haskell.Name
nameOfStructDecode =
  nameOfDecodeN . nameOfStruct

nameOfStructField :: Struct a -> Field a -> Haskell.Name
nameOfStructField struct field =
  nameOfField (struct ^. name) (field ^. name)

--------------------------------------------------------------------------------

nameOfEnum :: Enum a -> Haskell.Name
nameOfEnum enum =
  conN (enum ^. name)

typeOfEnum :: Enum a -> Haskell.Type
typeOfEnum =
  TyCon . UnQual . nameOfEnum

nameOfEnumDefault :: Enum a -> Haskell.Name
nameOfEnumDefault =
  nameOfDefaultN . nameOfEnum

nameOfEnumDecode :: Enum a -> Haskell.Name
nameOfEnumDecode =
  nameOfDecodeN . nameOfEnum

nameOfEnumAlt :: Enum a -> EnumDef a -> Haskell.Name
nameOfEnumAlt enum def =
  nameOfAlt (enum ^. name) (def ^. name)

conOfEnumAlt :: Enum a -> EnumDef a -> Haskell.Exp
conOfEnumAlt enum def =
  Con . UnQual $ nameOfEnumAlt enum def

--------------------------------------------------------------------------------

nameOfUnion :: Union a -> Haskell.Name
nameOfUnion union =
  conN (union ^. name)

typeOfUnion :: Union a -> Haskell.Type
typeOfUnion =
  TyCon . UnQual . nameOfUnion

nameOfUnionDefault :: Union a -> Haskell.Name
nameOfUnionDefault =
  nameOfDefaultN . nameOfUnion

nameOfUnionDecode :: Union a -> Haskell.Name
nameOfUnionDecode =
  nameOfDecodeN . nameOfUnion

nameOfUnionAlt :: Union a -> Field a -> Haskell.Name
nameOfUnionAlt union field =
  nameOfAlt (union ^. name) (field ^. name)

conOfUnionAlt :: Union a -> Field a -> Haskell.Exp
conOfUnionAlt union field =
  Con . UnQual $ nameOfUnionAlt union field
