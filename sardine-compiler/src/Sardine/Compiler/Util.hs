{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Util (
    pascalOfText
  , moduleOfText
  , acronymOfText
  , nameOfType
  , tyCon
  , nameOfRecordField
  , nameOfRecordField'
  , nameOfAlt
  , requiredness'
  ) where

import           Control.Lens ((^.))

import           Data.Char (toUpper, isUpper, isNumber)
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Syntax as Haskell

import           Language.Thrift.Types (Struct)
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (HasName(..))
import           Language.Thrift.Types (requiredness)

import           P


pascalOfText :: Text -> Text
pascalOfText txt =
  case T.uncons txt of
    Nothing ->
      T.empty
    Just (x, xs) ->
      toUpper x `T.cons` xs

moduleOfText :: Text -> Text
moduleOfText =
  T.intercalate "." . fmap pascalOfText . T.splitOn "."

acronymOfText :: Text -> Text
acronymOfText txt =
  case T.uncons txt of
    Nothing ->
      T.empty
    Just (x, xs) ->
      T.toLower $ x `T.cons` T.filter (\c -> isUpper c || isNumber c) xs

nameOfType :: Text -> Haskell.Name
nameOfType =
  Ident . T.unpack . pascalOfText

tyCon :: Text -> Haskell.Type
tyCon =
  TyCon . UnQual . nameOfType

nameOfAlt :: (HasName (ty a), HasName (alt a)) => ty a -> alt a -> Haskell.Name
nameOfAlt ty alt =
  Ident . T.unpack $ pascalOfText (ty ^. name) <> "_" <> pascalOfText (alt ^. name)

nameOfRecordField :: Text -> Text -> Haskell.Name
nameOfRecordField ty field =
  Ident . T.unpack $ acronymOfText ty <> pascalOfText field

nameOfRecordField' :: Struct a -> Field a -> Haskell.Name
nameOfRecordField' struct field =
  nameOfRecordField (struct ^. name) (field ^. name)

requiredness' :: Field a -> FieldRequiredness
requiredness' x =
  case x ^. requiredness of
    Nothing ->
      Required
    Just req ->
      req
