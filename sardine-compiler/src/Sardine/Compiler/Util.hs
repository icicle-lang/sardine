{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Util (
    requiredness'
  , valuesOfEnum
  ) where

import           Control.Lens ((^.))

import qualified Data.List as List

import           Language.Thrift.Types (EnumDef(..))
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (requiredness)

import           P hiding (Enum, exp)


requiredness' :: Field a -> FieldRequiredness
requiredness' x =
  case x ^. requiredness of
    Nothing ->
      Required
    Just req ->
      req

valueOfEnum :: Integer -> Maybe Integer -> Integer
valueOfEnum ix0 = \case
  Nothing ->
    ix0 + 1
  Just ix1 ->
    ix1

valuesOfEnum :: [EnumDef a] -> [Integer]
valuesOfEnum =
  List.drop 1 . List.scanl valueOfEnum (-1) . fmap enumDefValue
