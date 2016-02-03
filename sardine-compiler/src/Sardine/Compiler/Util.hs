{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Util (
    requiredness'
  , valuesOfEnum
  , takeFieldId
  , sortByFieldId
  , sortByFieldId'
  , prefixPE
  , lazyPrefixPE
  ) where

import           Control.Lens ((^.))

import           Data.Text (Text)
import qualified Data.List as List

import           Language.Haskell.Exts.Syntax (Exp, Pat)

import           Language.Thrift.Types (EnumDef(..))
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (requiredness, identifier, name)

import           Sardine.Compiler.Error
import           Sardine.Haskell.Combinators
import           Sardine.Compiler.Monad

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

takeFieldId :: Field a -> Compiler a Integer
takeFieldId field =
  case field ^. identifier of
    Just i | i > 0 ->
      pure i
    Just i ->
      hoistCE (FieldIdNotPositive field i)
    Nothing ->
      hoistCE (FieldMissingId field)

sortByFieldId :: [Field a] -> [Field a]
sortByFieldId =
  sortByFieldId' id

sortByFieldId' :: (a -> Field x) -> [a] -> [a]
sortByFieldId' f =
  List.sortBy (comparing (\x -> f x ^. identifier))

prefixPE :: Text -> Field a -> (Pat, Exp)
prefixPE pfx field =
  ( bangP . varP $ pfx <> field ^. name
  , varE $ pfx <> field ^. name )

lazyPrefixPE :: Text -> Field a -> (Pat, Exp)
lazyPrefixPE pfx field =
  ( varP $ pfx <> field ^. name
  , varE $ pfx <> field ^. name )
