{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Ident (
    conidOfText
  , conidOfNamed
  , conidOfNamed'
  , varidOfNamed
  ) where

import           Control.Lens ((^.))

import           Data.Char (toUpper)
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Thrift.Types (HasName, name)
import           Language.Thrift.Types (HasSrcAnnot, srcAnnot)

import           P

import           Sardine.Compiler.Error
import           Sardine.Compiler.Pretty


conidOfText :: Maybe a -> Text -> Either (CompilerError a) Doc
conidOfText mannot txt =
  case T.uncons txt of
    Nothing ->
      Left $ InvalidName txt mannot
    Just (x, xs) ->
      Right . text $ toUpper x `T.cons` xs

conidOfNamed :: (HasName (s a), HasSrcAnnot s) => s a -> Either (CompilerError a) Doc
conidOfNamed s =
  conidOfText (Just (s ^. srcAnnot)) (s ^. name)

conidOfNamed' :: (HasName (s a), HasSrcAnnot s) => Doc -> s a -> Either (CompilerError a) Doc
conidOfNamed' prefix s = do
  conid <- conidOfNamed s
  Right $ prefix <> "_" <> conid

varidOfNamed :: (HasName (s a), HasSrcAnnot s) => Doc -> s a -> Either (CompilerError a) Doc
varidOfNamed prefix s = do
  conid <- conidOfNamed s
  Right $ takeAcronym prefix <> conid
