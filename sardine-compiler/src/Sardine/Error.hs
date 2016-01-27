{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Error (
    SardineError(..)
  , renderSardineError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Trifecta.Delta (Delta)

import           P hiding (Const, Enum)

import           Sardine.Compiler.Error
import           Sardine.Haskell.Pretty

import           Text.PrettyPrint.ANSI.Leijen (Doc)


data SardineError =
    SardineParserError !Doc
  | SardineCompilerError !(CompilerError Delta)
  | SardinePrettyError !PrettyError
    deriving (Show)

renderSardineError :: SardineError -> Text
renderSardineError = \case
  SardineParserError doc ->
    T.pack (show doc)
  SardineCompilerError e ->
    renderCompilerError e
  SardinePrettyError e ->
    renderPrettyError e
