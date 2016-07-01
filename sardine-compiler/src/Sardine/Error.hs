{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Error (
    SardineError(..)
  , renderSardineError
  ) where

import qualified Data.Text as T

import           P hiding (Const, Enum)

import           Sardine.Compiler.Error
import           Sardine.Haskell.Pretty

import           Text.Megaparsec.Error (ParseError, Dec, parseErrorPretty)
import           Text.Megaparsec.Pos (SourcePos)


data SardineError =
    SardineParserError !(ParseError Char Dec)
  | SardineCompilerError !(CompilerError SourcePos)
  | SardinePrettyError !PrettyError
    deriving (Show)

renderSardineError :: SardineError -> Text
renderSardineError = \case
  SardineParserError err ->
    T.pack $ parseErrorPretty err
  SardineCompilerError e ->
    renderCompilerError e
  SardinePrettyError e ->
    renderPrettyError e
