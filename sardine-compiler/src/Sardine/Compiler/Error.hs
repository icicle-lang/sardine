{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Compiler.Error (
    CompilerError(..)
  , renderCompilerError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Thrift.Types (Service, Exception, Senum)
import           Language.Thrift.Types (Const, Typedef, Enum, Union, Field)

import           P hiding (Const, Enum)


data CompilerError a =
    ConstNotSupported !(Const a)
  | ServiceNotSupported !(Service a)
  | TypedefNotSupported !(Typedef a)
  | ExceptionNotSupported !(Exception a)
  | InvalidName !Text !(Maybe a)
  | SListDeprecated !a
  | SenumDeprecated !(Senum a)
  | FieldMissingId !(Field a)
  | FieldIdNotPositive !(Field a) !Integer
  | FieldIdTooLarge !(Field a) !Integer
  | EnumIsUninhabited !(Enum a)
  | UnionIsUninhabited !(Union a)
    deriving (Eq, Ord, Show)

renderCompilerError :: Show a => CompilerError a -> Text
renderCompilerError =
  T.pack . show
