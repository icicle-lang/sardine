{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Compiler.Error (
    CompilerError(..)
  ) where

import           Data.Text (Text)

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
  | FieldMissingIdentifier !(Field a)
  | EnumIsUninhabited !(Enum a)
  | UnionIsUninhabited !(Union a)
    deriving (Eq, Ord, Show)
