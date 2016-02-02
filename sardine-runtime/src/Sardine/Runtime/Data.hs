{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Runtime.Data (
    TypeId(..)
  ) where

import           Data.Data (Data)
import           Data.Eq (Eq)
import           Data.Ord (Ord)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)

import           Text.Read (Read)
import           Text.Show (Show)

newtype TypeId =
  TypeId {
      unTypeId :: Word8
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

