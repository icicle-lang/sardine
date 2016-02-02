{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Sardine.Arbitrary () where

import           Sardine.Runtime.Data

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary TypeId where
  arbitrary =
    TypeId <$> choose (0x1, 0xC)

  shrink = \case
    TypeId 0x1 ->
      []
    TypeId _ ->
      [TypeId 0x1]
