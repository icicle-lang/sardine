{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Sardine.Runtime.Base (
  -- * 7.8 Compatibility
    (<$!>)

  -- * Loops
  , replicateIx_

  -- * Type Level
  , AtLeast
  , fromNat

  -- ** Re-exports
  , Nat
  , KnownNat
  , Proxy(..)
  , type (+)
  , natVal
  ) where

import           Control.Monad (Monad(..))

import           Data.Bool (otherwise)
import           Data.Int (Int)
import           Data.Ord (Ord(..))
import           Data.Proxy (Proxy(..))

import           Prelude (Num(..), ($!), seq, fromIntegral)

import           GHC.TypeLits (KnownNat, Nat, type (<=), type (-), type (+), natVal)
import           GHC.Prim (Constraint)

------------------------------------------------------------------------
-- 7.8 Compatibility

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}

------------------------------------------------------------------------
-- Loops

replicateIx_ :: Monad m => Int -> (Int -> m ()) -> m ()
replicateIx_ n0 body0 =
  let
    loop !body !n !ix
      -- unrolled 4 times
      | ix + 4 <= n = do
        !_ <- body (ix+0)
        !_ <- body (ix+1)
        !_ <- body (ix+2)
        !_ <- body (ix+3)
        loop body n (ix+4)

      -- remaining elems
      | ix + 1 <= n = do
        !_ <- body (ix+0)
        loop body n (ix+1)

      -- finished
      | otherwise =
        return ()
  in
    loop body0 n0 0
{-# INLINE replicateIx_ #-}

------------------------------------------------------------------------
-- Type Level

type AtLeast (n :: Nat) (m :: Nat) =
  (KnownNat n, KnownNat m, AtLeast' n m)

type family AtLeast' (n :: Nat) (m :: Nat) :: Constraint where
  AtLeast' 0 m = ()
  AtLeast' n m = (n <= m, AtLeast' (n-1) m)

fromNat :: forall n proxy. KnownNat n => proxy (n :: Nat) -> Int
fromNat proxy =
  fromIntegral $! natVal proxy
