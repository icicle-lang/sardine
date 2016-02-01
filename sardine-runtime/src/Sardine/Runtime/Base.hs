{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Runtime.Base (
    -- * 7.8 Compatibility
    (<$!>)
  ) where

import           Control.Monad (Monad(..))

import           Prelude (seq)

------------------------------------------------------------------------
-- 7.8 Compatibility

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
{-# INLINE (<$!>) #-}
