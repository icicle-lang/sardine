{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime.ZigZag (
  -- * Zig-zag Encoding
    zigZag16
  , zigZag32
  , zigZag64

  -- * Zig-zag Decoding
  , unZigZag16
  , unZigZag32
  , unZigZag64
  ) where

import           Data.Bits (Bits(..))
import           Data.Int (Int16, Int32, Int64)
import           Data.Word (Word16, Word32, Word64)

import           Prelude (Num(..), ($!), fromIntegral)

------------------------------------------------------------------------
-- Encoding

zigZag16 :: Int16 -> Word16
zigZag16 !n =
  fromIntegral $! (n `shiftL` 1) `xor` (n `shiftR` 15)
{-# INLINE zigZag16 #-}

zigZag32 :: Int32 -> Word32
zigZag32 !n =
  fromIntegral $! (n `shiftL` 1) `xor` (n `shiftR` 31)
{-# INLINE zigZag32 #-}

zigZag64 :: Int64 -> Word64
zigZag64 !n =
  fromIntegral $! (n `shiftL` 1) `xor` (n `shiftR` 63)
{-# INLINE zigZag64 #-}

------------------------------------------------------------------------
-- Decoding

unZigZag16 :: Word16 -> Int16
unZigZag16 !n =
  fromIntegral $! (n `shiftR` 1) `xor` negate (n .&. 0x1)
{-# INLINE unZigZag16 #-}

unZigZag32 :: Word32 -> Int32
unZigZag32 !n =
  fromIntegral $! (n `shiftR` 1) `xor` negate (n .&. 0x1)
{-# INLINE unZigZag32 #-}

unZigZag64 :: Word64 -> Int64
unZigZag64 !n =
  fromIntegral $! (n `shiftR` 1) `xor` negate (n .&. 0x1)
{-# INLINE unZigZag64 #-}
