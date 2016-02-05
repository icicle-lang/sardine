{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sardine.Runtime.Buffer (
    Buffer
  , withBuffer
  , putBuffer
  ) where

import           Control.Exception (bracketOnError)
import           Control.Monad (return)

import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Word (Word8)

import           Foreign.ForeignPtr (newForeignPtr)
import           Foreign.Marshal.Alloc (free, finalizerFree)
import           Foreign.Marshal.Alloc (mallocBytes, reallocBytes)

import           GHC.Exts (Ptr(..), Int(..), isTrue#)
import           GHC.Prim (Addr#, plusAddr#, minusAddr#, leAddr#)
import           GHC.Prim (Int#, (+#), quotInt#)

import           Prelude (max, ($!))

import           System.IO (IO)

------------------------------------------------------------------------
-- Buffer

data BufferState =
  BufferState {
      _bufStart :: !Addr#
    , _bufPtr :: !Addr#
    , _bufEnd :: !Addr#
    }

newtype Buffer =
  Buffer {
      _unBuffer :: IORef BufferState
    }

newBuffer :: Int -> IO (Buffer, Ptr Word8, Ptr Word8)
newBuffer (I# len) = do
  !(Ptr ptr) <- mallocBytes (I# len)
  let
    !end = plusAddr# ptr len
  !ref <- newIORef (BufferState ptr ptr end)
  return $! (Buffer ref, Ptr ptr, Ptr end)
{-# INLINE newBuffer #-}

putBuffer :: Buffer -> Addr# -> Int# -> (Addr# -> Addr# -> IO ()) -> IO ()
putBuffer !(Buffer ref) srcend minBytes k = do
  !(BufferState dstart0 _ dend0) <- readIORef ref
  let
    !reqend = plusAddr# srcend minBytes
  if isTrue# (leAddr# reqend dend0) then do
    writeIORef ref $! BufferState dstart0 srcend dend0
    k srcend dend0
  else do
    let
      !doff0 = minusAddr# srcend dstart0
      !dlen0 = minusAddr# dend0 dstart0
      !dlen1 = dlen0 +# (quotInt# dlen0 2#)
      !dlen2 = dlen0 +# minBytes
      !(I# dmax) = max (I# dlen1) (I# dlen2)
    !(Ptr dstart1) <- mallocBytes (I# dmax)
    ByteString.memcpy (Ptr dstart1) (Ptr dstart0) (I# doff0)
    free (Ptr dstart0)
    let
      !dptr1 = plusAddr# dstart1 doff0
      !dend1 = plusAddr# dstart1 dmax
    writeIORef ref $! BufferState dstart1 dptr1 dend1
    k dptr1 dend1
{-# INLINE putBuffer #-}

closeBuffer :: Buffer -> IO Strict.ByteString
closeBuffer (Buffer !ref) = do
  !(BufferState start end _) <- readIORef ref
  let
    !len = I# (minusAddr# end start)
  !ptr <- reallocBytes (Ptr start) len
  !fp <- newForeignPtr finalizerFree ptr
  return $! ByteString.fromForeignPtr fp 0 len
{-# INLINE closeBuffer #-}

freeBuffer :: Buffer -> IO ()
freeBuffer (Buffer !ref) = do
  !(BufferState start _ _) <- readIORef ref
  free (Ptr start)
{-# INLINE freeBuffer #-}

withBuffer :: Int -> (Buffer -> Ptr Word8 -> Ptr Word8 -> IO ()) -> IO Strict.ByteString
withBuffer !len !io =
  bracketOnError (newBuffer len) (\(buf, _, _) -> freeBuffer buf) (\ !(!buf, !ptr, !end) -> do
    io buf ptr end
    closeBuffer buf)
{-# INLINE withBuffer #-}
