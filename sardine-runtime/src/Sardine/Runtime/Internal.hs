{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sardine.Runtime.Internal (
  -- * Core
    SrcAddr
  , DstAddr
  , EndAddr
  , MinBytes
  , Push
  , Pull
  , EncodeIO(..)
  , DecodeIO(..)
  , Encode(..)
  , Decode(..)
  , decodeFail
  , finishEncode

  -- * Running
  , runEncodeStrict
  , runEncodeStrict'
  , runDecodeStrict

  -- * Errors
  , DecodeError(..)
  , renderDecodeError

  -- * Util
  , unsafeEncodeSized
  , unsafeEncodeFixed
  , unsafeDecodeFixed

  -- * 7.8 Compatibility
  , (<$!>)

  -- * Loops
  , replicateIx_
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Monad (Monad, return, (>>=))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad(..))

import           Data.Bifunctor (Bifunctor(..))
import           Data.Bool (otherwise)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Either (Either(..))
import           Data.Eq (Eq(..))
import           Data.Function ((.), const, id)
import           Data.Functor (Functor(..))
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..))
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.Int (Int)
import           Data.Ord (Ord(..))
import           Data.Text (Text)
import           Data.Void (absurd)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Unsafe (unsafeLocalState)

import           GHC.Exts (Ptr(..), Int(..), isTrue#)
import           GHC.Prim (Addr#, plusAddr#, minusAddr#)
import           GHC.Prim (Int#, (>=#))

import           Prelude (Num(..), ($), ($!), seq)

import           Sardine.Runtime.Buffer

import           System.IO (IO)

import           Text.Show (Show(..))

------------------------------------------------------------------------
-- Data

type SrcAddr =
  Addr#

type DstAddr =
  Addr#

type EndAddr =
  Addr#

type MinBytes =
  Int#

type Push =
  DstAddr -> MinBytes -> (DstAddr -> EndAddr -> IO ()) -> IO ()

type Pull =
  SrcAddr -> MinBytes -> (SrcAddr -> EndAddr -> IO ()) -> IO ()

newtype EncodeIO =
  EncodeIO {
      runEncodeIO :: Push -> DstAddr -> EndAddr -> IO ()
    }

newtype DecodeIO =
  DecodeIO {
      runDecodeIO :: Pull -> SrcAddr -> EndAddr -> IO ()
    }

newtype Encode a =
  Encode {
      runEncode :: a -> EncodeIO -> EncodeIO
    }

newtype Decode x a =
  Decode {
      runDecode :: (x -> DecodeIO) -> (a -> DecodeIO) -> DecodeIO
    }

------------------------------------------------------------------------
-- Encode

instance Contravariant Encode where
  contramap f k =
    Encode $ \x ->
    runEncode k (f x)
  {-# INLINE contramap #-}

instance Divisible Encode where
  divide f ky kz =
    Encode $ \x ->
    case f x of
      (y, z) ->
        runEncode ky y .
        runEncode kz z
  {-# INLINE divide #-}

  conquer =
    Encode $
    const id
  {-# INLINE conquer #-}

instance Decidable Encode where
  lose f =
    Encode $
    absurd . f
  {-# INLINE lose #-}

  choose f ky kz =
    Encode $ \x ->
    case f x of
      Left y ->
        runEncode ky y
      Right z ->
        runEncode kz z
  {-# INLINE choose #-}

finishEncode :: EncodeIO
finishEncode =
  EncodeIO $ \push dst _ ->
    push dst 0# (\_ _ -> return ())
{-# INLINE finishEncode #-}

runEncodeStrict :: Int -> Encode a -> a -> Strict.ByteString
runEncodeStrict scratchSize encode x =
  unsafeLocalState . withBuffer scratchSize $ \buf (Ptr dst0) (Ptr end0) ->
    runEncodeIO (runEncode encode x finishEncode) (putBuffer buf) dst0 end0
{-# INLINE runEncodeStrict #-}

runEncodeStrict' :: Encode a -> a -> Strict.ByteString
runEncodeStrict' =
  runEncodeStrict 4
{-# INLINE runEncodeStrict' #-}

------------------------------------------------------------------------
-- Decode

instance Bifunctor Decode where
  bimap f g k =
    Decode $ \fail succ ->
    runDecode k (fail . f) (succ . g)
  {-# INLINE bimap #-}

  first f k =
    Decode $ \fail succ ->
    runDecode k (fail . f) succ
  {-# INLINE first #-}

  second g k =
    Decode $ \fail succ ->
    runDecode k fail (succ . g)
  {-# INLINE second #-}

instance Functor (Decode x) where
  fmap f k =
    Decode $ \fail succ ->
    runDecode k fail (succ . f)
  {-# INLINE fmap #-}

instance Applicative (Decode x) where
  pure x =
    Decode $ \_ succ ->
    succ x
  {-# INLINE pure #-}

  (<*>) f v =
    Decode $ \fail succ ->
    runDecode f fail $ \g ->
    runDecode v fail (succ . g)
  {-# INLINE (<*>) #-}

instance Monad (Decode x) where
  return x =
    Decode $ \_ succ ->
    succ x
  {-# INLINE return #-}

  (>>=) m k =
    Decode $ \fail succ ->
    runDecode m fail $ \x ->
    runDecode (k x) fail succ
  {-# INLINE (>>=) #-}

instance MonadIO (Decode x) where
  liftIO io =
    Decode $ \_ succ ->
    DecodeIO $ \pull src end -> do
      x <- io
      runDecodeIO (succ x) pull src end
  {-# INLINE liftIO #-}

instance PrimMonad (Decode x) where
  type PrimState (Decode x) =
    PrimState IO

  primitive =
    liftIO . primitive
  {-# INLINE primitive #-}

decodeFail :: x -> Decode x a
decodeFail x =
  Decode $ \fail _ ->
  fail x
{-# INLINE decodeFail #-}

data DecodeError x =
    DecodeError !x
  | DecodeNotEnoughBytes !Int
  | DecodeDidNotComplete
    deriving (Eq, Ord, Show)

renderDecodeError :: (x -> Text) -> DecodeError x -> Text
renderDecodeError render = \case
  DecodeError err ->
    render err
  DecodeNotEnoughBytes _ ->
    "not enough bytes to complete decode"
  DecodeDidNotComplete ->
    "internal decode error, poorly formed decode did not complete"

runDecodeStrict :: Decode x a -> Strict.ByteString -> Either (DecodeError x) a
runDecodeStrict decode !(ByteString.PS fp (I# off) (I# len)) =
  unsafeLocalState . withForeignPtr fp $! \(Ptr bs) -> do
    ref <- newIORef (Left DecodeDidNotComplete)

    let
      src = plusAddr# bs off
      end = plusAddr# src len

      pull cur _ _ =
        writeIORef ref . Left $ DecodeNotEnoughBytes (I# (minusAddr# cur src))

      fail x =
        DecodeIO $ \_ _ _ ->
        writeIORef ref . Left $ DecodeError x

      succ x =
        DecodeIO $ \_ _ _ ->
        writeIORef ref (Right x)

    runDecodeIO (runDecode decode fail succ) pull src end
    readIORef ref
{-# INLINE runDecodeStrict #-}

------------------------------------------------------------------------
-- Util

unsafeEncodeSized :: (a -> Int) -> (Ptr a -> a -> IO ()) -> Encode a
unsafeEncodeSized getLen writePtr =
  Encode $ \x k ->
  EncodeIO $ \push dst0 end0 ->
    let
      !(I# srcLen) = getLen x
      dstLen = minusAddr# end0 dst0
    in
      if isTrue# (dstLen >=# srcLen) then do
        writePtr (Ptr dst0) x
        runEncodeIO k push (plusAddr# dst0 srcLen) end0
      else
        push dst0 srcLen $ \dst1 end1 -> do
          writePtr (Ptr dst1) x
          runEncodeIO k push (plusAddr# dst1 srcLen) end1
{-# INLINE unsafeEncodeSized #-}

unsafeEncodeFixed :: Int -> (Ptr a -> a -> IO ()) -> Encode a
unsafeEncodeFixed srcLen writePtr =
  unsafeEncodeSized (const srcLen) writePtr
{-# INLINE unsafeEncodeFixed #-}

unsafeDecodeFixed :: Int -> (Ptr a -> IO a) -> Decode x a
unsafeDecodeFixed (I# dstLen) readPtr =
  Decode $ \_ k ->
  DecodeIO $ \pull src0 end0 ->
    let
      srcLen = minusAddr# end0 src0
    in
      if isTrue# (srcLen >=# dstLen) then do
        !x <- readPtr (Ptr src0)
        runDecodeIO (k x) pull (plusAddr# src0 dstLen) end0
      else
        pull src0 dstLen $ \src1 end1 -> do
          !x <- readPtr (Ptr src1)
          runDecodeIO (k x) pull (plusAddr# src1 dstLen) end1
{-# INLINE unsafeDecodeFixed #-}


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
