{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Fresh (
    Fresh
  , runFresh

  , FreshT
  , runFreshT

  , nextInt
  ) where

import           Control.Lens ((%%=), at)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

import           Data.Functor.Identity (Identity(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           P


newtype FreshT m a =
  FreshT {
      unFreshT :: StateT (Map Text Int) m a
    } deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

type Fresh a =
  FreshT Identity a

runFreshT :: Monad m => FreshT m a -> m a
runFreshT x =
  evalStateT (unFreshT x) Map.empty

runFresh :: Fresh a -> a
runFresh =
  runIdentity . runFreshT

increment :: Maybe Int -> (Int, Maybe Int)
increment = \case
  Nothing ->
    (0, Just 1)
  Just x ->
    (x, Just $! x + 1)

nextInt :: Monad m => Text -> FreshT m Int
nextInt key = do
  FreshT $! at key %%= increment
