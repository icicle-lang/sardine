{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Compiler.Monad (
    Compiler
  , runCompiler

  , CompilerT
  , runCompilerT

  , nextPE
  , nextNPE

  , hoistError
  ) where

import           Control.Monad.Trans.Class (MonadTrans(..))

import           Data.Functor.Identity (Identity(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax (Name, Pat, Exp)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Fresh
import           Sardine.Haskell.Combinators

import           P hiding (exp)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, left)


newtype CompilerT x m a =
  CompilerT {
      unCompilerT :: EitherT (CompilerError x) (FreshT m) a
    } deriving (Functor, Applicative, Monad)

instance MonadTrans (CompilerT x) where
  lift = CompilerT . lift . lift

type Compiler x a =
  CompilerT x Identity a

runCompilerT :: Monad m => CompilerT x m a -> m (Either (CompilerError x) a)
runCompilerT =
  runFreshT . runEitherT . unCompilerT

runCompiler :: Compiler x a -> Either (CompilerError x) a
runCompiler =
  runIdentity . runCompilerT

nextPE :: Monad m => Text -> CompilerT x m (Pat, Exp)
nextPE prefix = do
  (_, pat, exp) <- nextNPE prefix
  return (pat, exp)

nextNPE :: Monad m => Text -> CompilerT x m (Name, Pat, Exp)
nextNPE prefix = do
  n <- CompilerT . lift $ nextInt prefix
  let
    suffix =
      T.pack (show n)
    name =
      prefix <> suffix
  return (varN name, bangP $ varP name, varE name)

hoistError :: Monad m => CompilerError x -> CompilerT x m a
hoistError =
  CompilerT . left
