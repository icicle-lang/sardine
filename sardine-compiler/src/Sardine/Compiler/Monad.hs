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
  , lazyPE
  , nextNPE
  , lazyNPE

  , typeLookup
  , tryTypeLookup

  , hoistCE
  , hoistEitherCE
  ) where

import           Control.Monad.Trans.Reader (ReaderT(..), ask)
import           Control.Monad.Trans.Class (MonadTrans(..))

import           Data.Functor.Identity (Identity(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax (Name, Pat, Exp)

import           Sardine.Compiler.Analysis
import           Sardine.Compiler.Environment
import           Sardine.Compiler.Error
import           Sardine.Compiler.Fresh
import           Sardine.Haskell.Combinators

import           P hiding (exp)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, left, hoistEither)


newtype CompilerT x m a =
  CompilerT {
      unCompilerT :: EitherT (CompilerError x) (ReaderT (Env (ThriftType x)) (FreshT m)) a
    } deriving (Functor, Applicative, Monad)

instance MonadTrans (CompilerT x) where
  lift = CompilerT . lift . lift . lift

type Compiler x a =
  CompilerT x Identity a

runCompilerT :: Monad m => Env (ThriftType x) -> CompilerT x m a -> m (Either (CompilerError x) a)
runCompilerT env =
  runFreshT . flip runReaderT env . runEitherT . unCompilerT

runCompiler :: Env (ThriftType x) -> Compiler x a -> Either (CompilerError x) a
runCompiler env =
  runIdentity . runCompilerT env

nextPE :: Monad m => Text -> CompilerT x m (Pat, Exp)
nextPE prefix = do
  (_, pat, exp) <- nextNPE prefix
  return (pat, exp)

lazyPE :: Monad m => Text -> CompilerT x m (Pat, Exp)
lazyPE prefix = do
  (_, pat, exp) <- lazyNPE prefix
  return (pat, exp)

nextNPE :: Monad m => Text -> CompilerT x m (Name, Pat, Exp)
nextNPE prefix = do
  (n, p, e) <- lazyNPE prefix
  return (n, bangP p, e)

lazyNPE :: Monad m => Text -> CompilerT x m (Name, Pat, Exp)
lazyNPE prefix = do
  n <- CompilerT . lift . lift $ nextInt prefix
  let
    suffix =
      T.pack (show n)
    name =
      prefix <> suffix
  return (varN name, varP name, varE name)

tryTypeLookup :: Monad m => Text -> CompilerT x m (Maybe (ThriftType x))
tryTypeLookup text = do
  env <- CompilerT . lift $ ask
  return $ envLookup text env

typeLookup :: Monad m => Text -> x -> CompilerT x m (ThriftType x)
typeLookup text annot = do
  mtyp <- tryTypeLookup text
  case mtyp of
    Nothing ->
      hoistCE (TypeNotFound text annot)
    Just typ ->
      return typ

hoistCE :: Monad m => CompilerError x -> CompilerT x m a
hoistCE =
  CompilerT . left

hoistEitherCE :: Monad m => Either (CompilerError x) a -> CompilerT x m a
hoistEitherCE =
  CompilerT . hoistEither
