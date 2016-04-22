{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Environment (
    Env(..)
  , envEmpty
  , envSingleton
  , envUnion
  , envUnions
  , envLookup
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           P


newtype Env v =
  Env {
      unEnv :: Map Text v
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

envEmpty :: Env v
envEmpty =
  Env Map.empty

envSingleton :: Text -> v -> Env v
envSingleton name info =
  Env (Map.singleton name info)

envUnion :: Env v -> Env v -> Env v
envUnion (Env xs) (Env ys) =
  Env (Map.union xs ys)

envUnions :: [Env v] -> Env v
envUnions =
  Env . Map.unions . fmap unEnv

envLookup :: Text -> Env v -> Maybe v
envLookup name =
  Map.lookup name . unEnv
