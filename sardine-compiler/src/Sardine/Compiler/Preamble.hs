{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Preamble (
    preambleOfProgram
  ) where

import qualified Data.Text as T

import           Language.Thrift.Types (Program(..))
import           Language.Thrift.Types (Header(..), Namespace(..))

import           P hiding (Enum)

import           Sardine.Compiler.Error
import           Sardine.Compiler.Ident
import           Sardine.Compiler.Pretty

import           System.FilePath (FilePath, takeBaseName)


moduleNameOfHeader :: Header a -> Either (CompilerError a) (Maybe Doc)
moduleNameOfHeader = \case
  HeaderInclude _ ->
    return Nothing
  HeaderNamespace (Namespace lang name annot)
    | lang == "*" ->
      Just <$> conidOfText (Just annot) name
    | lang == "hs" ->
      Just <$> conidOfText (Just annot) name
    | otherwise ->
      return Nothing

preambleOfProgram :: FilePath -> Program a -> Either (CompilerError a) Doc
preambleOfProgram path = \case
  Program hdrs _ -> do
    defaultName <- conidOfText Nothing . T.pack $ takeBaseName path
    moduleNames <- traverse moduleNameOfHeader hdrs
    let
      moduleName =
        fromMaybe defaultName $ foldl (<|>) Nothing moduleNames
    return $ vsep
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
        , "{-# OPTIONS_GHC -funbox-strict-fields #-}"
        , "module" <+> moduleName <+> "where"
        , ""
        , "import           Data.Bool (Bool)"
        , "import           Data.ByteString (ByteString)"
        , "import           Data.Eq (Eq)"
        , "import           Data.Int (Int16, Int32, Int64)"
        , "import           Data.Map (Map)"
        , "import           Data.Maybe (Maybe)"
        , "import           Data.Ord (Ord)"
        , "import           Data.Set (Set)"
        , "import           Data.Text (Text)"
        , "import           Data.Vector (Vector)"
        , "import           Data.Word (Word8)"
        , ""
        , "import           Prelude (Double)"
        , ""
        , "import           Text.Read (Read)"
        , "import           Text.Show (Show)"
        ]
