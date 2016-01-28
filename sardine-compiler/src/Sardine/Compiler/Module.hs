{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Module (
    moduleOfProgram
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.SrcLoc (noLoc)

import           Language.Thrift.Types (Program(..))
import           Language.Thrift.Types (Header(..), Namespace(..))

import           P

import           Sardine.Compiler.Data
import           Sardine.Compiler.Error
import           Sardine.Compiler.Util

import           System.FilePath (FilePath, takeBaseName)


moduleNameOfHeader :: Header a -> Maybe ModuleName
moduleNameOfHeader = \case
  HeaderInclude _ ->
    Nothing
  HeaderNamespace (Namespace lang name _)
    | lang == "*" ->
      Just . ModuleName . T.unpack $ moduleOfText name
    | lang == "hs" ->
      Just . ModuleName . T.unpack $ moduleOfText name
    | otherwise ->
      Nothing

import' :: ModuleName -> [ImportSpec] -> ImportDecl
import' modName specs =
  ImportDecl {
      importLoc = noLoc
    , importModule = modName
    , importQualified = False
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Just (False, specs)
    }

ivar :: Text -> ImportSpec
ivar =
  IVar . Ident . T.unpack

moduleOfProgram :: FilePath -> Program a -> Either (CompilerError a) Module
moduleOfProgram path = \case
  Program hdrs defs -> do
    let
      defaultName =
        ModuleName . T.unpack . pascalOfText . T.pack $ takeBaseName path

      moduleNames =
        fmap moduleNameOfHeader hdrs

      moduleName =
        fromMaybe defaultName $ foldl (<|>) Nothing moduleNames

      pragmas =
        [ LanguagePragma noLoc [Ident "NoImplicitPrelude"]
        , OptionsPragma noLoc (Just GHC) "-fno-warn-unused-imports"
        , OptionsPragma noLoc (Just GHC) "-funbox-strict-fields"
        ]

      imports =
        [ import' (ModuleName "Data.Bool") [ivar "Bool"]
        , import' (ModuleName "Data.ByteString") [ivar "ByteString"]
        , import' (ModuleName "Data.Eq") [ivar "Eq"]
        , import' (ModuleName "Data.Int") [ivar "Int16", ivar "Int32", ivar "Int64"]
        , import' (ModuleName "Data.Map") [ivar "Map"]
        , import' (ModuleName "Data.Maybe") [ivar "Maybe"]
        , import' (ModuleName "Data.Ord") [ivar "Ord"]
        , import' (ModuleName "Data.Set") [ivar "Set"]
        , import' (ModuleName "Data.Text") [ivar "Text"]
        , import' (ModuleName "Data.Vector") [ivar "Vector"]
        , import' (ModuleName "Data.Word") [ivar "Word8"]
        , import' (ModuleName "Prelude") [ivar "Double"]
        , import' (ModuleName "Text.Read") [ivar "Read"]
        , import' (ModuleName "Text.Show") [ivar "Show"]
        ]

    datas <- traverse dataOfDefinition defs

    pure $
      Module noLoc moduleName pragmas Nothing Nothing imports datas
