{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Sardine.Compiler.Module (
    moduleOfProgram
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.QQ (decs)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax

import           Language.Thrift.Types (Program(..))
import           Language.Thrift.Types (Header(..), Namespace(..))

import           P

import           Sardine.Compiler.Data
import           Sardine.Compiler.Decode
import           Sardine.Compiler.Default
import           Sardine.Compiler.Monad
import           Sardine.Haskell.Combinators

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

exportMatch :: Match -> ExportSpec
exportMatch = \case
  Match _ name _ _ _ _ ->
    EVar (UnQual name)

exportDecl :: Decl -> [ExportSpec]
exportDecl = \case
  DataDecl _ _ _ tyName _ _ _ ->
    [EThingAll $ UnQual tyName]
  FunBind matches ->
    List.nub (fmap exportMatch matches)
  _ ->
    []

exportAll :: [Decl] -> [ExportSpec]
exportAll =
  concatMap exportDecl

importSpecs' :: ModuleName -> Maybe [ImportSpec] -> Maybe ModuleName -> ImportDecl
importSpecs' modName mspecs mas =
  ImportDecl {
      importLoc = noLoc
    , importModule = modName
    , importQualified = isJust mas
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = mas
    , importSpecs = fmap (False,) mspecs
    }

importSome :: ModuleName -> [ImportSpec] -> ImportDecl
importSome modName specs =
  importSpecs' modName (Just specs) Nothing

importAll :: ModuleName -> ImportDecl
importAll modName =
  importSpecs' modName Nothing Nothing

importQual :: ModuleName -> ModuleName -> ImportDecl
importQual modName mas =
  importSpecs' modName Nothing (Just mas)

varI :: Text -> ImportSpec
varI =
  IVar . Ident . T.unpack

allI :: Text -> ImportSpec
allI =
  IThingAll . Ident . T.unpack

varS :: Text -> ImportSpec
varS =
  IVar . Symbol . T.unpack

modN :: Text -> ModuleName
modN =
  ModuleName . T.unpack

moduleOfProgram :: FilePath -> Program a -> Compiler a Module
moduleOfProgram path = \case
  Program hdrs defs -> do
    let
      defaultName =
        modN . pascalOfText . T.pack $ takeBaseName path

      moduleNames =
        fmap moduleNameOfHeader hdrs

      moduleName =
        fromMaybe defaultName $ foldl (<|>) Nothing moduleNames

      pragmas =
        [ LanguagePragma noLoc [Ident "BangPatterns"]
        , LanguagePragma noLoc [Ident "DoAndIfThenElse"]
        , LanguagePragma noLoc [Ident "LambdaCase"]
        , LanguagePragma noLoc [Ident "NoImplicitPrelude"]
        , LanguagePragma noLoc [Ident "OverloadedStrings"]
        , OptionsPragma noLoc (Just GHC) "-fno-warn-unused-binds"
        , OptionsPragma noLoc (Just GHC) "-fno-warn-unused-imports"
        , OptionsPragma noLoc (Just GHC) "-funbox-strict-fields"
        ]

      imports =
        [ importSome (modN "Control.Exception") [varI "throw"]
        , importSome (modN "Control.Monad") [varI "return", varI "unless", varI "replicateM"]
        , importSome (modN "Data.Bits") [allI "Bits"]
        , importSome (modN "Data.Bool") [allI "Bool", varS "&&"]
        , importSome (modN "Data.ByteString") [varI "ByteString"]
        , importQual (modN "Data.ByteString") (modN "B")
        , importSome (modN "Data.Eq") [allI "Eq"]
        , importSome (modN "Data.Function") [varS "."]
        , importSome (modN "Data.Functor") [varS "<$>", varI "fmap"]
        , importSome (modN "Data.Int") [varI "Int16", varI "Int32", varI "Int64"]
        , importSome (modN "Data.Map") [varI "Map"]
        , importQual (modN "Data.Map") (modN "Map")
        , importSome (modN "Data.Maybe") [allI "Maybe"]
        , importSome (modN "Data.Ord") [varI "Ord"]
        , importSome (modN "Data.Set") [varI "Set"]
        , importQual (modN "Data.Set") (modN "Set")
        , importSome (modN "Data.Text") [varI "Text"]
        , importQual (modN "Data.Text") (modN "T")
        , importQual (modN "Data.Text.Encoding") (modN "T")
        , importQual (modN "Data.Vector") (modN "Boxed")
        , importQual (modN "Data.Vector.Unboxed") (modN "Unboxed")
        , importQual (modN "Data.Vector.Hybrid") (modN "Hybrid")
        , importSome (modN "Data.Word") [varI "Word8", varI "Word64"]
        , importSome (modN "GHC.Exts") [allI "SpecConstrAnnotation"]
        , importSome (modN "Prelude") [allI "Num", varI "Double", varS "$!", varI "fromIntegral"]
        , importAll (modN "Sardine.Runtime")
        , importSome (modN "Text.Read") [varI "Read"]
        , importSome (modN "Text.Show") [varI "Show"]
        ]

    datas <- traverse dataOfDefinition defs
    defaults <- concat <$> traverse defaultOfDefinition defs
    decodes <- concat <$> traverse decodeOfDefinition defs

    let
      exports =
        Just . exportAll $ datas <> decodes

    pure . Module noLoc moduleName pragmas Nothing exports imports $
      datas <>
      defaults <>
      decodes <>
      [decs|
        data SpecConstr = SpecConstr | SpecConstr2
        {-# ANN type SpecConstr ForceSpecConstr #-}
      |]
