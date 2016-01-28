{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Haskell.Pretty (
    ppModule
  , PrettyError(..)
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax

import           P hiding (exp)

import           Sardine.Pretty


data PrettyError =
    UnsupportedSyntax !Text
    deriving (Eq, Ord, Show)

unsupported :: Show a => a -> Either PrettyError b
unsupported =
  Left . UnsupportedSyntax . T.pack . show

ppName :: Name -> Doc
ppName = \case
  Ident name ->
    string name
  Symbol name ->
    parens (string name)

ppNameInfix :: Name -> Doc
ppNameInfix = \case
  Ident name ->
    "`" <> string name <> "`"
  Symbol name ->
    string name

ppModuleName :: ModuleName -> Doc
ppModuleName = \case
  ModuleName name ->
    string name

ppSpecialCon :: SpecialCon -> Doc
ppSpecialCon = \case
  UnitCon ->
    "()"
  ListCon ->
    "[]"
  FunCon ->
    "->"
  TupleCon Boxed n ->
    "(" <> hcat (List.replicate (n-1) ",") <> ")"
  TupleCon Unboxed n ->
    "(#" <> hcat (List.replicate (n-1) ",") <> "#)"
  Cons ->
    ":"
  UnboxedSingleCon ->
    "(# #)"

ppQName :: QName -> Doc
ppQName = \case
  Qual modn name ->
    ppModuleName modn <> "." <> ppName name
  UnQual name ->
    ppName name
  Special sc ->
    ppSpecialCon sc

ppBangType :: BangType -> Doc
ppBangType = \case
  BangedTy ->
    "!"
  UnpackedTy ->
    "{-# UNPACK #-}"

ppType :: Type -> Either PrettyError Doc
ppType = \case
  TyCon qname ->
    pure $ ppQName qname
  TyApp f x -> do
    pf <- ppType f
    px <- ppType x
    pure $ pf <+> savageParens px
  TyBang bang ty -> do
    pty <- ppType ty
    pure $ ppBangType bang <> savageParens pty
  ty ->
    unsupported ty

ppDataOrNew :: DataOrNew -> Doc
ppDataOrNew = \case
  DataType ->
    "data"
  NewType ->
    "newtype"

ppField :: ([Name], Type) -> Either PrettyError Doc
ppField (names, ty) = do
  pty <- ppType ty
  pure $
    hcat (punctuate ", " $ fmap ppName names) <+> "::" <+> pty

ppFieldList :: [([Name], Type)] -> Either PrettyError Doc
ppFieldList = \case
  [] ->
    pure $ "{" <&> "  }"
  (f:fs) -> do
    pf <- ppField f
    pfs <- traverse ppField fs
    pure $
      "{" <&> "  " <> align (foldl (\x y -> x <&> ", " <> y) ("  " <> pf) pfs <&> "}")

ppConDecl :: ConDecl -> Either PrettyError Doc
ppConDecl = \case
  ConDecl name tys -> do
    ptys <- traverse ppType tys
    pure $ ppName name <+> hsep ptys
  InfixConDecl x name y -> do
    px <- ppType x
    py <- ppType y
    pure $ px <+> ppNameInfix name <+> py
  RecDecl name fields -> do
    pfields <- ppFieldList fields
    pure $
      ppName name <+> pfields

isRecDecl :: QualConDecl -> Bool
isRecDecl = \case
  QualConDecl _ _ _ (RecDecl _ _) ->
    True
  QualConDecl _ _ _ _ ->
    False

ppQualConDecl :: QualConDecl -> Either PrettyError Doc
ppQualConDecl = \case
  QualConDecl _ [] [] con ->
    ppConDecl con
  qcon@(QualConDecl _ _ _ _) ->
    unsupported qcon

ppQualConDecls :: [QualConDecl] -> Either PrettyError Doc
ppQualConDecls = \case
  [] ->
    pure mempty
  q : [] | isRecDecl q -> do
    pq <- ppQualConDecl q
    pure $
      "=" <&> "  " <> align pq
  q : qs -> do
    pq <- ppQualConDecl q
    pqs <- traverse ppQualConDecl qs
    pure $
      "=" <&> "  " <> align (foldl (\x y -> x <&> "| " <> align y)  ("  " <> align pq) pqs)

ppDeriving :: (QName, [Type]) -> Either PrettyError Doc
ppDeriving = \case
  (qname, []) ->
    pure $ ppQName qname
  derv@(_, _:_) ->
    unsupported derv

ppDerivings :: [Deriving] -> Either PrettyError (Maybe Doc)
ppDerivings = \case
  [] ->
    pure Nothing
  ds -> do
    pds <- traverse ppDeriving ds
    pure . Just $
      "deriving (" <> hcat (punctuate ", " pds) <> ")"

ppDecl :: Decl -> Either PrettyError Doc
ppDecl = \case
  DataDecl _ dataOrNew [] name [] qcons dervs -> do
    pqs <- ppQualConDecls qcons
    mpds <- ppDerivings dervs
    pure $
      case (qcons, mpds) of
        (_, Nothing) ->
          ppDataOrNew dataOrNew <+> ppName name <+> pqs
        (qcon:[], Just pds) | isRecDecl qcon ->
          ppDataOrNew dataOrNew <+> ppName name <+> pqs <+> pds
        (_, Just pds) ->
          ppDataOrNew dataOrNew <+> ppName name <+> pqs <&>
          "    " <> pds
  decl ->
    unsupported decl

ppDecls :: [Decl] -> Either PrettyError Doc
ppDecls = \case
  [] ->
    pure mempty
  (d:ds) -> do
    pd <- ppDecl d
    pds <- traverse ppDecl ds
    pure $
      foldl (\x y -> x <> line <> line <> y) pd pds

ppExp :: Exp -> Either PrettyError Doc
ppExp = \case
  exp ->
    unsupported exp

ppAnnotation :: Annotation -> Either PrettyError Doc
ppAnnotation = \case
  Ann name exp -> do
    pexp <- ppExp exp
    pure $
      ppName name <+> pexp
  TypeAnn name exp -> do
    pexp <- ppExp exp
    pure $
      "type" <+> ppName name <+> pexp
  ModuleAnn exp -> do
    pexp <- ppExp exp
    pure $
      "module" <+> pexp

ppTool :: Tool -> Doc
ppTool = \case
  GHC ->
    "GHC"
  HUGS ->
    "HUGS"
  NHC98 ->
    "NHC98"
  YHC ->
    "YHC"
  HADDOCK ->
    "HADDOCK"
  UnknownTool tool ->
    string tool

ppPragma :: ModulePragma -> Either PrettyError Doc
ppPragma = \case
  LanguagePragma _ names ->
    pure $
      "{-# LANGUAGE" <+> hcat (punctuate ", " (fmap ppName names)) <+> "#-}"
  OptionsPragma _ Nothing opts ->
    pure $
      "{-# OPTIONS" <+> string opts <+> "#-}"
  OptionsPragma _ (Just tool) opts ->
    pure $
      "{-# OPTIONS_" <> ppTool tool <+> string opts <+> "#-}"
  AnnModulePragma _ ann -> do
    pann <- ppAnnotation ann
    pure $
      "{-# ANN" <+> pann <+> "#-}"

importRootModule :: ImportDecl -> ModuleName
importRootModule i =
  case importModule i of
    ModuleName name ->
      ModuleName (List.takeWhile (/= '.') name)

ppNamespace :: Namespace -> Doc
ppNamespace = \case
  NoNamespace ->
    mempty
  TypeNamespace ->
    "type"
  PatternNamespace ->
    "pattern"

ppCName :: CName -> Doc
ppCName = \case
  VarName name ->
    ppName name
  ConName name ->
    ppName name

ppImportSpec :: ImportSpec -> Doc
ppImportSpec = \case
  IVar name ->
    ppName name
  IAbs ns name ->
    ppNamespace ns <+> ppName name
  IThingAll name ->
    ppName name <> "(..)"
  IThingWith name names ->
    ppName name <> "(" <> hcsep (fmap ppCName names) <> ")"

ppImportSpecs :: (Bool, [ImportSpec]) -> Doc
ppImportSpecs (hide, specs) =
  if hide then
    "hiding (" <> hcsep (fmap ppImportSpec specs) <> ")"
  else
    "(" <> hcsep (fmap ppImportSpec specs) <> ")"

ppImportDecl :: ImportDecl -> Doc
ppImportDecl = \case
  ImportDecl _ name qual src safe mpkg mas mspecs ->
    let
      msrc = valueOrEmpty src "{-# SOURCE #-}"
      msafe = valueOrEmpty safe "safe"
      mqual = valueOrEmpty qual "qualified" <|> Just "         "
      mpkg' = fmap (string . show) mpkg
      mas' = fmap (("as" <+>) . ppModuleName) mas
      mspecs' = fmap ppImportSpecs mspecs
    in
      hsep $ catMaybes
        [ Just "import"
        , msrc
        , msafe
        , mqual
        , mpkg'
        , Just (ppModuleName name)
        , mas'
        , mspecs' ]

ppImports :: [ImportDecl] -> Doc
ppImports =
  vsep .
  punctuate line .
  fmap (vsep . fmap ppImportDecl) .
  List.groupBy ((==) `on` importRootModule) .
  List.sortBy (compare `on` importModule)

ppModule :: Module -> Either PrettyError Doc
ppModule = \case
  Module _ name pragmas Nothing Nothing imports decls -> do
    ppragmas <- traverse ppPragma pragmas
    pdecls <- ppDecls decls
    pure $ vsep
      [ vsep ppragmas
      , "module" <+> ppModuleName name <+> "where"
      , mempty
      , ppImports imports
      , mempty
      , mempty
      , pdecls
      ]

  Module _ _ _ _ (Just exports) _ _ ->
    unsupported exports

  Module _ _ _ (Just warningText) _ _ _ ->
    unsupported warningText
