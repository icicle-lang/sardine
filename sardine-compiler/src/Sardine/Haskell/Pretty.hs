{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Sardine.Haskell.Pretty (
    ppModule

  , PrettyError(..)
  , renderPrettyError
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Haskell.Exts.Syntax

import           P hiding (Alt, exp, pi)

import           Sardine.Pretty


data PrettyError =
    WhereClauseNotSupported !Binds
  | SyntaxNotSupported !Text !Text
    deriving (Eq, Ord, Show)

renderPrettyError :: PrettyError -> Text
renderPrettyError = \case
  WhereClauseNotSupported binds ->
    "Where clauses are not supported:\n  " <> T.pack (show binds)
  SyntaxNotSupported typ syntax ->
    typ <> " syntax not supported:\n  " <> syntax

unsupported :: Show a => Text -> a -> Either PrettyError b
unsupported typ =
  Left . SyntaxNotSupported typ . T.pack . show

isSymbol :: Name -> Bool
isSymbol = \case
  Ident _ ->
    False
  Symbol _ ->
    True

isQSymbol :: QName -> Bool
isQSymbol = \case
  Qual _ name ->
    isSymbol name
  UnQual name ->
    isSymbol name
  Special _ ->
    False

ppNameRaw :: Name -> Doc
ppNameRaw = \case
  Ident name ->
    string name
  Symbol name ->
    string name

ppQNameRaw :: QName -> Doc
ppQNameRaw = \case
  Qual modn name ->
    ppModuleName modn <> "." <> ppNameRaw name
  UnQual name ->
    ppNameRaw name
  Special sc ->
    ppSpecialCon sc

ppName :: Name -> Doc
ppName name =
  if isSymbol name then
    parens (ppNameRaw name)
  else
    ppNameRaw name

ppQName :: QName -> Doc
ppQName qname =
  if isQSymbol qname then
    parens (ppQNameRaw qname)
  else
    ppQNameRaw qname

ppNameInfix :: Name -> Doc
ppNameInfix name =
  if isSymbol name then
    ppNameRaw name
  else
    "`" <> ppNameRaw name <> "`"

ppQNameInfix :: QName -> Doc
ppQNameInfix qname =
  if isQSymbol qname then
    ppQNameRaw qname
  else
    "`" <> ppQNameRaw qname <> "`"

ppQOp :: QOp -> Doc
ppQOp = \case
  QVarOp qname ->
    ppQNameInfix qname
  QConOp qname ->
    ppQNameInfix qname

wrapQOp :: QOp -> Exp -> Bool
wrapQOp qop exp =
  case (qop, exp) of
    (QVarOp (UnQual (Symbol "<+>")), _) ->
      True
    (QVarOp (UnQual (Symbol "<>")), _) ->
      True
    (QVarOp (UnQual (Symbol ".")), _) ->
      True
    (QVarOp (UnQual (Symbol "$")), Lambda{}) ->
      False
    (QVarOp (UnQual (Symbol "$")), _) ->
      True
    _ ->
      False

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

ppBangType :: BangType -> Doc
ppBangType = \case
  BangedTy ->
    "!"
  UnpackedTy ->
    "{-# UNPACK #-}"

ppPromoted :: Promoted -> Either PrettyError Doc
ppPromoted = \case
  PromotedInteger x ->
    pure $ integer x
  promoted ->
    unsupported "Promoted" promoted

ppAssertion :: Asst -> Either PrettyError Doc
ppAssertion = \case
  ClassA qname tys -> do
    ptys <- traverse ppType tys
    pure $ hsep (ppQName qname : ptys)
  asst ->
    unsupported "Assertion" asst

ppContext :: [Asst] -> Either PrettyError Doc
ppContext = \case
  [] ->
    pure mempty
  [x] -> do
    px <- ppAssertion x
    pure $ px <+> "=> "
  xs -> do
    pxs <- traverse ppAssertion xs
    pure $ parens (hcsep pxs) <+> "=> "

ppType :: Type -> Either PrettyError Doc
ppType = \case
  TyCon qname ->
    pure $ ppQName qname
  TyApp f x -> do
    pf <- ppType f
    px <- ppType x
    pure $ pf <+> savageParens px
  TyFun f x -> do
    pf <- ppType f
    px <- ppType x
    pure $ pf <+> "->" <+> px
  TyVar name ->
    pure $ ppName name
  TyTuple Boxed tys -> do
    ptys <- traverse ppType tys
    pure $ parens (hcsep ptys)
  TyBang bang ty@TyTuple{} -> do
    pty <- ppType ty
    pure $ ppBangType bang <> pty
  TyBang bang ty -> do
    pty <- ppType ty
    pure $ ppBangType bang <> savageParens pty
  TyPromoted promoted -> do
    ppPromoted promoted
  TyForall Nothing ctx ty -> do
    pctx <- ppContext ctx
    pty <- ppType ty
    pure $ pctx <> pty
  ty ->
    unsupported "Type" ty

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
    pure $ hsep (ppName name : ptys)
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
    unsupported "QualConDecl" qcon

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
    unsupported "Deriving" derv

ppDerivings :: [Deriving] -> Either PrettyError (Maybe Doc)
ppDerivings = \case
  [] ->
    pure Nothing
  ds -> do
    pds <- traverse ppDeriving ds
    pure . Just $
      "deriving (" <> hcat (punctuate ", " pds) <> ")"

ppBinds :: Binds -> Either PrettyError Doc
ppBinds = \case
  BDecls decls ->
    ppDecls decls
  binds@(IPBinds _) ->
    unsupported "Binds" binds

hangExp :: Exp -> Either PrettyError Doc
hangExp exp = do
  pexp <- ppExp exp
  pure $ " " <> pexp

wrapExp :: Exp -> Either PrettyError Doc
wrapExp exp = do
  pexp <- ppExp exp
  pure $ line <> "  " <> align pexp

ppGeneratorExp :: Exp ->  Either PrettyError Doc
ppGeneratorExp exp =
  case exp of
    If _ _ _ ->
      wrapExp exp
    Case _ _ ->
      wrapExp exp
    Let _ _ ->
      wrapExp exp
    _ ->
      hangExp exp

ppStmt :: Stmt -> Either PrettyError Doc
ppStmt = \case
  Generator _ pat exp -> do
    ppat <- ppPat pat
    pexp <- ppGeneratorExp exp
    pure $ ppat <+> "<-" <> pexp
  Qualifier exp -> do
    ppExp exp
  LetStmt binds -> do
    pbinds <- ppBinds binds
    pure $
      "let" <&>
      "  " <> align pbinds
  stmt ->
    unsupported "Stmt" stmt

ppStmts :: [Stmt] -> Either PrettyError Doc
ppStmts stmts =
  vsep <$> traverse ppStmt stmts

ppAlt :: Alt -> Either PrettyError Doc
ppAlt = \case
  Alt _ pat rhs Nothing -> do
    ppat <- ppPat pat
    prhs <- ppRhs "->" rhs
    pure $
      ppat <> prhs
  Alt _ _ _ (Just binds) ->
    Left (WhereClauseNotSupported binds)

ppLiteral :: Literal -> Either PrettyError Doc
ppLiteral = \case
  String str ->
    pure $ string (show str)
  Int i ->
    pure $ integer i
  lit ->
    unsupported "Literal" lit

ppExp :: Exp -> Either PrettyError Doc
ppExp = \case
  Lit lit ->
    ppLiteral lit
  Var qname ->
    pure $ ppQName qname
  Con qname ->
    pure $ ppQName qname
  App f x@Paren{} -> do
    pf <- ppExp f
    px <- ppExp x
    pure $
      pf <+> px
  App f x -> do
    pf <- ppExp f
    px <- ppExp x
    pure $
      pf <+> savageParens px
  InfixApp x qop y -> do
    px <- ppExp x
    py <- ppExp y
    if wrapQOp qop y then
      pure $
        align (px <+> ppQOp qop <&&> py)
    else
      pure $
        px <+> ppQOp qop <+> py
  Let (BDecls []) exp ->
    ppExp exp
  Let binds exp -> do
    pbinds <- ppBinds binds
    pexp <- ppExp exp
    pure $
      "let" <&>
      "  " <> align pbinds <&>
      "in" <&>
      "  " <> align pexp
  Case exp alts -> do
    pexp <- ppExp exp
    palts <- traverse ppAlt alts
    pure $
      "case" <+> pexp <+> "of" <&>
      "  " <> align (vsep palts)
  Lambda _ pats exp -> do
    ppats <- traverse ppPat pats
    prhs <- ppRhsExp exp
    pure $
      "\\" <+> hsep ppats <+> "->" <> prhs
  If i t e -> do
    pi <- ppExp i
    pt <- ppRhsExp t
    pe <- ppRhsExp e
    pure $
      "if" <+> align pi <+> "then" <> pt <&>
      "else" <> pe
  LCase alts -> do
    palts <- traverse ppAlt alts
    pure $
      "\\case" <&>
      "  " <> align (vsep palts)
  Do stmts -> do
    pstmts <- ppStmts stmts
    pure $
      "do" <&> "  " <> align pstmts
  Paren exp -> do
    pexp <- ppExp exp
    pure $
      parens pexp
  Tuple Boxed exps -> do
    pexps <- traverse ppExp exps
    pure $
      parens $ hcsep pexps
  List exps -> do
    pexps <- traverse ppExp exps
    pure $
      brackets $ hcsep pexps
  ExpTypeSig _ exp ty -> do
    pexp <- ppExp exp
    pty <- ppType ty
    pure $
      pexp <+> "::" <+> pty
  exp ->
    unsupported "Exp" exp

ppRhsExp :: Exp ->  Either PrettyError Doc
ppRhsExp exp =
  case exp of
    Do _ ->
      hangExp exp
    LCase _ ->
      hangExp exp
    _ ->
      wrapExp exp

ppRhs :: Doc -> Rhs -> Either PrettyError Doc
ppRhs arrow = \case
  UnGuardedRhs exp -> do
    pexp <- ppRhsExp exp
    pure $ " " <> arrow <> pexp
  GuardedRhss gs ->
    unsupported "Rhs" gs

ppPat :: Pat -> Either PrettyError Doc
ppPat = \case
  PVar name ->
    pure $ ppName name
  PLit Signless lit ->
    ppLiteral lit
  PLit Negative lit -> do
    plit <- ppLiteral lit
    pure $ "(-" <> plit <> ")"
  PApp qname args -> do
    pargs <- traverse ppPat args
    pure $
      hsep (ppQName qname : pargs)
  PBangPat pat@PTuple{} -> do
    ppat <- ppPat pat
    pure $ "!" <> ppat
  PBangPat pat -> do
    ppat <- ppPat pat
    pure $ "!" <> savageParens ppat
  PWildCard ->
    pure "_"
  PTuple Boxed pats -> do
    ppats <- traverse ppPat pats
    pure $
      parens (hcsep ppats)
  pat ->
    unsupported "Pat" pat

ppSig :: Maybe Type -> Either PrettyError Doc
ppSig = \case
  Nothing ->
    pure mempty
  Just ty -> do
    pty <- ppType ty
    pure $ " ::" <> pty

ppMatch :: Match -> Either PrettyError Doc
ppMatch = \case
  Match _ name pats msig rhs Nothing -> do
    ppats <- traverse ppPat pats
    psig <- ppSig msig
    prhs <- ppRhs "=" rhs
    pure $
      hsep (ppName name : ppats) <> psig <> prhs

  Match _ _ _ _ _ (Just binds) ->
    Left $ WhereClauseNotSupported binds

ppActivation :: Activation -> Doc
ppActivation = \case
  AlwaysActive ->
    mempty
  ActiveFrom phase ->
    "[" <> int phase <> "]"
  ActiveUntil phase ->
    "[~" <> int phase <> "]"

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

  TypeSig _ names ty -> do
    pty <- ppType ty
    pure $
      hcsep (fmap ppName names) <+> "::" <+> pty

  FunBind matches ->
    vsep <$> traverse ppMatch matches

  PatBind _ pat rhs Nothing -> do
    ppat <- ppPat pat
    prhs <- ppRhs "=" rhs
    pure $
      ppat <> prhs

  PatBind _ _ _ (Just binds) ->
    Left (WhereClauseNotSupported binds)

  -- haskell-src-exts doesn't support the INLINABLE pragma, but
  -- that's what we want, so we hack it by using an activation
  -- phase of -1 on the INLINE pragma.
  InlineSig _ True (ActiveFrom (-1)) qname -> do
    pure $
      "{-#" <+> "INLINABLE" <+> ppQName qname <+> "#-}"

  InlineSig _ inline activation qname -> do
    let
      inlineNo = if inline then mempty else "NO"
    pure $
      "{-#" <+> inlineNo <> "INLINE" <> ppActivation activation <+> ppQName qname <+> "#-}"

  AnnPragma _ ann -> do
    pann <- ppAnnotation ann
    pure $
      "{-# ANN" <+> pann <+> "#-}"

  decl ->
    unsupported "Decl" decl

ppDeclSpacing :: Decl -> Decl -> Doc
ppDeclSpacing x y =
  case (x, y) of
    (TypeSig{}, FunBind{}) ->
      line
    (FunBind{}, InlineSig{}) ->
      line
    (PatBind{}, PatBind{}) ->
      line
    (DataDecl{}, AnnPragma{}) ->
      line
    (_, _) ->
      line <> line

ppDecls :: [Decl] -> Either PrettyError Doc
ppDecls = \case
  [] ->
    pure mempty
  (d:ds) -> do
    pd <- ppDecl d
    pds <- traverse ppDecl ds
    pure . snd $
      foldl (\(x, px) (y, py) -> (y, px <> ppDeclSpacing x y <> py)) (d, pd) (List.zip ds pds)

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
  IThingWith name cnames ->
    ppName name <> "(" <> hcsep (fmap ppCName cnames) <> ")"

ppExportSpec :: ExportSpec -> Doc
ppExportSpec = \case
  EVar qname ->
    ppQName qname
  EAbs ns qname ->
    ppNamespace ns <+> ppQName qname
  EThingAll qname ->
    ppQName qname <> "(..)"
  EThingWith qname cnames ->
    ppQName qname <> "(" <> hcsep (fmap ppCName cnames) <> ")"
  EModuleContents modName ->
    "module" <+> ppModuleName modName

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

ppExports :: Maybe [ExportSpec] -> Doc
ppExports = \case
  Nothing ->
    mempty
  Just [] ->
    "(" <&>
    "  )"
  Just exports ->
    "(" <&>
    "  " <> align (vcsep (fmap ppExportSpec exports) <&> ")")

ppModule :: Module -> Either PrettyError Doc
ppModule = \case
  Module _ name pragmas Nothing mexports imports decls -> do
    ppragmas <- traverse ppPragma pragmas
    pdecls <- ppDecls decls
    pure $ vsep
      [ vsep ppragmas
      , "module" <+> ppModuleName name <+> ppExports mexports <+> "where"
      , mempty
      , ppImports imports
      , mempty
      , mempty
      , pdecls
      ]

  Module _ _ _ _ (Just exports) _ _ ->
    unsupported "Module" exports

  Module _ _ _ (Just warningText) _ _ _ ->
    unsupported "Module" warningText
