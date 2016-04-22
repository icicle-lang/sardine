{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Sardine.Haskell.Combinators (
    varN
  , conN
  , conQ

  , (~>)
  , varT
  , conT
  , appT
  , intT

  , varE
  , conE
  , appE
  , intE
  , strE
  , caseE
  , lamE
  , doE
  , doE'
  , letE

  , appP
  , appP'
  , varP
  , intP
  , bangP
  , wildP
  , tupP

  , alt

  , (<~)
  , expS
  , expS'
  , letS
  , bindS

  , patD
  , funD
  , inlineFunD
  , inlineFunDT
  , inlinableFunDT

  , mapExpAlt
  , mapExpRhs
  , mapExpGRhs

  , pascalOfText
  , camelOfText
  , moduleOfText
  , acronymOfText
  ) where

import           Data.Char (toLower, toUpper, isUpper, isNumber)
import           Data.Generics.Geniplate (genUniverseBi', genTransformBi')
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import           Language.Haskell.Exts.SrcLoc (noLoc)
import           Language.Haskell.Exts.Syntax

import           P hiding (Enum, Alt, exp)


varN :: Text -> Name
varN =
  Ident . T.unpack . camelOfText

conN :: Text -> Name
conN =
  Ident . T.unpack . pascalOfText

conQ :: Text -> QName
conQ =
  UnQual . conN

infixr 4 ~>

(~>) :: Type -> Type -> Type
(~>) =
  TyFun

varT :: Text -> Type
varT =
  TyVar . Ident . T.unpack . camelOfText

conT :: Text -> Type
conT =
  TyCon . UnQual . Ident . T.unpack . pascalOfText

appT :: Type -> Type -> Type
appT =
  TyApp

intT :: Integer -> Type
intT =
  TyPromoted . PromotedInteger

varE :: Text -> Exp
varE =
  Var . UnQual . Ident . T.unpack . camelOfText

conE :: Text -> Exp
conE =
  Var . UnQual . Ident . T.unpack . pascalOfText

appE :: Exp -> Exp -> Exp
appE =
  App

intE :: Integer -> Exp
intE =
  Lit . Int

strE :: Text -> Exp
strE =
  Lit . String . T.unpack

caseE :: Exp -> [Alt] -> Exp
caseE =
  Case

lamE :: [Pat] -> Exp -> Exp
lamE =
  Lambda noLoc

doE :: [Stmt] -> Exp
doE =
  Do

doE' :: Exp -> Exp
doE' exp =
  Do (expS' exp)

letE :: [Decl] -> Exp -> Exp
letE decls =
  Let (BDecls decls)

expS :: Exp -> Stmt
expS exp =
  Qualifier exp

expS' :: Exp -> [Stmt]
expS' exp =
  [Qualifier exp]

letS :: [Decl] -> Stmt
letS decls =
  LetStmt (BDecls decls)

bindS :: Pat -> Exp -> Stmt
bindS pat exp =
  Generator noLoc pat exp

infixl 4 <~

(<~) :: Pat -> Exp -> Stmt
(<~) =
  bindS

appP :: Text -> [Pat] -> Pat
appP qname pats =
  PApp (UnQual . Ident . T.unpack . pascalOfText $ qname) pats

appP' :: Text -> Pat
appP' qname =
  appP qname []

varP :: Text -> Pat
varP =
  PVar . Ident . T.unpack . camelOfText

intP :: Integer -> Pat
intP x =
 PLit (if x >= 0 then Signless else Negative) . Int . abs $ x

bangP :: Pat -> Pat
bangP =
  PBangPat

wildP :: Pat
wildP =
  PWildCard

tupP :: [Pat] -> Pat
tupP =
  PTuple Boxed

patD :: Pat -> Exp -> Decl
patD pat exp =
  PatBind noLoc pat (UnGuardedRhs exp) Nothing

funD :: Name -> [Pat] -> Exp -> Decl
funD name pats exp =
  FunBind [ Match noLoc name pats Nothing (UnGuardedRhs exp) Nothing ]

inlineFunD :: Name -> [Pat] -> Exp -> [Decl]
inlineFunD name pats exp =
  [ FunBind . (:[]) $
      Match noLoc name pats Nothing (UnGuardedRhs exp) Nothing
  , InlineSig noLoc True AlwaysActive (UnQual name)
  ]

inlineFunDT :: Type -> Name -> [Pat] -> Exp -> [Decl]
inlineFunDT ty name pats exp =
  [ TypeSig noLoc [name] ty
  , FunBind . (:[]) $
      Match noLoc name pats Nothing (UnGuardedRhs exp) Nothing
  , InlineSig noLoc True AlwaysActive (UnQual name)
  ]

inlinableFunDT :: Type -> Name -> [Pat] -> Exp -> [Decl]
inlinableFunDT ty name pats exp =
  [ TypeSig noLoc [name] ty
  , FunBind . (:[]) $
      Match noLoc name pats Nothing (UnGuardedRhs exp) Nothing
  , InlineSig noLoc True (ActiveFrom (-1)) (UnQual name)
  ]

------------------------------------------------------------------------

mapExpGRhs :: (Exp -> Exp) -> GuardedRhs -> GuardedRhs
mapExpGRhs f = \case
  GuardedRhs loc stmts exp ->
    GuardedRhs loc stmts (f exp)

mapExpRhs :: (Exp -> Exp) -> Rhs -> Rhs
mapExpRhs f = \case
  UnGuardedRhs exp ->
    UnGuardedRhs (f exp)
  GuardedRhss rhss ->
    GuardedRhss (fmap (mapExpGRhs f) rhss)

mapExpAlt :: (Exp -> Exp) -> Alt -> Alt
mapExpAlt f = \case
  Alt loc pat rhs binds ->
    Alt loc pat (mapExpRhs f rhs) binds

------------------------------------------------------------------------

pascalOfText :: Text -> Text
pascalOfText txt =
  case T.uncons txt of
    Nothing ->
      T.empty
    Just (x, xs) ->
      toUpper x `T.cons` xs

camelOfText :: Text -> Text
camelOfText txt =
  case T.uncons txt of
    Nothing ->
      T.empty
    Just (x, xs) ->
      toLower x `T.cons` xs

moduleOfText :: Text -> Text
moduleOfText =
  T.intercalate "." . fmap pascalOfText . T.splitOn "."

acronymOfText :: Text -> Text
acronymOfText txt =
  case T.uncons txt of
    Nothing ->
      T.empty
    Just (x, xs) ->
      T.toLower $ x `T.cons` T.filter (\c -> isUpper c || isNumber c) xs

------------------------------------------------------------------------

alt :: Pat -> Exp -> Alt
alt pat exp =
  let
    vars = varsOfExp exp
    pat' = transformPat (wildVars vars) pat
  in
    Alt noLoc pat' (UnGuardedRhs exp) Nothing

wildVars :: Set QName -> Pat -> Pat
wildVars names = \case
  PVar name | Set.notMember (UnQual name) names ->
    PWildCard
  pat ->
    pat

varsOfExp :: Exp -> Set QName
varsOfExp = Set.fromList . $(genUniverseBi' [t| Exp -> [QName] |])

transformPat :: (Pat -> Pat) -> Pat -> Pat
transformPat = $(genTransformBi' [t| (Pat -> Pat) -> Pat -> Pat |])
