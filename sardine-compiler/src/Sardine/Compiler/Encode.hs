{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sardine.Compiler.Encode (
    encodeOfDefinition
  ) where

import           Control.Lens ((^.))

import qualified Data.List as List

import           Language.Haskell.Exts.QQ (hs)
import           Language.Haskell.Exts.Syntax

import           Language.Thrift.Types (Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Enum, Union, Struct)
import           Language.Thrift.Types (HasFields(..))
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (values, valueType)
import qualified Language.Thrift.Types as Thrift

import           P hiding (Enum, Alt, exp)

import           Sardine.Compiler.Analysis
import           Sardine.Compiler.Error
import           Sardine.Compiler.Monad
import           Sardine.Compiler.Names
import           Sardine.Compiler.TypeId
import           Sardine.Compiler.Util
import           Sardine.Haskell.Combinators

------------------------------------------------------------------------

data Encode =
    Encode Exp

data EncodeIO =
    EmptyEncodeIO
  | ExpEncodeIO Exp
  | AppEncodeIO EncodeIO EncodeIO

instance Monoid EncodeIO where
  mempty =
    EmptyEncodeIO
  mappend =
    AppEncodeIO

unEncode :: Encode -> Exp
unEncode (Encode exp) =
  exp

runEncode :: Encode -> Exp -> EncodeIO
runEncode (Encode enc) exp =
  ExpEncodeIO [hs| runEncode $(enc) $(exp) |]

unEncodeIO :: EncodeIO -> Exp
unEncodeIO = \case
  AppEncodeIO x EmptyEncodeIO ->
    unEncodeIO x
  AppEncodeIO EmptyEncodeIO y ->
    unEncodeIO y
  AppEncodeIO x y ->
    [hs| $(unEncodeIO x) . $(unEncodeIO y) |]
  ExpEncodeIO x ->
    x
  EmptyEncodeIO ->
    [hs| id |]

lamEncode :: Pat -> Exp -> Exp
lamEncode pat exp =
  let
    con = conE "Encode"
    lam = lamE [pat] exp
  in
    [hs| $(con) $ $(lam) |]

------------------------------------------------------------------------

encodeOfTypeReference :: TypeReference a -> Compiler a Encode
encodeOfTypeReference tref =
  let
    encodeSeq :: TypeReference a -> Compiler a Encode
    encodeSeq xref = do
      (loopP, loopE) <- lazyPE "loop"

      tidE <- intE <$> typeIdOfTypeReference xref
      encE <- encodeOfTypeReference xref

      pure . Encode $
        letE [patD loopP $ unEncode encE]
        [hs| encodeThriftList (TypeId $(tidE)) $(loopE) |]

    encodeMap :: TypeReference a -> TypeReference a -> Compiler a Encode
    encodeMap kref vref = do
      (kloopP, kloopE) <- lazyPE "kloop"
      (vloopP, vloopE) <- lazyPE "vloop"

      ktidE <- intE <$> typeIdOfTypeReference kref
      vtidE <- intE <$> typeIdOfTypeReference vref

      kencE <- encodeOfTypeReference kref
      vencE <- encodeOfTypeReference vref

      pure . Encode $
        letE
          [ patD kloopP $ unEncode kencE
          , patD vloopP $ unEncode vencE
          ]
        [hs| encodeThriftMap (TypeId $(ktidE)) (TypeId $(vtidE)) $(kloopE) $(vloopE) |]
  in
    case tref of
      SListType _ annot ->
        hoistCE (SListDeprecated annot)
      BoolType _ _ ->
        pure $ Encode [hs| encodeThriftBool |]
      ByteType _ _ ->
        pure $ Encode [hs| encodeThriftByte |]
      I16Type _ _ ->
        pure $ Encode [hs| encodeThriftI16 |]
      I32Type _ _ ->
        pure $ Encode [hs| encodeThriftI32 |]
      I64Type _ _ ->
        pure $ Encode [hs| encodeThriftI64 |]
      DoubleType _ _ ->
        pure $ Encode [hs| encodeThriftDouble |]
      StringType _ _ ->
        pure $ Encode [hs| encodeThriftString |]
      BinaryType _ _ ->
        pure $ Encode [hs| encodeThriftBinary |]
      ListType xref _ _ ->
        encodeSeq xref
      SetType xref _ _ ->
        encodeSeq xref
      MapType kref vref _ _ ->
        encodeMap kref vref
      DefinedType nam _ ->
        pure $ Encode [hs| $(encodeE nam) |]

tagOfTypeReference :: Exp -> Exp -> TypeReference a -> Compiler a Exp
tagOfTypeReference modE valE = \case
  BoolType _ _ ->
    pure [hs|
      case $(valE) of
        True ->
          $(intE 0x1) .|. $(modE)
        False ->
          $(intE 0x2) .|. $(modE)
    |]
  ref -> do
    tidE <- intE <$> typeIdOfTypeReference ref
    pure [hs|
      $(tidE) .|. $(modE)
    |]

encodeOfFieldTag :: Exp -> Field a -> Exp -> Compiler a EncodeIO
encodeOfFieldTag lastE field fieldE = do
  fidE <- intE <$> takeFieldId field

  let
    modE = [hs| fromIntegral (shiftL ($(fidE) - $(lastE)) 4) |]

  tagE <- tagOfTypeReference modE fieldE (field ^. valueType)

  pure . ExpEncodeIO $ [hs|
    if $(fidE) > $(lastE) && $(fidE) - $(lastE) < 15 then
      runEncode encodeWord8 $(tagE)
    else
      runEncode encodeWord8 $(tagE) .
      runEncode encodeVarInt16 $(fidE)
  |]

caseMaybeOfField ::
  Exp ->
  Field a ->
  Exp ->
  (Exp -> EncodeIO -> Exp) ->
  (Exp -> Compiler a EncodeIO) ->
  Compiler a Exp
caseMaybeOfField lastE field fieldE withFid just0 = do
  fid <- takeFieldId field
  let
    fidE =
      [hs| $(intE fid) :: Int16 |]
  case requiredness' field of
    Required ->
      withFid fidE <$> just0 fieldE
    Optional -> do
      (optP, optE) <- nextPE "opt"
      just1 <- just0 optE

      pure $ caseE fieldE
        [ alt (appP "Nothing" []) (withFid lastE mempty)
        , alt (appP "Just" [optP]) (withFid fidE just1)
        ]

encodeOfField :: Exp -> Field a -> Exp -> (Exp -> EncodeIO -> Exp) -> Compiler a Exp
encodeOfField lastE field fieldE withFid =
  caseMaybeOfField lastE field fieldE withFid $ \optE -> do
    tag <- encodeOfFieldTag lastE field optE
    case field ^. valueType of
      BoolType _ _ ->
        -- bools are packed in to the typeid
        pure tag
      tref -> do
        (tagP, tagE) <- lazyPE "tag"
        val <- encodeOfTypeReference tref
        pure . ExpEncodeIO $
          letE [patD tagP $ unEncodeIO tag]
          [hs| $(tagE) . $(unEncodeIO (runEncode val optE)) |]

stopTag :: EncodeIO
stopTag =
  ExpEncodeIO [hs| runEncode encodeWord8 0x0 |]

encodeOfStruct :: Struct a -> Compiler a [Decl]
encodeOfStruct struct = do
  let
    (valP, valE) =
      (bangP $ varP "val", varE "val")

    (vargsP, _) =
      List.unzip $ fmap (prefixPE "val_") (struct ^. fields)

    encodeFields :: Exp -> [Field a] -> Compiler a [(EncodeIO, [Decl])]
    encodeFields lastE = \case
      [] -> do
        pure [(stopTag, [])]

      x : xs -> do
        let
          (_, vargE) = prefixPE "val_" x
          (iargP, iargE) = prefixPE "ix_" x
          (xargP, xargE) = lazyPrefixPE "enc_" x

        fieldEs <- encodeFields iargE xs
        fieldE <- encodeOfField lastE x vargE $ \fidE enc -> [hs| ($fidE, $(unEncodeIO enc)) |]

        let
          fieldD =
            patD (bangP (tupP [iargP, xargP])) fieldE

        pure $ (ExpEncodeIO xargE, [fieldD]) : fieldEs

  (startP, startE) <- nextPE "start"
  (xargsE, encodes) <- fmap List.unzip . encodeFields startE . sortByFieldId $ struct ^. fields

  let
    encE =
      unEncodeIO $ mconcat xargsE

    funName =
      nameOfStructEncode struct

    funT =
      encodeT (typeOfStruct struct)

  pure . inlinableFunDT funT funName [] . lamEncode valP $
    letE [patD startP [hs| 0 :: Int16 |]] $
    caseE valE
      [ alt (patOfStruct struct vargsP) $
          letE (concat encodes) encE
      ]

encodeOfUnion :: Union a -> Compiler a [Decl]
encodeOfUnion union = do
  (startP, startE) <- nextPE "start"

  let
    funName =
      nameOfUnionEncode union

    funT =
      encodeT (typeOfUnion union)

    (altP, altE) =
      (bangP $ varP "alt", varE "alt")

    (valP, valE) =
      (bangP $ varP "val", varE "val")

    encodeField field = do
      exp <- ExpEncodeIO <$> encodeOfField startE field valE (const unEncodeIO)

      let
        (xargP, xargE) = lazyPrefixPE "enc_" field

      pure . alt (patOfUnionAlt union field valP) $
        letE [patD xargP $ unEncodeIO exp]
        (unEncodeIO $ ExpEncodeIO xargE <> stopTag)

  alts <- traverse encodeField (union ^. fields)

  pure . inlinableFunDT funT funName [] . lamEncode altP $
    letE [patD startP [hs| 0 :: Int16 |]] $
    caseE altE alts

encodeOfEnum :: Enum a -> Compiler a [Decl]
encodeOfEnum enum = do
  let
    pats =
      fmap (patOfEnumAlt enum) (enum ^. values)

    vals =
      valuesOfEnum (enum ^. values)

    patvals =
      List.sort $ List.zip pats vals

    altPV p v =
      alt p [hs|
        runEncode encodeVarInt32 $(intE v)
      |]

    (altP, altE) =
      (bangP $ varP "alt", varE "alt")

    funName =
      nameOfEnumEncode enum

    funT =
      encodeT (typeOfEnum enum)

  pure . inlineFunDT funT funName [] . lamEncode altP $
    caseE altE (fmap (uncurry altPV) patvals)

encodeOfThriftType :: ThriftType a -> Compiler a [Decl]
encodeOfThriftType = \case
  ThriftEnum enum ->
    encodeOfEnum enum
  ThriftStruct struct ->
    encodeOfStruct struct
  ThriftUnion union ->
    encodeOfUnion union

encodeOfType :: Ord a => Thrift.Type a -> Compiler a [Decl]
encodeOfType = \case
  TypedefType x ->
    hoistCE (TypedefNotSupported x)
  EnumType x ->
    encodeOfThriftType (ThriftEnum x)
  StructType x ->
    encodeOfThriftType (ThriftStruct x)
  UnionType x ->
    encodeOfThriftType (ThriftUnion x)
  SenumType x ->
    hoistCE (SenumDeprecated x)
  ExceptionType x ->
    hoistCE (ExceptionNotSupported x)

encodeOfDefinition :: Ord a => Definition a -> Compiler a [Decl]
encodeOfDefinition = \case
  ConstDefinition x ->
    hoistCE (ConstNotSupported x)
  ServiceDefinition x ->
    hoistCE (ServiceNotSupported x)
  TypeDefinition x ->
    encodeOfType x
