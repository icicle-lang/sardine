{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Sardine.Compiler.Decode (
    decodeOfDefinition
  ) where

import           Control.Lens ((^.))

import           Data.Foldable (foldr1)
import qualified Data.List as List

import           Language.Haskell.Exts.QQ (hs)
import           Language.Haskell.Exts.Syntax

import           Language.Thrift.AST (Definition(..))
import           Language.Thrift.AST (Type(..), TypeReference(..))
import           Language.Thrift.AST (Enum, Union, Struct)
import           Language.Thrift.AST (HasName(..), HasFields(..))
import           Language.Thrift.AST (Field, FieldRequiredness(..))
import           Language.Thrift.AST (values, valueType)
import qualified Language.Thrift.AST as Thrift

import           P hiding (Enum, Alt, exp)

import           Sardine.Compiler.Default
import           Sardine.Compiler.Error
import           Sardine.Compiler.Monad
import           Sardine.Compiler.Names
import           Sardine.Compiler.TypeId
import           Sardine.Compiler.Util
import           Sardine.Haskell.Combinators


checksOfTypeReference :: Exp -> Exp -> TypeReference a -> Exp -> Exp -> Compiler a Exp
checksOfTypeReference outerE fieldE tref tidE elseE = do
  tids <- typeIdsOfTypeReference tref

  let
    checks =
      foldr1 (\x y -> [hs| $(x) && $(y) |]) $
      fmap (\x -> [hs| $(tidE) /= TypeId $(intE x) |]) tids

  pure $ [hs|
    if $(checks) then
      decodeFail $! ThriftInvalidFieldType $(outerE) $(fieldE) $(tidE)
    else
      $(elseE)
  |]

decodeOfTypeReference :: Exp -> Exp -> TypeReference a -> Compiler a Exp
decodeOfTypeReference outerE fieldE tref =
  let
    decodeSeq :: TypeReference a -> Compiler a Exp
    decodeSeq xref = do
      (tidP, tidE) <- nextPE "tid"
      (checkN, _, checkE) <- nextNPE "check"
      (loopN, _, loopE) <- nextNPE "loop"

      checksE <- checksOfTypeReference outerE fieldE xref tidE [hs| return () |]
      decE <- decodeOfTypeReference outerE fieldE xref

      pure $
        letE
          (inlineFunD checkN [tidP] checksE <>
           inlineFunD loopN [] decE)
        [hs| decodeThriftList $(checkE) $(loopE) |]

    decodeMap :: TypeReference a -> TypeReference a -> Compiler a Exp
    decodeMap kref vref = do
      (ktidP, ktidE) <- nextPE "ktid"
      (vtidP, vtidE) <- nextPE "vtid"
      (kcheckN, _, kcheckE) <- nextNPE "kcheck"
      (vcheckN, _, vcheckE) <- nextNPE "vcheck"
      (kloopN, _, kloopE) <- nextNPE "kloop"
      (vloopN, _, vloopE) <- nextNPE "vloop"

      ckeyE <- checksOfTypeReference outerE fieldE kref ktidE [hs| return () |]
      cvalE <- checksOfTypeReference outerE fieldE vref vtidE [hs| return () |]

      dkeyE <- decodeOfTypeReference outerE fieldE kref
      dvalE <- decodeOfTypeReference outerE fieldE vref

      pure $
        letE
          (inlineFunD kcheckN [ktidP] ckeyE <>
           inlineFunD vcheckN [vtidP] cvalE <>
           inlineFunD kloopN [] dkeyE <>
           inlineFunD vloopN [] dvalE)
        [hs| decodeThriftMap $(kcheckE) $(vcheckE) $(kloopE) $(vloopE) |]

  in
    case tref of
      DefinedType ty _ ->
        pure $ decodeE ty
      StringType _ _ ->
        pure $ [hs| decodeThriftString |]
      BinaryType _ _ ->
        pure $ [hs| decodeThriftBinary |]
      SListType _ annot ->
        hoistCE (SListDeprecated annot)
      BoolType _ _ ->
        pure [hs| decodeThriftBool |]
      ByteType _ _ ->
        pure [hs| decodeThriftByte |]
      I16Type _ _ ->
        pure [hs| decodeThriftI16 |]
      I32Type _ _ ->
        pure [hs| decodeThriftI32 |]
      I64Type _ _ ->
        pure [hs| decodeThriftI64 |]
      DoubleType _ _ ->
        pure [hs| decodeThriftDouble |]
      ListType xref _ _ ->
        decodeSeq xref
      SetType xref _ _ ->
        decodeSeq xref
      MapType kref vref _ _ ->
        decodeMap kref vref

takeBitIx :: Field a -> Compiler a Integer
takeBitIx field = do
  fid <- takeFieldId field
  case fid - 1 of
    ix | ix >= 0 && ix < 64 ->
      pure $ fid - 1
    ix | ix >= 64 ->
      -- for now we only support up to 64 fields
      -- would be easy to fix, we just need to
      -- keep track of an extra Word64 bitfield
      -- for each additional 64 fields.
      hoistCE (FieldIdTooLarge field fid)
    _ ->
      hoistCE (FieldIdNotPositive field fid)

decodeOfField ::
  HasName (outer s) =>
  Exp ->
  outer s ->
  Field a ->
  Pat ->
  [Stmt] ->
  Compiler a Alt
decodeOfField tidE outer field bindP after = do
  let
    outerE =
      strE (outer ^. name)

    fnameE =
      strE (field ^. name)

    tref =
      field ^. valueType

  fidP <- intP <$> takeFieldId field

  case tref of
    BoolType _ _ -> do
      pure . alt fidP . doE $
        [ bindP <~ [hs|
            case $(tidE) of
              TypeId 0x1 ->
                return True
              TypeId 0x2 ->
                return False
              _ ->
                decodeFail $! ThriftInvalidFieldType $(outerE) $(fnameE) $(tidE)
          |]
        ] <> after

    _ -> do
      decE <- decodeOfTypeReference outerE fnameE tref
      checkE <- checksOfTypeReference outerE fnameE tref tidE . doE $
        [ bindP <~ decE ] <> after

      pure . alt fidP $ checkE

decodeOfStruct :: Struct a -> Compiler a [Decl]
decodeOfStruct struct = do
  (tagP, tagE) <- nextPE "tag"
  (tidP, tidE) <- nextPE "tid"
  (modP, modE) <- nextPE "mod"
  (fidP, fidE) <- nextPE "fid"
  (lastP, lastE) <- nextPE "last"
  (bitsP, bitsE) <- nextPE "bits"

  let
    funName =
      nameOfStructDecode struct

    funT =
      decodeT (typeOfStruct struct)

    (vargsP, vargsE) =
      List.unzip $ fmap (prefixPE "v_") (struct ^. fields)

    (_, xargsE) =
      List.unzip $ fmap (prefixPE "x_") (struct ^. fields)

    mkStruct =
      foldl appE (conOfStruct struct) xargsE

    loop ix bits args =
      foldl appE [hs| loop $(ix) $(bits) |] args

    unpackField field = do
      bitIxE <- intE <$> takeBitIx field
      let
        (_, v) = prefixPE "v_" field
        (x, _) = prefixPE "x_" field
      pure $
        case requiredness' field of
          Optional ->
            x <~ [hs|
              if testBit $(bitsE) $(bitIxE) then
                return $! Just $(v)
              else
                return Nothing
            |]
          Required ->
            x <~ [hs|
              if testBit $(bitsE) $(bitIxE) then
                return $(v)
              else
                decodeFail $! ThriftMissingField
                  $(strE (struct ^. name))
                  $(strE (field ^. name))
            |]

  unpackFields <- traverse unpackField (struct ^. fields)

  let
    unpack = doE $
      unpackFields <>
      expS' [hs| return $! $(mkStruct) |]

    retOrPass fieldE field other =
      if field ^.name == other ^. name then
        fieldE
      else
        snd (prefixPE "v_" other)

    decodeField field = do
      (fieldP, fieldE) <- nextPE "field"
      bitIxE <- intE <$> takeBitIx field
      let
        bitsE' = [hs|
          setBit $(bitsE) $(bitIxE)
        |]
      decodeOfField tidE struct field fieldP . expS' $
        loop fidE bitsE' . fmap (retOrPass fieldE field) $ struct ^. fields

  decodeAlts <- traverse decodeField . sortByFieldId $ struct ^. fields

  let
    decodeDefault =
      alt wildP . doE $
        [ expS $ [hs| skipThriftType $(tidE) |]
        , expS $ loop fidE bitsE vargsE ]

    decode = doE $
      [ letS
        [ patD tidP [hs| TypeId $! $(tagE) .&. 0x0F |]
        , patD modP [hs| $(tagE) `shiftR` 4 |]
        ]
      , fidP <~ [hs|
          if $(modE) /= 0 then
            return $! $(lastE) + fromIntegral $(modE)
          else
            first ThriftVarIntError decodeVarInt16
        |]
      , expS $ caseE fidE (decodeAlts <> [decodeDefault])
      ]

  defaults <- traverse defaultOfFieldType $ struct ^. fields

  pure . inlinableFunDT funT funName [] $ doE
    [ letS
      [ funD (varN "loop") ([lastP, bitsP] <> vargsP) $ doE
        [ tagP <~ [hs| first absurd decodeWord8 |]
        , expS [hs|
            if $(tagE) == 0 then
              $(unpack)
            else
              $(decode)
          |]
        ]
      ]
    , expS $ loop (intE 0) [hs| 0 :: Word64 |] defaults
    ]

decodeOfUnion :: Union a -> Compiler a [Decl]
decodeOfUnion union = do
  (tagP, tagE) <- nextPE "tag"
  (tidP, tidE) <- nextPE "tid"
  (modP, modE) <- nextPE "mod"
  (fidP, fidE) <- nextPE "fid"

  let
    funName =
      nameOfUnionDecode union

    funT =
      decodeT (typeOfUnion union)

    decodeField field = do
      (doneP, doneE) <- nextPE "done"
      (fieldP, fieldE) <- nextPE "field"
      decodeOfField tidE union field fieldP $
        [ doneP <~ [hs| first absurd decodeWord8 |]
        , expS [hs|
            if $(doneE) == 0 then
              return $! $(conOfUnionAlt union field) $(fieldE)
            else
              decodeFail $! ThriftMultipleUnionAlts $(strE (union ^. name))
          |]
        ]

    decodeDefault =
      alt wildP . doE' $ [hs|
        decodeFail $! ThriftUnknownUnionAlt $(strE (union ^. name)) $(fidE)
      |]

  decodeAlts <- traverse decodeField $ sortByFieldId (union ^. fields)

  let
    decode = doE $
      [ letS
        [ patD tidP [hs| TypeId $! $(tagE) .&. 0x0F |]
        , patD modP [hs| $(tagE) `shiftR` 4 |]
        ]
      , fidP <~ [hs|
          if $(modE) /= 0 then
            return $! fromIntegral $(modE)
          else
            first ThriftVarIntError decodeVarInt16
        |]
      , expS $ caseE fidE (decodeAlts <> [decodeDefault])
      ]

  pure . inlinableFunDT funT funName [] $ doE
    [ tagP <~ [hs| first absurd decodeWord8 |]
    , expS [hs|
        if $(tagE) == 0 then
          decodeFail $! ThriftMissingUnionAlt $(strE (union ^. name))
        else
          $(decode)
      |]
    ]

decodeOfEnum :: Enum a -> [Decl]
decodeOfEnum enum =
  let
    funName =
      nameOfEnumDecode enum

    funT =
      decodeT (typeOfEnum enum)

    cons =
      fmap (conOfEnumAlt enum) (enum ^. values)

    vals =
      valuesOfEnum (enum ^. values)

    valcons =
      List.sort $ List.zip vals cons

    altVC v c =
      alt (intP v) [hs| return $(c) |]
  in
    inlinableFunDT funT funName [] $ doE
      [ bangP (varP "x") <~ [hs| first ThriftVarIntError decodeVarInt32 |]
      , expS . caseE (varE "x") $
          fmap (uncurry altVC) valcons <>
          [ alt wildP $ [hs|
              decodeFail $! ThriftInvalidEnumValue $(strE (enum ^. name)) x
            |]
          ]
      ]

decodeOfType :: Thrift.Type a -> Compiler a [Decl]
decodeOfType = \case
  TypedefType x ->
    hoistCE (TypedefNotSupported x)
  EnumType x ->
    pure $ decodeOfEnum x
  StructType x ->
    decodeOfStruct x
  UnionType x ->
    decodeOfUnion x
  SenumType x ->
    hoistCE (SenumDeprecated x)
  ExceptionType x ->
    hoistCE (ExceptionNotSupported x)

decodeOfDefinition :: Definition a -> Compiler a [Decl]
decodeOfDefinition = \case
  ConstDefinition x ->
    hoistCE (ConstNotSupported x)
  ServiceDefinition x ->
    hoistCE (ServiceNotSupported x)
  TypeDefinition x ->
    decodeOfType x
