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
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)

import           Language.Haskell.Exts.QQ (hs)
import           Language.Haskell.Exts.Syntax

import           Language.Thrift.Types (Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Enum, Union, Struct)
import           Language.Thrift.Types (HasName(..), HasFields(..))
import           Language.Thrift.Types (Field, FieldRequiredness(..))
import           Language.Thrift.Types (values, valueType, identifier)
import qualified Language.Thrift.Types as Thrift

import           P hiding (Enum, Alt, exp)

import           Sardine.Compiler.Default
import           Sardine.Compiler.Error
import           Sardine.Compiler.Monad
import           Sardine.Compiler.Names
import           Sardine.Compiler.Util
import           Sardine.Haskell.Combinators


typeIdOfTypeReference :: TypeReference a -> Compiler a (NonEmpty Integer)
typeIdOfTypeReference = \case
  SListType _ annot ->
    hoistError (SListDeprecated annot)
  BoolType _ _ ->
    pure $ 0x1 :| [0x2]
  ByteType _ _ ->
    pure $ 0x3 :| []
  I16Type _ _ ->
    pure $ 0x4 :| []
  I32Type _ _ ->
    pure $ 0x5 :| []
  I64Type _ _ ->
    pure $ 0x6 :| []
  DoubleType _ _ ->
    pure $ 0x7 :| []
  StringType _ _ ->
    pure $ 0x8 :| []
  BinaryType _ _ ->
    pure $ 0x8 :| []
  ListType _ _ _ ->
    pure $ 0x9 :| []
  SetType _ _ _ ->
    pure $ 0xA :| []
  MapType _ _ _ _ ->
    pure $ 0xB :| []
  DefinedType _ _ ->
    pure $ 0x5 :| [0xC] -- TODO lookup 'ty' in env (enum == 0x5, union/struct = 0xC)

checksOfTypeReference :: Exp -> Exp -> TypeReference a -> Exp -> Exp -> Compiler a Exp
checksOfTypeReference outerE fieldE tref tidE elseE = do
  tids <- typeIdOfTypeReference tref

  let
    checks =
      foldr1 (\x y -> [hs| $(x) && $(y) |]) $
      fmap (\x -> [hs| $(tidE) /= TypeId $(intE x) |]) tids

  pure $ [hs|
    if $(checks) then
      decodeFail $! DecodeInvalidFieldType $(outerE) $(fieldE) $(tidE)
    else
      $(elseE)
  |]

decodeOfTypeReference :: Exp -> Exp -> TypeReference a -> Compiler a Exp
decodeOfTypeReference outerE fieldE tref =
  let
    decodeSeq :: TypeReference a -> Compiler a Exp
    decodeSeq elemTRef = do
      (tidP, tidE) <- nextPE "tid"
      (checkN, _, checkE) <- nextNPE "check"
      (loopN, _, loopE) <- nextNPE "loop"

      checksE <- checksOfTypeReference outerE fieldE elemTRef tidE [hs| return () |]
      decE <- decodeOfTypeReference outerE fieldE elemTRef

      pure $
        letE
          (inlineFunD checkN [tidP] checksE <>
           inlineFunD loopN [] decE)
        [hs| decodeList $(checkE) $(loopE) |]

    decodeMap :: TypeReference a -> TypeReference a -> Compiler a Exp
    decodeMap keyTRef valTRef = do
      (ktidP, ktidE) <- nextPE "ktid"
      (vtidP, vtidE) <- nextPE "vtid"
      (kcheckN, _, kcheckE) <- nextNPE "kcheck"
      (vcheckN, _, vcheckE) <- nextNPE "vcheck"
      (kloopN, _, kloopE) <- nextNPE "kloop"
      (vloopN, _, vloopE) <- nextNPE "vloop"

      ckeyE <- checksOfTypeReference outerE fieldE keyTRef ktidE [hs| return () |]
      cvalE <- checksOfTypeReference outerE fieldE valTRef vtidE [hs| return () |]

      dkeyE <- decodeOfTypeReference outerE fieldE keyTRef
      dvalE <- decodeOfTypeReference outerE fieldE valTRef

      pure $
        letE
          (inlineFunD kcheckN [ktidP] ckeyE <>
           inlineFunD vcheckN [vtidP] cvalE <>
           inlineFunD kloopN [] dkeyE <>
           inlineFunD vloopN [] dvalE)
        [hs| decodeMap $(kcheckE) $(vcheckE) $(kloopE) $(vloopE) |]

  in
    case tref of
      DefinedType ty _ ->
        -- TODO lookup 'ty' in environment (enum == 0x5, struct/union = 0xC)
        pure $ decodeE ty
      StringType _ _ -> do
        pure $ [hs| T.decodeUtf8 <$!> decodeBytes |]
      BinaryType _ _ ->
        pure $ [hs| decodeBytes |]
      SListType _ annot ->
        hoistError (SListDeprecated annot)
      BoolType _ _ -> do
        (boolP, boolE) <- nextPE "bool"
        pure . doE $
          [ boolP <~ [hs| decodeWord8 |]
          , expS [hs|
              if $(boolE) == 0 then
                return False
              else
                return True
            |]
          ]
      ByteType _ _ ->
        pure [hs| decodeWord8 |]
      I16Type _ _ ->
        pure [hs| decodeVarInt16 |]
      I32Type _ _ ->
        pure [hs| decodeVarInt32 |]
      I64Type _ _ ->
        pure [hs| decodeVarInt64 |]
      DoubleType _ _ ->
        pure [hs| decodeFloat64le |]
      MapType keyTRef valTRef _ _ ->
        decodeMap keyTRef valTRef
      SetType elemTRef _ _ ->
        decodeSeq elemTRef
      ListType elemTRef _ _ ->
        decodeSeq elemTRef

takeFieldId :: Field a -> Compiler a Integer
takeFieldId field =
  case field ^. identifier of
    Just i | i > 0 ->
      pure i
    Just i ->
      hoistError (FieldIdNotPositive field i)
    Nothing ->
      hoistError (FieldMissingId field)

takeBitIx :: Field a -> Compiler a Integer
takeBitIx field = do
  fid <- takeFieldId field
  case fid -1 of
    ix | ix >= 0 && ix < 64 ->
      pure $ fid - 1
    ix | ix >= 64 ->
      -- for now we only support up to 64 fields
      -- would be easy to fix, we just need to
      -- keep track of an extra Word64 bitfield
      -- for each additional 64 fields.
      hoistError (FieldIdTooLarge field fid)
    _ ->
      hoistError (FieldIdNotPositive field fid)


sortByFieldId :: [Field a] -> [Field a]
sortByFieldId =
  List.sortBy (comparing (^. identifier))

prefixPE :: Text -> Field a -> (Pat, Exp)
prefixPE pfx field =
  ( bangP . varP $ pfx <> field ^. name
  , varE $ pfx <> field ^. name )

decodeOfField ::
  HasName (outer s) =>
  HasFields outer =>
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
                decodeFail $! DecodeInvalidFieldType $(outerE) $(fnameE) $(tidE)
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

    mkLoopD spec =
      funD (varN "loop") $ [spec, lastP, bitsP] <> vargsP

    loop ix bits args =
      foldl appE [hs| loop SpecConstr $(ix) $(bits) |] args

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
                decodeFail $! DecodeMissingField
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
        [ expS $ [hs| skipType $(tidE) |]
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
            decodeVarInt16
        |]
      , expS $ caseE fidE (decodeAlts <> [decodeDefault])
      ]

  defaults <- traverse defaultOfFieldType $ struct ^. fields

  pure . inlinableFunDT funT funName [] $ doE
    [ letS
      [ mkLoopD (bangP $ appP' "SpecConstr2") (loop lastE bitsE vargsE)
      , mkLoopD (bangP $ appP' "SpecConstr") $ doE
        [ tagP <~ [hs| decodeWord8 |]
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
        [ doneP <~ [hs| decodeWord8 |]
        , expS [hs|
            if $(doneE) == 0 then
              return $! $(conOfUnionAlt union field) $(fieldE)
            else
              decodeFail $! DecodeMultipleUnionAlts $(strE (union ^. name))
          |]
        ]

    decodeDefault =
      alt wildP . doE' $ [hs|
        decodeFail $! DecodeUnknownUnionAlt $(strE (union ^. name)) $(fidE)
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
            decodeVarInt16
        |]
      , expS $ caseE fidE (decodeAlts <> [decodeDefault])
      ]

  pure . inlinableFunDT funT funName [] $ doE
    [ tagP <~ [hs| decodeWord8 |]
    , expS [hs|
        if $(tagE) == 0 then
          decodeFail $! DecodeMissingUnionAlt $(strE (union ^. name))
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
      [ varP "x" <~ [hs| decodeVarInt32 |]
      , expS . caseE (varE "x") $
          fmap (uncurry altVC) valcons <>
          [ alt wildP $ [hs|
              decodeFail $! DecodeInvalidEnumValue $(strE (enum ^. name)) x
            |]
          ]
      ]

decodeOfType :: Thrift.Type a -> Compiler a [Decl]
decodeOfType = \case
  TypedefType x ->
    hoistError (TypedefNotSupported x)
  EnumType x ->
    pure $ decodeOfEnum x
  StructType x ->
    decodeOfStruct x
  UnionType x ->
    decodeOfUnion x
  SenumType x ->
    hoistError (SenumDeprecated x)
  ExceptionType x ->
    hoistError (ExceptionNotSupported x)

decodeOfDefinition :: Definition a -> Compiler a [Decl]
decodeOfDefinition = \case
  ConstDefinition x ->
    hoistError (ConstNotSupported x)
  ServiceDefinition x ->
    hoistError (ServiceNotSupported x)
  TypeDefinition x ->
    decodeOfType x
