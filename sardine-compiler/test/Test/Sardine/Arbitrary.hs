{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Sardine.Arbitrary (
     ThriftIDL(..)
   , X(..)
   ) where

import           Control.Lens ((^.))
import           Control.Lens.Plated (universeOnOf, transformOnOf, rewriteOnOf)

import           Data.Data (Data)
import           Data.Data.Lens (biplate, template)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T

import           Disorder.Corpus

import           Language.Thrift.Types (Program(..), Header(..), Definition(..))
import           Language.Thrift.Types (Type(..), TypeReference(..))
import           Language.Thrift.Types (Enum(..), EnumDef(..), Union(..), Struct(..))
import           Language.Thrift.Types (Typedef(..), Exception(..), Senum(..))
import           Language.Thrift.Types (Field(..), FieldRequiredness(..))
import           Language.Thrift.Types (requiredness)

import           P hiding (Enum)

import           Sardine.Compiler.Util (valuesOfEnum)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


-- dummy annotation
data X = X
  deriving (Eq, Ord, Show, Data, Typeable)

type Name = Text

newtype ThriftIDL =
  ThriftIDL {
      unThriftIDL :: Program X
    } deriving (Eq, Show)

instance Arbitrary ThriftIDL where
  arbitrary =
    ThriftIDL . fixupProgram <$> genProgram
  shrink =
    fmap (ThriftIDL . fixupProgram) . shrinkProgram . unThriftIDL

genName :: [Name] -> Gen Name
genName xs = do
  name <- T.replace " " "" <$> elements xs
  index <- choose (0, 9) :: Gen Int
  return (name <> T.pack (show index))

genNames :: [Name] -> Gen [Name]
genNames xs =
  ordNub <$> listOf (genName xs)

genEnumDef :: Name -> Gen (EnumDef X)
genEnumDef name = do
  value <- arbitrary
  return (EnumDef name value [] Nothing X)

uniqEnumDefs :: [EnumDef a] -> [EnumDef a]
uniqEnumDefs defs =
  let
    values =
      valuesOfEnum defs

    defs' =
      fmap snd $
      List.nubBy ((==) `on` fst) $
      List.zip values defs

    values' =
      valuesOfEnum defs'
  in
    if values == values' then
      defs
    else
      -- removing a def can change the value of
      -- subsequent defs, so run to a fixed point
      uniqEnumDefs defs'

genEnum :: Name -> Gen (Enum X)
genEnum name = do
  vnames <- genNames cooking
  values <- traverse genEnumDef vnames
  return (Enum name (uniqEnumDefs values) [] Nothing X)

genTypeReference :: [Name] -> Gen (TypeReference X)
genTypeReference env = do
  oneof
    [ DefinedType <$> elements env <*> pure X
    , pure $ StringType [] X
    , pure $ BinaryType [] X
  --, pure $ SListType [] X -- deprecated by thrift, not supported
    , pure $ BoolType [] X
    , pure $ ByteType [] X
    , pure $ I16Type [] X
    , pure $ I32Type [] X
    , pure $ I64Type [] X
    , pure $ DoubleType [] X
    , MapType <$> genTypeReference env <*> genTypeReference env <*> pure [] <*> pure X
    , SetType <$> genTypeReference env <*> pure [] <*> pure X
    , ListType <$> genTypeReference env <*> pure [] <*> pure X
    ]

genField :: [Name] -> Name -> Maybe (Positive Int) -> Gen (Field X)
genField env name pfid = do
  let fid = fmap (fromIntegral . getPositive) pfid
  required <- elements [Nothing, Just Required, Just Optional]
  ftype <- genTypeReference (filter (/= name) env)
  return (Field fid required ftype name Nothing [] Nothing X)

genFields :: [Name] -> Gen [Field X]
genFields env = do
  fnames <- genNames viruses
  fids <- fmap Just . List.take (length fnames) . List.nub <$> infiniteListOf arbitrary
  zipWithM (genField env) fnames fids

genStruct :: [Name] -> Name -> Gen (Struct X)
genStruct env name = do
  fields <- genFields env
  return (Struct name fields [] Nothing X)

genUnion :: [Name] -> Name -> Gen (Union X)
genUnion env name = do
  fields <- filter (\x -> x ^. requiredness /= Just Optional) <$> genFields env
  return (Union name fields [] Nothing X)

genType :: [Name] -> Name -> Gen (Type X)
genType env name =
  oneof
    [ EnumType <$> genEnum name
    , StructType <$> genStruct env name
    , UnionType <$> genUnion env name ]

genDefinition :: [Name] -> Name -> Gen (Definition X)
genDefinition env name =
  TypeDefinition <$> genType env name

genProgram :: Gen (Program X)
genProgram = do
  dnames <- genNames muppets
  defs <- traverse (genDefinition dnames) dnames
  return (Program [] defs)

shrinkEnumDef :: EnumDef X -> [EnumDef X]
shrinkEnumDef = \case
  EnumDef name value _ _ X ->
    [ EnumDef name value' [] Nothing X | value' <- shrink value ]

shrinkEnum :: Enum X -> [Enum X]
shrinkEnum = \case
  Enum name values _ _ X ->
    [ Enum name (uniqEnumDefs values') [] Nothing X
    | values' <- shrinkList shrinkEnumDef values ]

shrinkRequired :: Maybe FieldRequiredness -> [Maybe FieldRequiredness]
shrinkRequired = \case
  Nothing ->
    []
  Just Required ->
    [Nothing]
  Just Optional ->
    [Nothing, Just Required]

shrinkTypeReference :: TypeReference X -> [TypeReference X]
shrinkTypeReference = \case
  DefinedType _ X ->
    []
  StringType _ X ->
    []
  BinaryType _ X ->
    []
  SListType _ X ->
    []
  BoolType _ X ->
    []
  ByteType _ X ->
    []
  I16Type _ X ->
    []
  I32Type _ X ->
    []
  I64Type _ X ->
    []
  DoubleType _ X ->
    []
  MapType kt vt _ X ->
    [ kt, vt ] <>
    [ MapType kt' vt [] X | kt' <- shrinkTypeReference kt ] <>
    [ MapType kt vt' [] X | vt' <- shrinkTypeReference vt ]
  SetType ty _ X ->
    [ ty ] <>
    [ SetType ty' [] X | ty' <- shrinkTypeReference ty ]
  ListType ty _ X ->
    [ ty ] <>
    [ ListType ty' [] X | ty' <- shrinkTypeReference ty ]

shrinkField :: Field X -> [Field X]
shrinkField = \case
  Field fid req typ name def _ _ X ->
    [ Field fid req' typ name def [] Nothing X | req' <- shrinkRequired req ] <>
    [ Field fid req typ' name def [] Nothing X | typ' <- shrinkTypeReference typ ]

shrinkStruct :: Struct X -> [Struct X]
shrinkStruct = \case
  Struct name fields _ _ X ->
    [ Struct name fields' [] Nothing X | fields' <- shrinkList shrinkField fields ]

shrinkUnion :: Union X -> [Union X]
shrinkUnion = \case
  Union name fields _ _ X ->
    [ Union name fields' [] Nothing X | fields'@(_:_) <- shrinkList shrinkField fields ]

shrinkType :: Type X -> [Type X]
shrinkType = \case
  TypedefType _ ->
    []
  EnumType enum ->
    EnumType <$> shrinkEnum enum
  StructType struct ->
    StructType <$> shrinkStruct struct
  UnionType union ->
    UnionType <$> shrinkUnion union
  ExceptionType _ ->
    []
  SenumType _ ->
    []

shrinkDefinition :: Definition X -> [Definition X]
shrinkDefinition = \case
  ConstDefinition _ ->
    []
  TypeDefinition typ ->
    TypeDefinition <$> shrinkType typ
  ServiceDefinition _ ->
    []

shrinkHeader :: Header X -> [Header X]
shrinkHeader =
  shrinkNothing

shrinkProgram :: Program X -> [Program X]
shrinkProgram = \case
  Program hs ds ->
    [ Program hs' ds | hs' <- shrinkList shrinkHeader hs ] <>
    [ Program hs ds' | ds' <- shrinkList shrinkDefinition ds ]

nameOfType :: Type X -> Text
nameOfType = \case
  TypedefType (Typedef _ name _ _ _) ->
    name
  EnumType (Enum name _ _ _ _) ->
    name
  StructType (Struct name _ _ _ _) ->
    name
  UnionType (Union name _ _ _ _) ->
    name
  ExceptionType (Exception name _ _ _ _) ->
    name
  SenumType (Senum name _ _ _ _) ->
    name

namesOfTypes :: Program X -> Set Text
namesOfTypes =
  Set.fromList . fmap nameOfType . universeOnOf biplate template

hasValidType' :: Set Text -> TypeReference X -> Bool
hasValidType' names = \case
  DefinedType typ X ->
    Set.member typ names
  StringType _ X ->
    True
  BinaryType _ X ->
    True
  SListType _ X ->
    True
  BoolType _ X ->
    True
  ByteType _ X ->
    True
  I16Type _ X ->
    True
  I32Type _ X ->
    True
  I64Type _ X ->
    True
  DoubleType _ X ->
    True
  MapType kt vt _ X ->
    hasValidType' names kt &&
    hasValidType' names vt
  SetType ty _ X ->
    hasValidType' names ty
  ListType ty _ X ->
    hasValidType' names ty

hasValidType :: Set Text -> Field X -> Bool
hasValidType names = \case
  Field _ _ typ _ _ _ _ X ->
    hasValidType' names typ

fixupFields :: Set Text -> [Field X] -> [Field X]
fixupFields names =
  List.filter (hasValidType names)

isValidDefinition :: Definition X -> Bool
isValidDefinition = \case
  ConstDefinition _ ->
    True
  TypeDefinition (EnumType (Enum _ [] _ _ X)) ->
    False -- uninhabited enums are not allowed
  TypeDefinition (UnionType (Union _ [] _ _ X)) ->
    False -- uninhabited unions are not allowed
  TypeDefinition _ ->
    True
  ServiceDefinition _ ->
    True

fixupDefinitions :: [Definition X] -> [Definition X]
fixupDefinitions =
  List.filter isValidDefinition

transformEq :: (Data to, Data from, Eq from) => (to -> to) -> from -> Maybe from
transformEq t p =
  let
    p' = transformOnOf biplate template t p
  in
    if p /= p' then
      Just p'
    else
      Nothing

fixupProgram :: Program X -> Program X
fixupProgram =
  rewriteOnOf biplate template $ \p ->
    transformEq (fixupFields (namesOfTypes p)) p <|>
    transformEq fixupDefinitions p
