{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Compiler.Pretty (
    module X
  , (<&>)
  , (<&&>)
  , text
  , takeAcronym
  , encloseVSep
  , withParens

  , Eval(..)
  , withEval
  ) where

import           Data.Char (isUpper, isNumber, isSpace, toLower)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           P hiding (Const(..), Enum)

import           Text.PrettyPrint.ANSI.Leijen as X hiding ((<>), (<$>), (<$$>), empty, text)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen


infixr 5 <&>, <&&>

(<&>) :: Doc -> Doc -> Doc
(<&>) = (Leijen.<$>)

(<&&>) :: Doc -> Doc -> Doc
(<&&>) = (Leijen.<$$>)

text :: Text -> Doc
text =
  string . T.unpack

takeAcronym :: Doc -> Doc
takeAcronym doc =
  string . fmap toLower . filter (\c -> isUpper c || isNumber c) $ show doc

encloseVSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseVSep left right separator = \case
  []  -> align (right)
  [d] -> align (left <> d <> line <> right)
  ds  -> align (vsep (List.zipWith (<>) (left : List.repeat separator) ds) <> line <> right)

withParens :: Doc -> Doc
withParens doc =
  -- totally savage way to determine if we need parens, lol
  if any isSpace (show doc) then
    "(" <> doc <> ")"
  else
    doc

data Eval =
    Default
  | Strict
    deriving (Eq, Ord, Show)

withEval :: Eval -> Doc -> Doc
withEval eval doc =
  case eval of
    Default ->
      doc
    Strict ->
      "!" <> withParens doc
