{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sardine.Pretty (
    module X
  , (<&>)
  , (<&&>)
  , text
  , hcsep
  , savageParens
  ) where

import           Data.Char (isSpace)
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

hcsep :: [Doc] -> Doc
hcsep =
  hcat . punctuate ", "

savageParens :: Doc -> Doc
savageParens doc =
  -- totally savage way to determine if we need parens, lol
  if any isSpace (show doc) then
    "(" <> doc <> ")"
  else
    doc
