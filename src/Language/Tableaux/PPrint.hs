{-# LANGUAGE OverloadedStrings #-}

module Language.Tableaux.PPrint where

import Language.Tableaux.Parser

import Data.Text (Text, pack, singleton)
import Prettyprinter (Doc, Pretty, defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)

instance Pretty TableauxInput where
    -- simple propositions
    pretty (TableauxAtom _ atom) = pretty atom
    -- signs
    pretty (TableauxSign _ T) = (pretty . pack) "T"
    pretty (TableauxSign _ F) = (pretty . pack) "F"
    -- operators
    pretty (TableauxOperator _ (Not ch)) = (pretty . singleton) ch
    pretty (TableauxOperator _ (And ch)) = (pretty . singleton) ch
    pretty (TableauxOperator _ (Or ch)) = (pretty . singleton) ch
    pretty (TableauxOperator _ (Implies ch)) = (pretty . singleton) ch
    pretty (TableauxOperator _ (Equiv ch)) = (pretty . singleton) ch
    pretty (TableauxOperator _ (Prove ch)) = (pretty . singleton) ch
    -- well formed formulas\
    pretty (TableauxWff _ wff) = pretty '(' <+> prettyWff <+> pretty ')'
      where
        prettyWff = foldl1 (\acc f -> acc <+> f) (pretty <$> wff)

render :: Doc a -> Text
render doc = renderStrict $ layoutPretty defaultLayoutOptions doc
