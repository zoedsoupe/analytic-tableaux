module Language.Tableaux.ParserSpec where

import Data.Text (singleton)

import Test.Hspec
import Test.Hspec.Parsec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Utf8 (genValidUtf8)

import Text.Parsec (parse)

import Language.Tableaux.Parser

instance Arbitrary Sign where
    arbitrary = chooseEnum (T, F)

instance Arbitrary Operator where
    arbitrary = oneof
      [ Not <$> elements "¬~!"
      , And <$> elements "∧&·"
      , Or <$> elements "∨+∥"
      , Implies <$> elements "⇒→⊃"
      , Equiv <$> elements "⇔≡↔"
      , Prove <$> elements "⊢"
      ]

instance Arbitrary TableauxInput where
    arbitrary = oneof
      [ TableauxAtom <$> inputMetadata <*> genValidUtf8
      , TableauxSign <$> inputMetadata <*> arbitrary
      , TableauxOperator <$> inputMetadata <*> arbitrary
      , TableauxWff <$> inputMetadata <*> arbitrary
      ]
      where
        inputMetadata = InputMetadata <$> arbitrary <*> arbitrary

spec :: Spec
spec = undefined
