module Language.Tableaux.ParserSpec where

import Data.Text (singleton)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Parsec
import Test.QuickCheck
import Test.QuickCheck.Unicode

import Text.Parsec (parse)

import Language.Tableaux.Parser

instance Arbitrary Sign where
  arbitrary = chooseEnum (T, F)

instance Arbitrary Operator where
  arbitrary = intToOp <$> choose (1, 6)

instance Arbitrary TableauxInput where
  arbitrary = intToTableaux <$> choose (1, 4)

main :: Spec
main = undefined

intToOp 1 = Not <$> elements "¬~!"
intToOp 2 = And <$> elements "∧&·"
intToOp 3 = Or <$> elements "∨+∥"
intToOp 4 = Implies <$> elements "⇒→⊃"
intToOp 5 = Equiv <$> elements "⇔≡↔"
intToOp 6 = Prove <$> elements "⊢"

intToTableaux 1 = TableauxAtom <$> inputMetadata <*> letter
intToTableaux 2 = TableauxSign <$> inputMetadata <*> arbitrary
intToTableaux 3 = TableauxOperator <$> inputMetadata <*> arbitrary
intToTableaux 4 = TableauxWff <$> inputMetadata <*> arbitrary

letter = frequency [(26, choose ('a', 'z'))]
inputMetadata = InputMetadata <$> arbitrary <*> arbitrary
