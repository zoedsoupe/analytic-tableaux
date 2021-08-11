{-# LANGUAGE OverloadedStrings #-}

module Language.Tableaux.Parser where

import Control.Applicative ((*>))

import Data.Text (Text)
import qualified Data.Text as Text

import Text.Parsec

type Parser = Parsec Text ()

data InputMetadata = InputMetadata
    { metadataLine :: Int
    , metadataColumn :: Int
    }
    deriving (Show)

data Operator
    = Not
    | And
    | Or
    | Implies
    | Equiv
    | Prove
    deriving (Show)

data Sign = T | F deriving (Show)

data TableauxInput
    = TableauxAtom InputMetadata Text
    | TableauxSign InputMetadata Sign
    | TableauxOperator InputMetadata Operator
    | TableauxWff InputMetadata TableauxInput
    deriving (Show)

getInputMetadata :: Parser InputMetadata
getInputMetadata = do
    position <- statePos <$> getParserState
    return $
        InputMetadata
            { metadataLine = sourceLine position
            , metadataColumn = sourceColumn position
            }

parseAtom :: Parser TableauxInput
parseAtom = do
    meta <- getInputMetadata
    first <- lower
    rest <- many (lower <|> digit)
    return $ TableauxAtom meta (Text.pack (first : rest))

parseSign :: Parser TableauxInput
parseSign = do
    meta <- getInputMetadata
    sign <- upper
    case sign of
        'T' -> return $ TableauxSign meta T
        'F' -> return $ TableauxSign meta F

parseTableaux :: Parser TableauxInput
parseTableaux = parseAtom <|> parseSign
