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
    = Not Char
    | And Char
    | Or Char
    | Implies Char
    | Equiv Char
    | Prove Char
    deriving (Show)

data Sign = T | F deriving (Show)

data TableauxInput
    = TableauxAtom InputMetadata Text
    | TableauxSign InputMetadata Sign
    | TableauxOperator InputMetadata Operator
    | TableauxWff InputMetadata TableauxInput
    deriving (Show)

parseNot :: Parser Operator
parseNot = do
    ch <- oneOf "¬~!"
    return $ Not ch

parseAnd :: Parser Operator
parseAnd = do
    ch <- oneOf "∧&·"
    return $ And ch

parseOr :: Parser Operator
parseOr = do
    ch <- oneOf "∨+∥"
    return $ Or ch

parseImplies :: Parser Operator
parseImplies = do
    ch <- oneOf "⇒→⊃"
    return $ Implies ch

parseEquiv :: Parser Operator
parseEquiv = do
    ch <- oneOf "⇔≡↔"
    return $ Equiv ch

parseProve :: Parser Operator
parseProve = do
    ch <- char '⊢'
    return $ Prove ch

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

parseOperator :: Parser TableauxInput
parseOperator = do
    meta <- getInputMetadata
    op <- parseNot <|> parseAnd <|> parseOr <|> parseImplies <|> parseEquiv <|> parseProve
    return $ TableauxOperator meta op

parseTableaux :: Parser TableauxInput
parseTableaux = parseAtom <|> parseSign <|> parseOperator
