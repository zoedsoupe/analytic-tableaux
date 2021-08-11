{-# LANGUAGE OverloadedStrings #-}

module Language.Tableaux.Parser where

import Control.Applicative ((*>))
import Control.Monad (liftM)

import Data.Text (Text)
import qualified Data.Text as Text

import Text.Parsec

type Parser = Parsec Text ()

parseTableaux = ""
