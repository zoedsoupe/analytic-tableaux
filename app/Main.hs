module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)

import qualified Data.Text as Text

import Lib

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "parser: No input. Exiting."
        else
            let input = Text.pack (head args)
             in print (parse parseTableaux "<stdio>" input)
