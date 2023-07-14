module Main where

import System.Environment (getArgs)
import System.Exit (die)

import Eval (execute)
import Syntax (parse)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then die "Usage: Brainfuck PROGRAM_FILE"
        else do
            maybeProg <- parse <$> readFile (head args)
            case maybeProg of
                Nothing -> die "Syntax error!"
                Just program -> execute program
