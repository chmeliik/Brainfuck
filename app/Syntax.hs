module Syntax (
    Instruction (..),
    parse,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, mzero)
import Control.Monad.State (State, get, modify, put, runState)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

data Instruction
    = MoveR
    | MoveL
    | Inc
    | Dec
    | Out
    | In
    | While [Instruction]
    deriving (Eq, Show)

parse :: String -> Maybe [Instruction]
parse s = do
    let (instrs, s') = runParser instrStar s
    guard (null s')
    instrs

type Parser a = MaybeT (State String) a

runParser :: Parser a -> String -> (Maybe a, String)
runParser = runState . runMaybeT

instrPlus :: Parser [Instruction]
instrPlus = do
    skipUnknown
    instr <- instrSingle <|> instrWhile
    instrs <- instrStar
    pure (instr : instrs)

instrStar :: Parser [Instruction]
instrStar = instrPlus <|> pure []

instrSingle :: Parser Instruction
instrSingle = do
    c <- oneOf "><+-.,"
    pure $ case c of
        '>' -> MoveR
        '<' -> MoveL
        '+' -> Inc
        '-' -> Dec
        '.' -> Out
        ',' -> In
        _ -> error "the oneOf parser should have made this impossible"

instrWhile :: Parser Instruction
instrWhile = do
    s0 <- get
    oneOf_ "["
    instrs <- instrStar
    oneOf_ "]" <|> do
        -- reset back to original state if we couldn't parse the while loop
        put s0
        mzero
    pure (While instrs)

oneOf :: String -> Parser Char
oneOf chars = do
    (c : s') <- get
    guard $ c `elem` chars
    put s'
    pure c

oneOf_ :: String -> Parser ()
oneOf_ s = oneOf s >> pure ()

skipUnknown :: Parser ()
skipUnknown = modify (dropWhile (`notElem` "><+-.,[]"))
