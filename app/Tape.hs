module Tape (
    Tape,
    empty,
    moveL,
    moveR,
    inc,
    dec,
    read,
    write,
) where

import Prelude hiding (read)

import Data.Word (Word8)

data Tape = Tape {_tapeLeft :: [Word8], tapeHead :: Word8, _tapeRight :: [Word8]}

empty :: Tape
empty = Tape [] 0 []

moveL :: Tape -> Tape
moveL (Tape [] h r) = Tape [] 0 (h : r)
moveL (Tape (h' : l') h r) = Tape l' h' (h : r)

moveR :: Tape -> Tape
moveR (Tape l h []) = Tape (h : l) 0 []
moveR (Tape l h (h' : r')) = Tape (h : l) h' r'

updateHead :: (Word8 -> Word8) -> Tape -> Tape
updateHead f (Tape l h r) = Tape l (f h) r

inc :: Tape -> Tape
inc = updateHead (+ 1)

dec :: Tape -> Tape
dec = updateHead (subtract 1)

read :: Tape -> Word8
read = tapeHead

write :: Word8 -> Tape -> Tape
write b = updateHead (const b)
