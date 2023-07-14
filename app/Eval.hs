module Eval (execute) where

import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString as B
import System.IO (stdin, stdout)

import Syntax (Instruction (..))
import Tape (Tape)
import qualified Tape

execute :: [Instruction] -> IO ()
execute instrs = evalProgram (execAll instrs) Tape.empty >> pure ()
  where
    evalProgram :: Program a -> Tape -> IO (Maybe a)
    evalProgram = evalStateT . runMaybeT

-- Just a  => executed instruction(s) succesfully
-- Nothing => encountered EOF
type Program a = MaybeT (StateT Tape IO) a

execAll :: [Instruction] -> Program ()
execAll = mapM_ execInstr

execInstr :: Instruction -> Program ()
execInstr instr =
    case instr of
        MoveR -> modify Tape.moveR
        MoveL -> modify Tape.moveL
        Inc -> modify Tape.inc
        Dec -> modify Tape.dec
        Out -> do
            byte <- gets Tape.read
            liftIO $ B.hPut stdout (B.pack [byte])
        In -> do
            bytes <- liftIO $ B.hGet stdin 1
            byte <- if B.null bytes then mzero else pure (B.head bytes)
            modify $ Tape.write byte
        While instrs -> do
            byte <- gets Tape.read
            when (byte /= 0) $ do
                execAll instrs
                execInstr (While instrs)
