module GHostCPU where

import Data.Array.IArray
import Data.Array.IO
import Data.IORef
import Data.Word

-- 'A' to 'H' or 'P' for PC
type Reg = Char

data Arg
  = ArgReg {-# UNPACK #-} !Reg
  | ArgInd {-# UNPACK #-} !Reg
  | ArgConst {-# UNPACK #-} !Word8
  | ArgLoc {-# UNPACK #-} !Word8
  deriving (Eq, Ord, Show)

type Dest = Arg
type Src = Arg
type X = Arg
type Y = Arg
type Targ = Word8

data Inst
  = MOV Dest Src
    -- ^ Copy the value of the src argument into the dest argument. dest may not be a constant.
  | INC Dest
    -- ^ Add 1 to the value of dest and store the result in dest. dest may not be a constant or the register PC.
  | DEC Dest
    -- ^ Subtract 1 from the value of dest and store the result in dest. dest may not be a constant or the register PC.
  | ADD Dest Src
    -- ^Add the value of src to the value of dest and store the result in dest. dest may not be a constant or the register PC.
  | SUB Dest Src
    -- ^ Subtract the value of src from the value of dest and store the result in dest. dest may not be a constant or the register PC.
  | MUL Dest Src
    -- ^ Multiply the value of src by the value of dest and store the result in dest. dest may not be a constant or the register PC.
  | DIV Dest Src
    -- ^ Compute the integer quotient of dest by the value of src, and store the result in dest. dest may not be a constant or the register PC. Results in an error if the value of src is 0.
  | AND Dest Src
    -- ^ Bitwise AND the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.
  | OR Dest Src
    -- ^ Bitwise OR the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.
  | XOR Dest Src
    -- ^ Bitwise XOR the value of dest and the value of src, storing the result in dest. dest may not be a constant or the register PC.
  | JLT Targ X Y
    -- ^ Compare the value of x with the value of y. If the value of x is less than the value of y, set the PC to the constant value targ.
  | JEQ Targ X Y
    -- ^ Compare the value of x with the value of y. If the value of x is equal to the value of y, set the PC to the constant value targ.
  | JGT Targ X Y
    -- ^ Compare the value of x with the value of y. If the value of x is greater than the value of y, set the PC to the constant value targ.
  | INT Word8
    -- ^ Invoke the interrupt service i (see Interrupt Reference).
  | HLT
    -- ^ Halt execution of the GHC.


data Machine
  = Machine
  { mPC :: IORef Word8
  , mRegs :: IOArray Reg Word8
  , mDataMemory :: IOArray Word8 Word8
  , mCodeMemory :: Array Word8 Inst
  }

newMachine :: [Inst] -> IO Machine
newMachine prog = do
  pc <- newIORef 0
  regs <- newArray ('A','H') 0
  mem <- newArray (0,0xff) 0
  let code = array (0,0xff) $ zip [0..] (take 256 prog)
  return $ Machine pc regs mem code
