module GHostCPU where

import Data.Bits
import Data.Array.IArray
import Data.Array.IO
import Data.IORef
import Data.Word

-- 'A' to 'H' or 'P' for PC
type Reg = Char

data Arg
  = ArgPC
  | ArgReg {-# UNPACK #-} !Reg
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

type InstructionCounter = IORef Word16 
type ProgramCounter     = IORef Word8
type Registers   = IOArray Reg Word8
type DataMemory  = IOArray Word8 Word8
type CodeMemory  = Array Word8 Inst
type InterruptHandler = Registers -> IO ()

data Machine
  = Machine
  { mIC   :: InstructionCounter
  , mPC   :: ProgramCounter
  , mRegs :: Registers
  , mDataMemory :: DataMemory
  , mCodeMemory :: CodeMemory
  , mIntHandler :: Array Word8 InterruptHandler
  }

data Reason 
   = ResHLT -- HLT
   | ResErr -- Error
   | ResLim -- Instruction Limit
   | ResNon -- Not terminated
   deriving (Show)

newMachine :: [Inst] -> IO Machine
newMachine prog = do
  ic <- newIORef 0
  pc <- newIORef 0
  regs <- newArray ('A','H') 0
  mem  <- newArray (0,0xff) 0
  let code = array (0,0xff) $ zip [0..] (take 256 prog)
  let hdl  = array (0,0xff) $ zip [0..] (replicate 9 undefined)
  return $ Machine ic pc regs mem code hdl

runMachine :: Machine -> IO (Machine, Reason)
runMachine m = do 
  { mr@(m',r) <- instrCycle m
  ; case r of
      ResNon -> runMachine m'
      _      -> return mr
  }

instrCycle :: Machine -> IO (Machine,Reason)
instrCycle m@(Machine _ pc _ _ code _) = do
  { c   <- readIORef pc
  ; case code ! c of
      mov@(MOV _ _) -> execMov m mov
      inc@(INC _)   -> execInc m inc
      dec@(DEC _)   -> execDec m dec
      add@(ADD _ _) -> execAdd m add
      sub@(SUB _ _) -> execSub m sub
      mul@(MUL _ _) -> execMul m mul
      div@(DIV _ _) -> execDiv m div
      and@(AND _ _) -> execAnd m and
      or@(OR _ _)   -> execOr  m or
      xor@(XOR _ _) -> execXor m xor
      jlt@(JLT _ _ _) -> execJlt m jlt
      jeq@(JEQ _ _ _) -> execJeq m jeq
      jgt@(JGT _ _ _) -> execJgt m jgt
      int@(INT _)     -> execInt m int
      HLT           -> execHlt m HLT
  }

execMov m@(Machine ic pc regs mem code hdl) (MOV d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> writeIORef pc v >> return (m,reason)
      ArgReg   r -> writeArray regs r v >> return (m,reason)
      ArgInd   r -> readArray regs r >>= flip (writeArray mem) v >> return (m,reason)
      ArgLoc   w -> writeArray mem w v >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execMov _ _ = error "Not MOV"

execInc m@(Machine ic pc regs mem code hdl) (INC d) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray succ regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray succ mem >> return (m,reason)
      ArgLoc   w -> modifyArray (1+) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execInc _ _ = error "Not INC"

execDec m@(Machine ic pc regs mem code hdl) (DEC d) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray pred regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray pred mem >> return (m,reason)
      ArgLoc   w -> modifyArray pred mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execDec _ _ = error "Not DEC"

execAdd m@(Machine ic pc regs mem code hdl) (ADD d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (v+) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (v+) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (v+) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execAdd _ _ = error "Not ADD"

execSub m@(Machine ic pc regs mem code hdl) (SUB d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (subtract v) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (subtract v) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (subtract v) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execSub _ _ = error "Not SUB"

execMul m@(Machine ic pc regs mem code hdl) (MUL d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (v*) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (v*) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (v*) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execMul _ _ = error "Not MUL"

execDiv m@(Machine ic pc regs mem code hdl) (DIV d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> if v==0 then return (m,ResErr) else modifyArray (`div` v) regs r >> return (m,reason)
      ArgInd   r -> if v==0 then return (m,ResErr) else readArray regs r >>= modifyArray (`div` v) mem >> return (m,reason)
      ArgLoc   w -> if v==0 then return (m,ResErr) else modifyArray (`div` v) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execDiv _ _ = error "Not DIV"

execAnd m@(Machine ic pc regs mem code hdl) (AND d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (v .&.) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (v .&.) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (v .&.) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execAnd _ _ = error "Not ADD"

execOr m@(Machine ic pc regs mem code hdl) (OR d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (v .|.) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (v .|.) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (v .|.) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execOr _ _ = error "Not OR"

execXor m@(Machine ic pc regs mem code hdl) (XOR d s) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; v  <- getValue pc regs mem s
  ; case d of
      ArgPC      -> return (m,ResErr)
      ArgReg   r -> modifyArray (v `xor`) regs r >> return (m,reason)
      ArgInd   r -> readArray regs r >>= modifyArray (v `xor`) mem >> return (m,reason)
      ArgLoc   w -> modifyArray (v `xor`) mem w >> return (m,reason)
      ArgConst _ -> return (m,ResErr)
  }
execXor _ _ = error "Not XOR"

execJlt m@(Machine ic pc regs mem code hdl) (JLT targ x y) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; vx  <- getValue pc regs mem x
  ; vy  <- getValue pc regs mem y
  ; if vx < vy then modifyIORef pc (const targ) >> return (m,ResNon)
               else return (m,ResNon)
  }
execJlt _ _ = error "Not JLT"

execJeq m@(Machine ic pc regs mem code hdl) (JEQ targ x y) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; vx  <- getValue pc regs mem x
  ; vy  <- getValue pc regs mem y
  ; if vx == vy then modifyIORef pc (const targ) >> return (m,ResNon)
                else return (m,ResNon)
  }
execJEQ _ _ = error "Not JEQ"

execJgt m@(Machine ic pc regs mem code hdl) (JGT targ x y) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; vx  <- getValue pc regs mem x
  ; vy  <- getValue pc regs mem y
  ; if vx > vy then modifyIORef pc (const targ) >> return (m,ResNon)
               else return (m,ResNon)
  }
execJgt _ _ = error "Not JGT"

execInt m@(Machine ic pc regs mem code hdl) (INT i) = do
  { modifyIORef pc succ
  ; modifyIORef ic succ
  ; c  <- readIORef ic
  ; let reason = if c == 1024 then ResLim else ResNon
  ; (hdl ! i) regs
  ; return (m,ResNon)
  }
execInt _ _ = error "Not Int"

execHlt m@(Machine ic _ _ _ _ _) HLT = modifyIORef' ic succ >> return (m,ResHLT)
execHlt _ _ = error "not HLT"

getValue :: IORef Word8 -> IOArray Reg Word8 -> IOArray Word8 Word8 -> Arg -> IO Word8
getValue pc _    _  ArgPC        = readIORef pc
getValue _ regs _   (ArgReg r)   = readArray regs r
getValue _ regs mem (ArgInd r)   = readArray regs r >>= readArray mem
getValue _ _    _   (ArgConst w) = return w
getValue _ _    mem (ArgLoc w)   = readArray mem w

modifyArray :: (MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m ()
modifyArray f a i = readArray a i >>= writeArray a i . f

sampleProg = undefined
