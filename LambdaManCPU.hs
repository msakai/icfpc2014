module LambdaManCPU where

import Data.Array
import Data.Int
import Data.IORef

-- | absolute instruction addresses
type InstAddr = Int -- ?

type FrameSize = Int

type NArg = Int

data Inst
  = LDC Int32 -- ^ load constant
  | LD Int Int -- ^ load from environment
  | ADD -- ^ integer addition
  | SUB -- ^ integer subtraction
  | MUL -- ^ integer multiplication
  | DIV -- ^ integer division
  | CEQ -- ^ compare equal
  | CGT -- ^ compare greater than
  | CGTE -- ^ compare greater than or equal
  | ATOM -- ^ test if value is an integer
  | CONS -- ^ allocate a CONS cell
  | CAR -- ^ extract first element from CONS cell
  | CDR -- ^ extract second element from CONS cell
  | SEL InstAddr InstAddr -- ^ conditional branch
  | JOIN -- ^ return from branch
  | LDF InstAddr -- ^ load function
  | AP NArg -- ^ call function
  | RTN -- ^ return from function call
  | DUM FrameSize -- ^ create empty environment frame
  | RAP NArg -- ^ recursive environment call function
  | STOP -- ^ terminate co-processor execution
  | TSEL InstAddr InstAddr -- ^ tail-call conditional branch
  | TAP NArg -- ^ tail-call function
  | TRAP NArg -- ^ recursive environment tail-call function
  | ST Int Int -- ^ store to environment
  | DBUG -- ^ printf debugging
  | BRK -- ^ breakpoint debugging


data Value
  = VInt Int32
  | VPtr HeapObj

data HeapObj
  = HCons{ car :: IORef Value, cdr :: IORef Value }
  | HClosure InstAddr Frame

data Frame
  = Frame
  { fparent  :: Maybe Frame
  , fentries :: Array Int Value
  }
