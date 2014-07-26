{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module LambdaManCPU where

import Control.Monad
import Data.Array.IArray
import Data.Array.IO
import Data.Int
import Data.IORef
import Data.Maybe

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
  deriving (Eq, Ord, Show)

data Value
  = VInt {-# UNPACK #-} !Int32
  | VCons{ car :: IORef Value, cdr :: IORef Value }
  | VClosure {-# UNPACK #-} !InstAddr Frame
  deriving (Eq)

data Frame
  = Frame
  { frameParent :: Maybe Frame
  , frameValues :: IOArray Int Value
  }
  deriving (Eq)

data ContFrame
  = ContJoin InstAddr -- ^ TAG_JOIN
  | ContRet InstAddr  -- ^ TAG_RET
  | ContFP Frame -- ContRetと融合した方が良いか
  | ContStop -- ^ TAG_STOP

data Machine
  = Machine
  { mC :: IORef Int -- ^ %c: control register (program counter / instruction pointer)
  , mS :: IORef [Value] -- ^ %s: data stack register
  , mD :: IORef [ContFrame] -- ^ %d: control stack register
  , mE :: IORef Frame -- ^ %e: environment frame register
  , mProg :: Array Int Inst
  }

newMachine :: [Inst] -> IO Machine
newMachine prog = do
  c <- newIORef 0
  s <- newIORef []
  d <- newIORef [ContStop]
  e <- newIORef (error "no frame")
  return $ Machine{ mC = c, mS = s, mD = d, mE = e, mProg = array (0, length prog -1) (zip [0..] prog) }

step :: Machine -> IO Bool
step Machine{ mC, mS, mD, mE, mProg } = do
  let incC = modifyIORef' mC (+1)

  let popS = do
        xxs <- readIORef mS
        case xxs of
          x:xs -> writeIORef mS xs >> return x
          _ -> error "popS from empty stack"
      pushS v = modifyIORef mS (v:)

  let popD = do
        xxs <- readIORef mD
        case xxs of
          x:xs -> writeIORef mD xs >> return x
          _ -> error "popS from empty stack"
      pushD v = modifyIORef mD (v:)

  pc <- readIORef mC
  case mProg ! pc of
    LDC n -> do -- load constant
      pushS (VInt n)
      incC
      return True

    LD n i -> do -- load from environment
      let f 0 fp = return fp
          f m fp = f (m-1) (fromJust (frameParent fp))
      fp <- f n =<< readIORef mE
      pushS =<< readArray (frameValues fp) i
      incC
      return True

    ADD -> do -- integer addition
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (x+y)
      incC
      return True

    SUB -> do -- integer subtraction
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (x-y)
      incC
      return True

    MUL -> do -- integer multiplication
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (x*y)
      incC
      return True

    DIV -> do -- integer division
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (x `div` y) -- TODO: 負数の場合の定義は?
      incC
      return True

    CEQ -> do -- compare equal
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (if x==y then 1 else 0)
      incC
      return True

    CGT -> do -- compare greater than
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (if x>y then 1 else 0)
      incC
      return True

    CGTE -> do -- compare greater than or equal
      VInt y <- popS
      VInt x <- popS
      pushS $ VInt (if x>=y then 1 else 0)
      incC
      return True

    ATOM -> do -- test if value is an integer
      x <- popS
      let y = case x of
                VInt _ -> 1
                _ -> 0
      pushS $ VInt y
      incC
      return True

    CONS -> do -- allocate a CONS cell
      y <- popS
      x <- popS
      carRef <- newIORef x
      cdrRef <- newIORef y
      pushS $ VCons carRef cdrRef
      incC
      return True

    CAR -> do -- extract first element from CONS cell
      VCons car _ <- popS
      y <- readIORef car
      pushS $ y
      incC
      return True

    CDR -> do -- extract second element from CONS cell
      VCons _ cdr <- popS
      y <- readIORef cdr
      pushS $ y
      incC
      return True

    SEL t f -> do -- conditional branch
      VInt x <- popS
      pushD $ ContJoin (pc+1)
      if x==0 then
        writeIORef mC f
      else
        writeIORef mC t
      return True

    JOIN -> do -- return from branch
      ContJoin x <- popD
      writeIORef mC x
      return True

    LDF f -> do -- load function
      e <- readIORef mE
      pushS $ VClosure f e
      incC
      return True

    AP n -> do -- call function
      VClosure f e <- popS
      a <- newArray (0,n-1) undefined
      let g (-1) = return ()
          g i = do
            y <- popS
            writeArray a i y
            g (i-1)
      g (n-1)
      let fp = Frame{ frameParent = Just e, frameValues = a }
      pushD (ContFP e)
      pushD (ContRet (pc+1))
      writeIORef mE fp
      writeIORef mC f
      return True

    RTN -> do -- return from function call
      cont <- popD
      case cont of
        ContStop -> return False -- MACHINE_STOP
        ContRet x -> do
          ContFP y <- popD
          writeIORef mE y
          writeIORef mC x
          return True
        _ -> error "RTN: FAULT(CONTROL_MISMATCH)"

    DUM n -> do -- create empty environment frame
      e <- readIORef mE
      a <- newArray (0,n-1) undefined
      let fp = Frame{ frameParent = Just e, frameValues = a }
      writeIORef mE fp
      incC
      return True

    RAP n -> do -- recursive environment call function
      VClosure f fp <- popS
      size <- liftM rangeSize $ getBounds $ frameValues fp
      when (size /= n) $ error "FAULT(FRAME_MISMATCH)"
      let g (-1) = return ()
          g i = do
            y <- popS
            writeArray (frameValues fp) i y
            g (i-1)
      g (n-1)
      let Just fpp = frameParent fp
      pushD (ContFP fpp)
      pushD (ContRet (pc+1))
      writeIORef mE fp
      writeIORef mC f
      return True

    STOP -> do -- terminate co-processor execution
      return False -- MACHINE_STOP

    TSEL t f -> do -- tail-call conditional branch
      VInt x <- popS
      if x==0 then
        writeIORef mC f
      else
        writeIORef mC t
      return True

    TAP n -> do -- tail-call function
      VClosure f e <- popS
      a <- newArray (0,n-1) undefined
      let g (-1) = return ()
          g i = do
            y <- popS
            writeArray a i y
            g (i-1)
      g (n-1)
      let fp = Frame{ frameParent = Just e, frameValues = a }
      writeIORef mE fp
      writeIORef mC f
      return True

    TRAP n -> do -- recursive environment tail-call function
      VClosure f fp <- popS
      size <- liftM rangeSize $ getBounds $ frameValues fp
      when (size /= n) $ error "FAULT(FRAME_MISMATCH)"
      let g (-1) = return ()
          g i = do
            y <- popS
            writeArray (frameValues fp) i y
            g (i-1)
      g (n-1)
      writeIORef mE fp
      writeIORef mC f
      return True

    ST n i -> do -- store to environment
      let f 0 fp = return fp
          f m fp = f (m-1) (fromJust (frameParent fp))
      fp <- f n =<< readIORef mE
      v <- popS
      writeArray (frameValues fp) i v
      incC
      return True

    DBUG -> do -- printf debugging
      _ <- popS
      incC
      return True

    BRK -> do -- breakpoint debugging
      incC
      return True

run :: Machine -> IO ()
run m = do
  b <- step m
  if b then
    run m
  else
    return ()

run1sec :: Machine -> IO ()
run1sec m = go 0
  where
    go :: Int -> IO ()
    go cnt
      | cnt > 3072 * 10^(3::Int) = error "catastrophic failure"
      | otherwise = do
          b <- step m
          if b then
            go (cnt+1)
          else
            return ()
