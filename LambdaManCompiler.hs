{-# LANGUAGE NamedFieldPuns, BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module LambdaManCompiler
  ( Ident
  , Expr (..)
  , TopLevelFuncDefinition
  , compile
  ) where

import Control.Monad.State
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String

import LambdaManCPU

-- ---------------------------------------------------------------
-- language definition
-- ---------------------------------------------------------------

type Ident = String

data Expr
  = EConst Int32
  | ERef Ident
  | ESet Ident Expr
  | EIf Expr Expr Expr
  | EBegin [Expr]
  | ELet [(Ident, Expr)] Expr
  | ELetRec [(Ident, Expr)] Expr
  | EPrimOp1 Ident Expr -- ATOM, CAR, CDR
  | EPrimOp2 Ident Expr Expr -- ADD, SUB, MUL, DIV, CEQ, CGT, CGTE, CONS
  | ECall Expr [Expr]
  | ELambda [Ident] Expr

data TopLevelFuncDefinition
  = TopLevelFuncDefinition
  { funcName    :: Ident
  , funcParams  :: [Ident]
  , funcBody    :: Expr
  }

-- ---------------------------------------------------------------
-- compiler
-- ---------------------------------------------------------------

data Label
  = GenLabel {-# UNPACK #-} !Int
  | NamedLabel String
  deriving (Eq, Ord, Show)

type InstSeq = [GInst Label]

type Env = [Map Ident Int]

lookupVarSlot :: Ident -> Env -> (Int,Int)
lookupVarSlot name = f 0
  where
    f !_ [] = error $ "no such variable " ++ show name ++ " in scope"
    f !n (e:es) =
      case Map.lookup name e of
        Just i -> (n,i)
        _ -> f (n+1) es


newtype M a = M (State (Int, Map Label InstSeq) a) deriving Monad

runM :: M a -> (a, Map Label InstSeq)
runM (M m) =
  case runState m (0, Map.empty) of
    (a, (_, defs)) -> (a, defs)

emit :: InstSeq -> M Label
emit code = M $ do
  (n,defs) <- get
  let l = GenLabel n
  put $ (n+1, Map.insert l code defs)
  return l

emitWithName :: Ident -> InstSeq -> M Label
emitWithName name code = M $ do
  (n,defs) <- get
  let l = NamedLabel name
  when (l `Map.member` defs) $
    error ("label " ++ show name ++ " is already used")
  put $ (n, Map.insert l code defs)
  return l

compile :: Expr -> [TopLevelFuncDefinition] -> [Inst]
compile main funcs = map (mapAddr (addrs Map.! )) code2
  where
    (code,defs) = runM $ compileMain main funcs
    (code2,addrs) = link1 (Map.toAscList defs) (length code) (code, Map.empty)

    link1 :: [(Label,InstSeq)] -> InstAddr -> (InstSeq, Map Label InstAddr) -> (InstSeq, Map Label InstAddr)
    link1 [] _ result = result
    link1 ((label,block):defs) offset (code, table) =
      link1 defs (offset + length block) (code ++ block, Map.insert label offset table)

    mapAddr :: (a -> b) -> GInst a -> GInst b
    mapAddr _ (LDC n)      = LDC n
    mapAddr _ (LD n i)     = LD n i
    mapAddr _ ADD          = ADD       
    mapAddr _ SUB          = SUB       
    mapAddr _ MUL          = MUL       
    mapAddr _ DIV          = DIV       
    mapAddr _ CEQ          = CEQ       
    mapAddr _ CGT          = CGT       
    mapAddr _ CGTE         = CGTE      
    mapAddr _ ATOM         = ATOM      
    mapAddr _ CONS         = CONS
    mapAddr _ CAR          = CAR       
    mapAddr _ CDR          = CDR       
    mapAddr fun (SEL t f)  = SEL (fun t) (fun f)
    mapAddr _ JOIN         = JOIN      
    mapAddr fun (LDF f)    = LDF (fun f)
    mapAddr _ (AP n)       = AP n    
    mapAddr _ RTN          = RTN       
    mapAddr _ (DUM n)      = DUM n
    mapAddr _ (RAP n)      = RAP n
    mapAddr _ STOP         = STOP      
    mapAddr fun (TSEL t f) = TSEL (fun t) (fun f)
    mapAddr _ (TAP n)      = TAP n
    mapAddr _ (TRAP n)     = TRAP n
    mapAddr _ (ST n i)     = ST n i
    mapAddr _ DBUG         = DBUG      
    mapAddr _ BRK          = BRK

compileMain :: Expr -> [TopLevelFuncDefinition] -> M InstSeq
compileMain main funcs = do
  let n = length funcs
      topEnv = [ Map.fromList (zip [funcName func | func <- funcs] [0..]) ]

  ls <- forM funcs $ \TopLevelFuncDefinition{ funcName, funcParams, funcBody } -> do
    let env = Map.fromList (zip funcParams [0..]) : topEnv
    s <- compileExpr env funcBody
    emitWithName funcName (s ++ [RTN])

  s <- compileExpr topEnv main
  lmain <- emitWithName "main" (s ++ [RTN])
  return $ [DUM n] ++ map LDF (ls++[lmain]) ++ [RAP n, RTN]

compileExpr :: Env -> Expr -> M InstSeq
compileExpr env = f
  where
    f (EConst n)  = return [LDC n]
    f (ERef name) =
      case lookupVarSlot name env of
        (n,i) -> return [LD n i]
    f (ESet name e) = do
      s <- f e
      case lookupVarSlot name env of
        (n,i) -> return $ s ++ [ST n i, LDC 0]
    f (EIf c tbody fbody) = do
      s1 <- f c
      lT <- emit =<< f tbody
      lF <- emit =<< f fbody
      return $ s1 ++ [SEL lT lF]
    f (EBegin xs) = do
      ss <- mapM f xs
      return $ intercalate [DBUG] ss -- 単純にスタックから値をPOPする命令がないのでDBUGを使ってる
    f (ELet vs body) = do
      let n = length vs
          envDefn = Map.empty : env
          envBody = Map.fromList (zip (map fst vs) [0..]) : env
      ss <- liftM concat $ mapM (compileExpr envDefn . snd) vs
      l <- do
        s <- compileExpr envBody body
        emit (s++[RTN])
      return $ [DUM n] ++ ss ++ [LDF l, RAP n]
    f (ELetRec vs body) = do
      let n = length vs
          envDefn = envBody
          envBody = Map.fromList (zip (map fst vs) [0..]) : env
      ss <- liftM concat $ mapM (compileExpr envDefn . snd) vs
      l <- do
        s <- compileExpr envBody body
        emit (s++[RTN])
      return $ [DUM n] ++ ss ++ [LDF l, RAP n]
    f (EPrimOp1 op arg) = do
      s <- f arg
      case op of
        "ATOM" -> return $ s ++ [ATOM]
        "CAR"  -> return $ s ++ [CAR]
        "CDR"  -> return $ s ++ [CDR]
        _ -> error $ "unknown operator: " ++ op
    f (EPrimOp2 op arg1 arg2) = do
      s <- liftM concat $ mapM f [arg1,arg2]
      case op of
        "ADD"  -> return $ s ++ [ADD]
        "SUB"  -> return $ s ++ [SUB]
        "MUL"  -> return $ s ++ [MUL]
        "DIV"  -> return $ s ++ [DIV]
        "CEQ"  -> return $ s ++ [CEQ]
        "CGT"  -> return $ s ++ [CGT]
        "CGTE" -> return $ s ++ [CGTE]
        "CONS" -> return $ s ++ [CONS]
        _ -> error $ "unknown operator: " ++ op
    f (ECall func args) = do
      let n = length args
      s1 <- liftM concat $ mapM f args
      s2 <- f func
      return $ s1 ++ s2 ++ [AP n]
    f (ELambda params body) = do
      s <- compileExpr (Map.fromList (zip params [0..]) : env) body
      l <- emit $ s ++ [RTN]
      return $ [LDF l]

-- ---------------------------------------------------------------
-- EDSL combinators
-- ---------------------------------------------------------------

instance Num Expr where
  (+) = EPrimOp2 "ADD"
  (-) = EPrimOp2 "SUB"
  (*) = EPrimOp2 "MUL"
  abs    = error "Expr.abs is not implemented"
  signum = error "Expr.signum is not implemented"
  fromInteger = EConst . fromInteger

instance IsString Expr where
  fromString = ERef

cons :: Expr -> Expr -> Expr
cons = EPrimOp2 "CONS"

nil :: Expr
nil = EConst 0

list :: [Expr] -> Expr
list = foldr cons nil

tuple :: [Expr] -> Expr
tuple [] = error "empty tuple"
tuple [x] = x
tuple (x:xs) = cons x (tuple xs)

-- ---------------------------------------------------------------
-- test case
-- ---------------------------------------------------------------

test :: [Inst]
test = compile e [to,go]
  where
    e  = ECall "go" [1]
    to = TopLevelFuncDefinition
         { funcName   = "to"
         , funcParams = ["n"]
         , funcBody   = ECall "go" ["n" - 1]
         }
    go = TopLevelFuncDefinition
         { funcName   = "go"
         , funcParams = ["n"]
         , funcBody   = ECall "to" ["n" + 1]
         }
{-
[DUM 2,LDF 16,LDF 6,LDF 12,RAP 2,RTN] ++
go(6):[LD 0 0,LDC 1,ADD,LD 1 0,AP 1,RTN] ++
main(12):[LDC 1,LD 0 1,AP 1,RTN] ++
to(16):[LD 0 0,LDC 1,SUB,LD 1 1,AP 1,RTN] 
-}

test2 :: [Inst]
test2 = compile e []
  where
    e  =
      ELetRec
        [ ("to", ELambda ["n"] $ ECall "go" ["n" - 1])
        , ("go", ELambda ["n"] $ ECall "to" ["n" + 1])
        ]
        (ECall "go" [1])
{-
[DUM 0,LDF 20,RAP 0,RTN]
4:[LD 0 0,LDC 1,SUB,LD 1 1,AP 1,RTN]
10:[LD 0 0,LDC 1,ADD,LD 1 0,AP 1,RTN]
16:[LDC 1,LD 0 1,AP 1,RTN]
20:[DUM 2,LDF 4,LDF 10,LDF 16,RAP 2,RTN]
-}

exampleAI :: [Inst]
exampleAI = compile e [step]
  where
    e = tuple [42, "step"]
    step =
      TopLevelFuncDefinition
      { funcName   = "step"
      , funcParams = ["state","world"]
      , funcBody   = tuple ["state" + 1, right]
      }
    [up, right, down, left] = map EConst [0..3]
