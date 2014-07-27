{-# LANGUAGE NamedFieldPuns, BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module ULambdaCompiler
  ( compile
  , compileWithDefinitions
  ) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String

import LambdaManCPU
import ULambda

-- ---------------------------------------------------------------
-- test case
-- ---------------------------------------------------------------

compile :: [Ident] -> Expr -> [Inst]
compile params e = map (mapAddr (addrs Map.! )) code2
  where
    (code,defs) = runM $ do
      let env = [ Map.fromList (zip [v | v <- params] [0..]) ]
      s <- compileExpr env e
      return $ s ++ [RTN]
    (code2,addrs) = link1 (Map.toAscList defs) (length code) (code, Map.empty)

    link1 :: [(Label,InstSeq)] -> InstAddr -> (InstSeq, Map Label InstAddr) -> (InstSeq, Map Label InstAddr)
    link1 [] _ result = result
    link1 ((label,block):defs) offset (code, table) =
      link1 defs (offset + length block) (code ++ block, Map.insert label offset table)

compileWithDefinitions :: [TopLevelFuncDefinition] -> [Ident] -> Expr -> [Inst]
compileWithDefinitions funcs params main  = compile params e
  where
    e = ELetRec [(funcName func, ELambda [p | p <- funcParams func] (funcBody func)) | func <- funcs] main

-- ---------------------------------------------------------------
-- implementation
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
      lT <- do
        s <- f tbody
        emit (s ++ [JOIN])
      lF <- do
        s <- f fbody
        emit (s ++ [JOIN])
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

-- ---------------------------------------------------------------
-- test case
-- ---------------------------------------------------------------

test :: [Inst]
test = compileWithDefinitions [to,go] [] e
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

test2 :: [Inst]
test2 = compile [] $
  ELetRec
    [ ("to", ELambda ["n"] $ ECall "go" ["n" - 1])
    , ("go", ELambda ["n"] $ ECall "to" ["n" + 1])
    ]
    (ECall "go" [1])

-- try it with <http://icfpcontest.org/lman.html>
test_fact :: [Inst]
test_fact = compile [] $
  ELetRec
    [ ("fact", ELambda ["n"] $ EIf ("n" .==. 0) 1 ("n" * (ECall "fact" ["n" - 1]))) ]
    (ECall "fact" [4])

exampleAI :: [Inst]
exampleAI = compileWithDefinitions [step] [] e
  where
    e = tuple [42, "step"]
    step =
      TopLevelFuncDefinition
      { funcName   = "step"
      , funcParams = ["state","world"]
      , funcBody   = tuple ["state" + 1, right]
      }
    [up, right, down, left] = map EConst [0..3]
