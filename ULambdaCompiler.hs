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
      compileExpr env True e
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

compileExpr :: Env -> Bool -> Expr -> M InstSeq
compileExpr env = f
  where
    f isTail (EConst n) = return $ [LDC n] ++ [RTN | isTail]
    f isTail (ERef name) =
      case lookupVarSlot name env of
        (n,i) -> return $ [LD n i] ++ [RTN | isTail]
    f isTail (ESet name e) = do
      s <- f False e
      case lookupVarSlot name env of
        (n,i) -> return $ s ++ [ST n i, LDC 0] ++ [RTN | isTail]
    f isTail (EIf c tbody fbody) = do
      s1 <- f False c
      lT <- do
        s <- f isTail tbody
        emit (s ++ [JOIN | not isTail])
      lF <- do
        s <- f isTail fbody
        emit (s ++ [JOIN | not isTail])
      return $ s1 ++ [if isTail then TSEL lT lF else SEL lT lF]
    f isTail (EBegin xs) = do
      ss <- forM (init xs) $ \e -> do
        s <- f False e
        return $ s ++ [DBUG] -- 単純にスタックから値をPOPする命令がないのでDBUGを使ってる
      s <- f isTail (last xs)
      return $ concat (ss ++ [s])
    f isTail (ELet defs body) = f isTail $ ECall (ELambda (map fst defs) body) (map snd defs)
    f isTail (ELetRec defs body) = do
      let n = length defs
          envDefn = envBody
          envBody = Map.fromList (zip (map fst defs) [0..]) : env
      ss <- liftM concat $ mapM (compileExpr envDefn False . snd) defs
      l <- do
        s <- compileExpr envBody isTail body
        emit (s++[RTN | not isTail])
      return $ [DUM n] ++ ss ++ [LDF l, if isTail then TRAP n else RAP n]
    f isTail (ELetStar defs body) = do
      let g ss i ef [] = do
            s <- compileExpr (ef:env) True body
            l <- emit (ss++s)
            let n = length defs
            return $ replicate n (LDC 1234) ++ [LDF l, if isTail then TAP n else AP n]
          g ss i ef ((v,vbody):defs) = do
            s <- compileExpr (ef:env) False vbody
            g (ss++s++[ST 0 i]) (i+1) (Map.insert v i ef) defs
      g [] 0 Map.empty defs
    f isTail (EPrimOp1 op arg) = do
      s <- f False arg
      case op of
        "ATOM" -> return $ s ++ [ATOM] ++ [RTN | isTail]
        "CAR"  -> return $ s ++ [CAR] ++ [RTN | isTail]
        "CDR"  -> return $ s ++ [CDR] ++ [RTN | isTail]
        _ -> error $ "unknown operator: " ++ op
    f isTail (EPrimOp2 op arg1 arg2) = do
      s <- liftM concat $ mapM (f False) [arg1,arg2]
      case op of
        "ADD"  -> return $ s ++ [ADD] ++ [RTN | isTail]
        "SUB"  -> return $ s ++ [SUB] ++ [RTN | isTail]
        "MUL"  -> return $ s ++ [MUL] ++ [RTN | isTail]
        "DIV"  -> return $ s ++ [DIV] ++ [RTN | isTail]
        "CEQ"  -> return $ s ++ [CEQ] ++ [RTN | isTail]
        "CGT"  -> return $ s ++ [CGT] ++ [RTN | isTail]
        "CGTE" -> return $ s ++ [CGTE] ++ [RTN | isTail]
        "CONS" -> return $ s ++ [CONS] ++ [RTN | isTail]
        _ -> error $ "unknown operator: " ++ op
    f isTail (ECall func args) = do
      let n = length args
      s1 <- liftM concat $ mapM (f False) args
      s2 <- f False func
      return $ s1 ++ s2 ++ [if isTail then TAP n else AP n]
    f isTail (ELambda params body) = do
      l <- emit =<< compileExpr (Map.fromList (zip params [0..]) : env) True body
      return $ [LDF l] ++ [RTN | isTail]

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

-- => 4
test_letstar :: [Inst]
test_letstar = compile [] $
  ELetStar
    [ ("a", 1)
    , ("b", "a" + 2)
    , ("c", "a" + "b")
    ]
    "c"

-- => 4
test_letstar2 :: [Inst]
test_letstar2 = compile [] $
  ELet [ ("a", 100) ] $ 
    ELetStar
      [ ("a", 1)
      , ("b", "a" + 2)
      , ("c", "a" + "b")
      ]
      "c"

-- *** Exception: no such variable "b" in scope
test_letstar3 :: [Inst]
test_letstar3 = compile [] $
  ELetStar
    [ ("a", 1)
    , ("b", "a" + "b")
    ]
    "b"

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
