{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- UuunnLAMBnikukuitainDAyo
module ULambda
  ( Ident
  , Expr (..)
  , TopLevelFuncDefinition (..)

  , (.==.)
  , (.>.)
  , (.>=.)
  , (.<.)
  , (.<=.)

  , cons
  , nil
  , car
  , cdr
  , list
  , null'

  , tuple
  , tproj

  , true
  , false
  , not'
  , and'
  , or'

  , constTable
  , op1Table
  , op2Table
  , opNTable
  ) where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Text.Printf

infix 4 .==.
infix 4 .>.
infix 4 .>=.
infix 4 .<.
infix 4 .<=.

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
  | ELetStar [(Ident, Expr)] Expr
  | EPrimOp1 Ident Expr -- ATOM, CAR, CDR
  | EPrimOp2 Ident Expr Expr -- ADD, SUB, MUL, DIV, CEQ, CGT, CGTE, CONS
  | ECall Expr [Expr]
  | ELambda [Ident] Expr
  deriving (Show)

data TopLevelFuncDefinition
  = TopLevelFuncDefinition
  { funcName    :: Ident
  , funcParams  :: [Ident]
  , funcBody    :: Expr
  }
  deriving (Show)

-- ---------------------------------------------------------------
-- AST manipulation
-- ---------------------------------------------------------------

class Vars a where
  fvs :: a -> Set Ident

instance Vars a => Vars [a] where
  fvs = Set.unions . map fvs

instance Vars Expr where
  fvs (EConst _)  = Set.empty
  fvs (ERef v)    = Set.singleton v
  fvs (ESet v e)  = Set.insert v (fvs e)
  fvs (EIf c t e) = fvs [c,t,e]
  fvs (EBegin xs) = fvs xs
  fvs (ELet bs body) =
    Set.union (Set.unions [fvs def | (_,def) <- bs])
              (fvs body `Set.difference` Set.fromList [v | (v,_) <- bs])
  fvs (ELetRec bs body) =
    Set.unions (fvs body : [fvs def | (_,def) <- bs]) `Set.difference` Set.fromList [v | (v,_) <- bs]
  fvs (ELetStar bs body) = f bs
    where
      f [] = fvs body
      f ((v,def):bs) = Set.delete v (f bs) `Set.union` fvs def
  fvs (EPrimOp1 _ arg) = fvs arg
  fvs (EPrimOp2 _ arg1 arg2) = fvs [arg1, arg2]
  fvs (ECall fun args) = fvs (fun : args)
  fvs (ELambda params body) = fvs body `Set.difference` Set.fromList params

-- vsに含まれない変数名を生成する
gensym :: Set Ident -> Ident
gensym vs = head $ [v | n <- [(1::Int)..], let v = "__v" ++ show n, v `Set.notMember` vs]

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

(.==.) :: Expr -> Expr -> Expr
a .==. b = EPrimOp2 "CEQ" a b

(.>.) :: Expr -> Expr -> Expr
a .>. b = EPrimOp2 "CGT" a b

(.>=.) :: Expr -> Expr -> Expr
a .>=. b = EPrimOp2 "CGTE" a b

(.<.) :: Expr -> Expr -> Expr
(.<.) = flip (.>.)

(.<=.) :: Expr -> Expr -> Expr
(.<=.) = flip (.>=.)

instance IsString Expr where
  fromString = ERef

cons :: Expr -> Expr -> Expr
cons = EPrimOp2 "CONS"

nil :: Expr
nil = EConst 0

car :: Expr -> Expr
car = EPrimOp1 "CAR"

cdr :: Expr -> Expr
cdr = EPrimOp1 "CDR"

list :: [Expr] -> Expr
list = foldr cons nil

-- nilは0で表されていることに注意
null' :: Expr -> Expr
null' xs = EPrimOp1 "ATOM" xs

tuple :: [Expr] -> Expr
tuple [] = error "empty tuple"
tuple [x] = x
tuple (x:xs) = cons x (tuple xs)

-- tproj project i-th component of n-tuple
-- (x_0,…,x_i,…,x_{n-1}) ↦ x_i
tproj :: Int -> Int -> Expr -> Expr
tproj n i e | i>=n = error $ printf "tproj %d %d: should not happen" n i
tproj 1 0 e = e -- (x) = x
tproj n 0 e = car e
tproj n i e = tproj (n-1) (i-1) (cdr e)

true :: Expr
true = 1

false :: Expr
false = 0

not' :: Expr -> Expr
not' e = e .==. 0

and' :: Expr -> Expr -> Expr
and' a b = EIf a b false

or' :: Expr -> Expr -> Expr
or' a b = EIf a true b


constTable :: Map Ident Expr
constTable =
  Map.fromList
  [ ("true", true)
  , ("false", false)
  , ("nil", nil)
  ]

op1Table :: Map Ident (Expr -> Expr)
op1Table =
  Map.fromList
  [ ("car", car)
  , ("cdr", cdr)
  , ("null", null')

  , ("not", not')
  ]

op2Table :: Map Ident (Expr -> Expr -> Expr)
op2Table =
  Map.fromList
  [ (":", cons)
  , ("+", (+))
  , ("-", (-))
  , ("*", (*))
  , ("/", EPrimOp2 "DIV")

  , ("=", (.==.))
  , (">", (.>.))
  , ("<", (.<.))
  , (">=", (.>=.))
  , ("<=", (.<=.))

  , ("or", or')
  , ("and", and')
  ]

opNTable :: Map Ident ([Expr] -> Expr)
opNTable =
  Map.fromList
  [ ("list", list)
  , ("tuple", tuple)
  ]
