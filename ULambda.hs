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
  , tuple
  , tproj
  ) where

import Data.Int
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
cdr = EPrimOp1 "CAR"

list :: [Expr] -> Expr
list = foldr cons nil

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
