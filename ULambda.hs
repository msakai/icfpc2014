{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- UuunnLAMBnikukuitainDAyo
module ULambda
  ( Ident
  , Expr (..)
  , TopLevelFuncDefinition (..)
  , cons
  , nil
  , list
  , tuple
  ) where

import Data.Int
import Data.String

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
  deriving (Show)
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
