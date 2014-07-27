{-# LANGUAGE OverloadedStrings #-}
-- 壁に当たると右へ曲がるだけのAI
module AI01 where

import Text.ParserCombinators.Parsec

import LambdaManCPU hiding (car, cdr)
import ULambda
import ULambdaCompiler
import ULambdaParser

ai01 :: [Inst]
ai01 = compileWithDefinitions [step, defNth, defLookupMap, defMove, defTurnClockwise] ["initial-world", "ghost-progs"] e
  where
    e = tuple [42, "step"]

    step =
      TopLevelFuncDefinition
      { funcName   = "step"
      , funcParams = ["state","world"]
      , funcBody   = 
          ELet
            [ ("current-map", tproj 4 0 "world")
            , ("lambda-man", tproj 4 1 "world")
            , ("ghosts", tproj 4 2 "world")
            , ("fruits", tproj 4 3 "world")
            ] $
            ELet 
              [ ("lambda-man-pos", tproj 5 1 "lambda-man")
              , ("lambda-man-dir", tproj 5 2 "lambda-man")
              ] $
              ELet [ ("lambda-man-next-pos", move "lambda-man-pos" "lambda-man-dir") ] $
              ELet [ ("cell", lookupMap "current-map" "lambda-man-next-pos") ] $
                EIf ("cell" .==. wall)
                    (tuple ["state" + 1, turnClockWise "lambda-man-dir"])
                    (tuple ["state", "lambda-man-dir"])
      }

ai01' :: [Inst]
ai01' = compileWithDefinitions [step, defNth, defLookupMap, defMove, defTurnClockwise] ["initial-world", "ghost-progs"] e
  where
    e = tuple [42, "step"]

    step =
      TopLevelFuncDefinition
      { funcName   = "step"
      , funcParams = ["state","world"]
      , funcBody   = 
          ELetStar
            [ ("current-map", tproj 4 0 "world")
            , ("lambda-man", tproj 4 1 "world")
            , ("ghosts", tproj 4 2 "world")
            , ("fruits", tproj 4 3 "world")
            , ("lambda-man-pos", tproj 5 1 "lambda-man")
            , ("lambda-man-dir", tproj 5 2 "lambda-man")
            , ("lambda-man-next-pos", move "lambda-man-pos" "lambda-man-dir")
            , ("cell", lookupMap "current-map" "lambda-man-next-pos")
            ] $
            EIf ("cell" .==. wall)
              (tuple ["state" + 1, turnClockWise "lambda-man-dir"])
              (tuple ["state", "lambda-man-dir"])
      }

wall = 0
[up, right, down, left] = map EConst [0..3]

nth :: Expr -> Expr -> Expr
nth xs i = ECall "nth" [xs, i]

defNth :: TopLevelFuncDefinition
{-
defNth =
  TopLevelFuncDefinition
  { funcName   = "nth"
  , funcParams = ["xs","i"]
  , funcBody   = EIf ("i" .==. 0) (car "xs") (nth (cdr "xs") ("i" - 1))
  }
-}
Right defNth = parse parseSC "" "(define (nth xs i) (if (= i 0) (car xs) (nth (cdr xs) (- i 1))))"

lookupMap :: Expr -> Expr -> Expr
lookupMap map pos = ECall "lookup-map" [map, pos]

defLookupMap :: TopLevelFuncDefinition
defLookupMap =
  TopLevelFuncDefinition
  { funcName   = "lookup-map"
  , funcParams = ["map","pos"]
  , funcBody   = nth (nth "map" y) x
  }
  where
    x = tproj 2 0 "pos"
    y = tproj 2 1 "pos"

move :: Expr -> Expr -> Expr
move pos dir = ECall "move" [pos, dir]

defMove :: TopLevelFuncDefinition
defMove = 
  TopLevelFuncDefinition
  { funcName   = "move"
  , funcParams = ["pos","dir"]
  , funcBody   = 
      EIf ("dir" .==. up)    (tuple [tproj 2 0 "pos", tproj 2 1 "pos" - 1]) $
      EIf ("dir" .==. right) (tuple [tproj 2 0 "pos" + 1, tproj 2 1 "pos"]) $
      EIf ("dir" .==. down)  (tuple [tproj 2 0 "pos", tproj 2 1 "pos" + 1]) $
      {- dir==left -} tuple [tproj 2 0 "pos" - 1, tproj 2 1 "pos"]
  }
  where
    x = tproj 2 0 "pos"
    y = tproj 2 1 "pos"

turnClockWise :: Expr -> Expr
turnClockWise dir = ECall "turn-clockwise" [dir]

defTurnClockwise :: TopLevelFuncDefinition
defTurnClockwise = 
  TopLevelFuncDefinition
  { funcName   = "turn-clockwise"
  , funcParams = ["dir"]
  , funcBody   = 
      EIf ("dir" .==. up)    right $
      EIf ("dir" .==. right) down $
      EIf ("dir" .==. down)  left $
      {- dir == left -} up
  }

{-
> putStr $ showInstSeq ai01
DUM 5
LDF 53
LDF 81
LDF 86
LDF 138
LDF 161
LDF 166
RAP 5
RTN
LD 4 0
LDC 1
ADD
LD 2 1
LD 5 4
AP 1
CONS
JOIN
LD 4 0
LD 2 1
CONS
JOIN
LD 0 0
LDC 0
CEQ
SEL 9 17
RTN
DUM 1
LD 3 0
LD 1 0
LD 5 2
AP 2
LDF 21
RAP 1
RTN
DUM 1
LD 1 0
LD 1 1
LD 4 3
AP 2
LDF 26
RAP 1
RTN
DUM 2
LD 1 1
CDR
CAR
LD 1 1
CDR
CDR
CAR
LDF 34
RAP 2
RTN
DUM 4
LD 1 1
CAR
LD 1 1
CDR
CAR
LD 1 1
CDR
CDR
CAR
LD 1 1
CDR
CDR
CDR
LDF 42
RAP 4
RTN
LD 0 0
CAR
JOIN
LD 0 0
CDR
LD 0 1
LDC 1
SUB
LD 1 1
AP 2
JOIN
LD 0 1
LDC 0
CEQ
SEL 70 73
RTN
LD 0 0
LD 0 1
CDR
LD 1 1
AP 2
LD 0 1
CAR
LD 1 1
AP 2
RTN
LD 0 0
CAR
LD 0 0
CDR
LDC 1
SUB
CONS
JOIN
LD 0 0
CAR
LDC 1
ADD
LD 0 0
CDR
CONS
JOIN
LD 0 0
CAR
LD 0 0
CDR
LDC 1
ADD
CONS
JOIN
LD 0 0
CAR
LDC 1
SUB
LD 0 0
CDR
CONS
JOIN
LD 0 1
LDC 2
CEQ
SEL 112 120
JOIN
LD 0 1
LDC 1
CEQ
SEL 104 128
JOIN
LD 0 1
LDC 0
CEQ
SEL 96 133
RTN
LDC 1
JOIN
LDC 2
JOIN
LDC 3
JOIN
LDC 0
JOIN
LD 0 0
LDC 2
CEQ
SEL 147 149
JOIN
LD 0 0
LDC 1
CEQ
SEL 145 151
JOIN
LD 0 0
LDC 0
CEQ
SEL 143 156
RTN
LDC 42
LD 0 0
CONS
RTN
-}
