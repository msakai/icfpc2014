{-# LANGUAGE OverloadedStrings #-}
-- 壁に当たると右へ曲がるだけのAI
module AI01 where

import qualified Text.ParserCombinators.Parsec as Parsec

import LambdaManCPU hiding (car, cdr)
import ULambda
import ULambdaCompiler
import ULambdaParser

ai02 :: IO [Inst]
ai02 = do
  ret <- Parsec.parseFromFile (Parsec.many (Parsec.try parseSC)) "AI02.scm"
  case ret of
    Left err -> error (show err)
    Right defs -> do
      let prog = compileWithDefinitions defs
                 ["initial-world", "ghost-progs"]
                 (ECall "main" ["initial-world", "ghost-progs"])
      mapM print prog
      return prog
