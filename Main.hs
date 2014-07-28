{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified Text.ParserCombinators.Parsec as Parsec

import LambdaManCPU hiding (car, cdr)
import ULambda
import ULambdaCompiler
import ULambdaParser

main :: IO ()
main = do
  [fname] <- getArgs
  ret <- Parsec.parseFromFile (Parsec.many (Parsec.try parseSC)) fname
  case ret of
    Left err -> error (show err)
    Right defs -> do
      let prog = compileWithDefinitions defs
                 ["initial-world", "ghost-progs"]
                 (ECall "main" ["initial-world", "ghost-progs"])
      putStr $ showInstSeq prog
