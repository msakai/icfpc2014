{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import LambdaManCPU hiding (car, cdr)
import ULambda
import ULambdaCompiler
import ULambdaParser

main :: IO ()
main = do
  [fname] <- getArgs
  ret <- parseULambdaFile fname
  case ret of
    Left err -> error (show err)
    Right defs -> do
      let prog = compileWithDefinitions defs
                 ["initial-world", "ghost-progs"]
                 (ECall "main" ["initial-world", "ghost-progs"])
      putStr $ showInstSeq prog
