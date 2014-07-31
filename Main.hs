{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
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
      case find (\def -> funcName def == "main") defs of
        Nothing -> error "no main function"
        Just def ->
          let arity = length (funcParams def)
              args = ["__arg" ++ show n ++ "__" | n <- [0..arity-1]]
              prog = compileWithDefinitions defs args (ECall "main" (map ERef args))
          in putStr $ showInstSeq prog

