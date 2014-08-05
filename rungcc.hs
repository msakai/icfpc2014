module Main where

import Control.Monad
import Data.IORef
import System.Environment
import System.IO
import qualified LambdaManCPU

main :: IO ()
main = do
  [fname] <- getArgs
  s <- readFile fname
  let prog :: [LambdaManCPU.Inst]
      prog = map read (lines s)
  m <- LambdaManCPU.newMachine prog
  LambdaManCPU.run m
  vs <- readIORef (LambdaManCPU.mS m)
  forM_ vs $ \v -> do
    putStrLn =<< LambdaManCPU.showValue v
