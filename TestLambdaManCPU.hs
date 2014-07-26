import LambdaManCPU

-- Minimal example of creating and using a local variable.
local_gcc = 
  [ LDC 21
  , LDF body -- load body
  , AP 1 -- call body with 1 variable in a new frame
  , RTN
  -- body:
  , LD 0 0 -- var x
  , LD 0 0 -- var x
  , ADD
  , RTN
  ]
  where
    body = 4

test_local_gcc = do
  m <- newMachine local_gcc
  run m
