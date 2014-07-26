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

{-
Minimal example of mutual recursion. Note that the recursion in this
example does not terminate. It will fail with an out of memory error
due to the stack use.
-}
goto_gcc = 
  [ DUM  2        -- 2 top-level declarations
  , LDF  go       -- declare function go
  , LDF  to       -- declare function to
  , LDF  main     -- main function
  , RAP  2        -- load declarations into environment and run main
  , RTN           -- final return
  -- main:
  , LDC  1
  , LD   0 0      -- var go
  , AP   1        -- call go(1)
  , RTN
  -- to:
  , LD   0 0      -- var n
  , LDC  1
  , SUB
  , LD   1 0      -- var go
  , AP   1        -- call go(n-1)
  , RTN
  -- go:
  , LD   0 0      -- var n
  , LDC  1
  , ADD
  , LD   1 1      -- var to
  , AP   1        -- call to(n+1)
  , RTN
  ]
  where
    main = 6
    to = 10
    go = 16

test_goto_gcc = do
  m <- newMachine goto_gcc
  runNStep m 100
