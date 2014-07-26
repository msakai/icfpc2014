module ReadGhcProg where

import Data.Char
import Text.ParserCombinators.ReadP
import GHostCPU

getProgram :: String -> [Inst]
getProgram = map (fst . head . filter (null . snd) . readP_to_S parseInst) . map (fst . break (';'==)) . lines

parseInst =   pMov
          +++ pSop
          +++ pBop
          +++ pJmp
          +++ pInt
          +++ pHlt

pMov = do
     { skipSpaces
     ; string "MOV"
     ; skipSpaces
     ; [a,b] <- sepBy1 parseArg (char ',')
     ; return (MOV a b)
     }
pSop = do 
     { skipSpaces
     ; sop <- string "INC"+++string "DEC"
     ; a   <- parseArg
     ; return (mkSop sop a)
     }

mkSop "INC" = INC
mkSop "DEC" = DEC

pBop = do
    { skipSpaces
    ; bop <- string "ADD"+++string "SUB"
          +++string "MUL"+++string "DIV"
          +++string "AND"+++string "OR"
          +++string "XOR"
    ; skipSpaces
    ; [a,b] <- sepBy1 parseArg (char ',')
    ; return (mkBop bop a b)
    }
mkBop bop = case bop of
    "ADD" -> ADD
    "SUB" -> SUB
    "MUL" -> MUL
    "DIV" -> DIV
    "AND" -> AND
    "OR"  -> OR
    "XOR" -> XOR

pJmp = do
    { skipSpaces
    ; jmp <- string "JLT"+++string "JEQ"+++string "JGT"
    ; skipSpaces
    ; lab <- many1 (satisfy isDigit)
    ; skipSpaces
    ; char ','
    ; skipSpaces
    ; [x,y] <- sepBy1 parseArg (char ',')
    ; return (mkJmp jmp (read lab) x y)
    }
mkJmp jmp = case jmp of
    "JLT" -> JLT
    "JEQ" -> JEQ
    "JGT" -> JGT

pInt = do
    { skipSpaces 
    ; _ <- string "INT"
    ; skipSpaces
    ; s <- many1 (satisfy isDigit)
    ; return (INT (read s))
    }

pHlt = do
    { skipSpaces 
    ; _ <- string "HLT"
    ; return HLT
    }

parseArg :: ReadP Arg
parseArg =   argPC             
         +++ argReg
         +++ argInd
         +++ argConst
         +++ argLoc

argPC :: ReadP Arg
argPC  = skipSpaces >> string "PC" >>= \_ -> return ArgPC

argReg :: ReadP Arg
argReg = do 
    { skipSpaces;
    ; c <- satisfy (`elem` "ABCDEFGH")
    ; skipSpaces;
    ; return (ArgReg c)
    }

argInd :: ReadP Arg
argInd = do 
    { skipSpaces; 
    ; char '['
    ; skipSpaces;
    ; c <- satisfy (`elem` "ABCDEFGH")
    ; skipSpaces;
    ; char ']'
    ; skipSpaces;
    ; return (ArgInd c)
    }

argConst :: ReadP Arg
argConst = do
    { skipSpaces
    ; num <- many1 (satisfy isDigit)
    ; return (ArgConst (read num))
    }

argLoc :: ReadP Arg
argLoc = do
    { skipSpaces; 
    ; char '['
    ; skipSpaces;
    ; num <- many1 (satisfy isDigit)
    ; skipSpaces;
    ; char ']'
    ; skipSpaces;
    ; return (ArgLoc (read num))
    }
