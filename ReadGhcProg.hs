module ReadGhcProg where

import Data.Char
import Text.ParserCombinators.ReadP
import GHostCPU

-- getProgram :: String -> [Inst]
getProgram =  map (fst . head . filter (null . snd) . readP_to_S parseInst) 
           . filter (not . null)
           . map (trim . fst . break (';'==)) . lines . map toUpper

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseInst :: ReadP Inst
parseInst =   pMov
          +++ pSop
          +++ pBop
          +++ pJmp
          +++ pInt
          +++ pHlt

pMov :: ReadP Inst
pMov = do
     { skipSpaces
     ; string "MOV"
     ; skipSpaces
     ; [a,b] <- sepBy1 parseArg (char ',')
     ; return (MOV a b)
     }

pSop :: ReadP Inst
pSop = do 
     { skipSpaces
     ; sop <- string "INC"+++string "DEC"
     ; a   <- parseArg
     ; return (mkSop sop a)
     }

mkSop :: String -> Arg -> Inst
mkSop "INC" = INC
mkSop "DEC" = DEC

pBop :: ReadP Inst
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

mkBop :: String -> Dest -> Src -> Inst
mkBop bop = case bop of
    "ADD" -> ADD
    "SUB" -> SUB
    "MUL" -> MUL
    "DIV" -> DIV
    "AND" -> AND
    "OR"  -> OR
    "XOR" -> XOR
    _     -> error "mkBop"

pJmp :: ReadP Inst
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

mkJmp :: String -> Targ -> X -> Y -> Inst
mkJmp jmp = case jmp of
    "JLT" -> JLT
    "JEQ" -> JEQ
    "JGT" -> JGT
    _     -> error "mkJmp"

pInt :: ReadP Inst
pInt = do
    { skipSpaces 
    ; _ <- string "INT"
    ; skipSpaces
    ; s <- many1 (satisfy isDigit)
    ; return (INT (read s))
    }

pHlt :: ReadP Inst
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

showArg :: Arg -> String
showArg ArgPC = "PC"
showArg (ArgReg r) = [r]
showArg (ArgInd r) = '[':r:"]"
showArg (ArgConst w) = show w
showArg (ArgLoc w) = '[':(show w++"]")

showInst :: Inst -> String
showInst (MOV d s)      = "MOV "++showArg d++","++showArg s
showInst (INC d)        = "INC "++showArg d
showInst (DEC d)        = "DEC "++showArg d
showInst (ADD d s)      = "ADD "++showArg d++","++showArg s
showInst (SUB d s)      = "SUB "++showArg d++","++showArg s
showInst (MUL d s)      = "MUL "++showArg d++","++showArg s
showInst (DIV d s)      = "DIV "++showArg d++","++showArg s
showInst (AND d s)      = "AND "++showArg d++","++showArg s
showInst (OR d s)       = "OR "++showArg d++","++showArg s
showInst (XOR d s)      = "XOR "++showArg d++","++showArg s
showInst (JLT lab x y)  = "JEQ "++show lab++","++showArg x++","++showArg y
showInst (JEQ lab x y)  = "JEQ "++show lab++","++showArg x++","++showArg y
showInst (JGT lab x y)  = "JGT "++show lab++","++showArg x++","++showArg y
showInst (INT n)        = "INT "++show n
showInst HLT            = "HLT"
