{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module ULambdaParser
      where

import Control.Applicative ((<$>),(<*>))
import Data.Char
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (spaces)
import ULambda

parseULambda :: Parser [TopLevelFuncDefinition]
parseULambda = many1 parseSC

parseSC :: Parser TopLevelFuncDefinition
parseSC = spaces >> parens parseDefine

parseDefine = spaces >> string "define" >> spaces >> parens (many1 parseIdent) >>= \ (f:xs) ->
              parseExpr >>= \ e -> return (TopLevelFuncDefinition f xs e)

parseExpr :: Parser Expr
parseExpr = spaces >> (parseAtom <|> parens parseCompound)

parens =  between ( char '(') (char ')')
spaces =  skipMany space

parseAtom :: Parser Expr
parseAtom = parseEConst
        <|> parseERef

parseEConst :: Parser Expr
parseEConst = many1 digit >>= return . EConst . read

parseERef :: Parser Expr
parseERef = ERef <$> parseIdent

parseIdent :: Parser String
parseIdent = spaces >> (:) <$> letter <*> many (letter <|> digit) >>= (spaces >>) .  return

parseCompound :: Parser Expr
parseCompound = parseESet
            <|> parseEIf
            <|> parseEBegin
            <|> parseELet
            <|> parseELetRec
            <|> parseEPrimOp1
            <|> parseEPrimOp2
            <|> parseECall
            <|> parseELambda
       
parseESet :: Parser Expr
parseESet = string "set!" >> ESet <$> parseIdent <*> parseExpr

parseEIf :: Parser Expr
parseEIf = string "if" >> EIf <$> parseExpr <*> parseExpr <*> parseExpr

parseEBegin :: Parser Expr
parseEBegin = string "begin" >> EBegin <$> many1 parseExpr

parseELet :: Parser Expr
parseELet = string "let" >> ELet <$> parseDefns <*> parseExpr

parseDefns :: Parser [(Ident,Expr)]
parseDefns = spaces >> parens (many1 parseDefn)

parseDefn :: Parser (Ident,Expr)
parseDefn = spaces >> parens ((,) <$> parseIdent <*> parseExpr)

parseELetRec :: Parser Expr
parseELetRec = string "letrec" >> ELetRec <$> parseDefns <*> parseExpr

parseEPrimOp1 :: Parser Expr
parseEPrimOp1 = EPrimOp1 <$> pPrimOp1 <*> parseExpr

pPrimOp1 :: Parser Ident
pPrimOp1 = upcase <$> string "atom" <|> string "car" <|> string "cdr"

upcase :: String -> String
upcase = map toUpper

parseEPrimOp2 :: Parser Expr
parseEPrimOp2 = EPrimOp2 <$> pPrimOp2 <*> parseExpr <*> parseExpr

pPrimOp2 :: Parser Ident
pPrimOp2 = maybe (error "Unknown primitive operator")
                 id . flip M.lookup primop2s
       <$> (   string "+" <|> string "-" <|> string "*"
           <|> string "/" <|> string "=" <|> string ">"
           <|> string ">=" <|> string ":"
           )

primop2s :: M.Map String Ident
primop2s = M.fromList
         $ [("+","ADD"),("-","ADD"),("*","MUL"),("/","DIV")
           ,("=","CEQ"),(">","CGT"),(">=","CGTE"),(":","CONS")]

parseECall :: Parser Expr
parseECall = ECall <$> parseExpr <*> many parseExpr

parseELambda :: Parser Expr
parseELambda = string "\\" >> spaces >> ELambda <$> parens (many parseIdent) <*> parseExpr
