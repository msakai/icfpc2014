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
parseIdent = spaces >> (:) <$> letter <*> many (letter <|> digit <|> char '-') >>= (spaces >>) .  return

parseCompound :: Parser Expr
parseCompound = choice $ map try
            [ parseESet
            , parseEIf
            , parseEBegin
            , parseELet
            , parseELetRec
            , parseELetStar
            , parseEPrimOp1
            , parseEPrimOp2
            , parseECall
            , parseELambda ]

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

parseELetStar :: Parser Expr
parseELetStar = string "let*" >> ELetStar <$> parseDefns <*> parseExpr

parseEPrimOp1 :: Parser Expr
parseEPrimOp1 = pPrimOp1 <*> parseExpr

pPrimOp1 :: Parser (Expr -> Expr)
pPrimOp1 = choice [ try (string name) >> return op | (name, op) <- M.toList op1Table ]

upcase :: String -> String
upcase = map toUpper

parseEPrimOp2 :: Parser Expr
parseEPrimOp2 = pPrimOp2 <*> parseExpr <*> parseExpr

pPrimOp2 :: Parser (Expr -> Expr -> Expr)
pPrimOp2 = choice [ try (string name) >> return op | (name, op) <- M.toList op2Table ]

parseEPrimOpN :: Parser Expr
parseEPrimOpN = pPrimOpN <*> many parseExpr

pPrimOpN :: Parser ([Expr] -> Expr)
pPrimOpN = choice [ try (string name) >> return op | (name, op) <- M.toList opNTable ]

parseECall :: Parser Expr
parseECall = ECall <$> parseExpr <*> many parseExpr

parseELambda :: Parser Expr
parseELambda = string "\\" >> spaces >> ELambda <$> parens (many parseIdent) <*> parseExpr
