{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module ULambdaParser
  ( parseExpr
  , parseDefinition
  , parseDefinitions
  , parseULambdaFile
  )
  where

import Control.Applicative ((<$>),(<*>),(<*),liftA)
import Data.Char
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (spaces)
import ULambda


parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces >> pExpr <* spaces <* eof) "<string>"

parseDefinition :: String -> Either ParseError TopLevelFuncDefinition
parseDefinition = parse (spaces >> (parseSC <* spaces) <* eof) "<string>"

parseDefinitions :: String -> Either ParseError [TopLevelFuncDefinition]
parseDefinitions = parse (spaces >> many (parseSC <* spaces) <* eof) "<string>"

parseULambdaFile :: FilePath -> IO (Either ParseError [TopLevelFuncDefinition])
parseULambdaFile fname = parseFromFile (spaces >> many (parseSC <* spaces) <* eof) fname


parseULambda :: Parser [TopLevelFuncDefinition]
parseULambda = many1 parseSC

parseSC :: Parser TopLevelFuncDefinition
parseSC = spaces >> parens parseDefine

parseDefine :: Parser TopLevelFuncDefinition
parseDefine = spaces >> string "define" >> spaces >> parens (many1 parseIdent) >>= \ (f:xs) ->
              pExpr >>= \ e -> return (TopLevelFuncDefinition f xs e)

pExpr :: Parser Expr
pExpr = spaces >> (parseAtom <|> parens parseCompound)

parens :: Parser a -> Parser a
parens =  between ( char '(') (char ')')

spaces :: Parser ()
spaces =  skipMany ((space >> return ()) <|> comment)
  where
    comment = char ';' >> manyTill anyChar (try (char '\n')) >> return ()

parseAtom :: Parser Expr
parseAtom = parseEConst
        <|> parseConst
        <|> parseERef
  where
    parseConst = choice $ map try $
      [ string s >> return c
      | (s,c) <- M.toList constTable
      ]

parseEConst :: Parser Expr
parseEConst = many1 digit >>= return . EConst . read

parseERef :: Parser Expr
parseERef = ERef <$> parseIdent

parseIdent :: Parser String
parseIdent = spaces >> (:) <$> letter <*> many (letter <|> digit <|> char '-' <|> char '?') >>= (spaces >>) .  return

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
            , parseEPrimOpN
            , parseTProj
            , parseECall
            , parseELambda ]

parseESet :: Parser Expr
parseESet = string "set!" >> ESet <$> parseIdent <*> pExpr

parseEIf :: Parser Expr
parseEIf = string "if" >> EIf <$> pExpr <*> pExpr <*> pExpr

parseEBegin :: Parser Expr
parseEBegin = string "begin" >> EBegin <$> many1 pExpr

parseELet :: Parser Expr
parseELet = string "let" >> ELet <$> parseDefns <*> pExpr

parseDefns :: Parser [(Ident,Expr)]
parseDefns = spaces >> parens (many1 parseDefn)

parseDefn :: Parser (Ident,Expr)
parseDefn = spaces >> parens ((,) <$> parseIdent <*> pExpr)

parseELetRec :: Parser Expr
parseELetRec = string "letrec" >> ELetRec <$> parseDefns <*> pExpr

parseELetStar :: Parser Expr
parseELetStar = string "let*" >> ELetStar <$> parseDefns <*> pExpr

parseEPrimOp1 :: Parser Expr
parseEPrimOp1 = pPrimOp1 <*> pExpr

pPrimOp1 :: Parser (Expr -> Expr)
pPrimOp1 = choice [ try (string name) >> return op | (name, op) <- M.toList op1Table ]

upcase :: String -> String
upcase = map toUpper

parseEPrimOp2 :: Parser Expr
parseEPrimOp2 = pPrimOp2 <*> pExpr <*> pExpr

pPrimOp2 :: Parser (Expr -> Expr -> Expr)
pPrimOp2 = choice [ try (string name) >> return op | (name, op) <- M.toList op2Table ]

parseEPrimOpN :: Parser Expr
parseEPrimOpN = pPrimOpN <*> many pExpr

pPrimOpN :: Parser ([Expr] -> Expr)
pPrimOpN = choice [ try (string name) >> return op | (name, op) <- M.toList opNTable ]

parseECall :: Parser Expr
parseECall = ECall <$> pExpr <*> many pExpr

parseELambda :: Parser Expr
parseELambda = string "\\" >> spaces >> ELambda <$> parens (many parseIdent) <*> pExpr

parseTProj :: Parser Expr
parseTProj = do
  _ <- string "tproj_"
  n <- liftA read (many digit)
  _ <- char '_'
  i <- liftA read (many digit)
  _ <- spaces
  e <- pExpr
  return $ tproj n i e
