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
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (spaces)
import ULambda


parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces >> pExpr <* spaces <* eof) "<string>"

parseDefinition :: String -> Either ParseError TopLevelFuncDefinition
parseDefinition = parse (spaces >> parseSC <* spaces <* eof) "<string>"

parseDefinitions :: String -> Either ParseError [TopLevelFuncDefinition]
parseDefinitions = parse parseULambda "<string>"

parseULambdaFile :: FilePath -> IO (Either ParseError [TopLevelFuncDefinition])
parseULambdaFile fname = parseFromFile parseULambda fname


parseULambda :: Parser [TopLevelFuncDefinition]
parseULambda = spaces >> many (parseSC <* spaces) <* eof

parseSC :: Parser TopLevelFuncDefinition
parseSC = parens parseDefine

parseDefine :: Parser TopLevelFuncDefinition
parseDefine = string "define" >> spaces1 >> parens (many1 parseIdent) >>= \ (f:xs) ->
              pExpr >>= \ e -> return (TopLevelFuncDefinition f xs e)

pExpr :: Parser Expr
pExpr = (parseAtom <* spaces) <|> parens parseCompound

parens :: Parser a -> Parser a
parens =  between (char '(' >> spaces) (char ')' >> spaces)

spaces :: Parser ()
spaces =  skipMany ((space >> return ()) <|> comment)

spaces1 :: Parser ()
spaces1 =  skipMany1 ((space >> return ()) <|> comment)

comment :: Parser ()
comment = char ';' >> manyTill anyChar (try (char '\n')) >> return ()

parseAtom :: Parser Expr
parseAtom = parseEConst
        <|> parseConst
        <|> parseERef
  where
    parseConst = choice $ 
      [ try (string s >> spaces) >> return c
      | (s,c) <- M.toList constTable
      ]

parseEConst :: Parser Expr
parseEConst = EConst <$> (liftA read $ many1 digit <* spaces)

parseERef :: Parser Expr
parseERef = ERef <$> parseIdent

parseIdent :: Parser String
parseIdent = ((:) <$> letter <*> many (letter <|> digit <|> char '-' <|> char '?')) <* spaces

parseCompound :: Parser Expr
parseCompound = choice $ 
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
            , parseELambda
            , parseECall -- 一番genericなやつなので最後に
            ]

parseESet :: Parser Expr
parseESet = try (string "set!" >> spaces1) >> ESet <$> parseIdent <*> pExpr

parseEIf :: Parser Expr
parseEIf = try (string "if" >> spaces1) >> EIf <$> pExpr <*> pExpr <*> pExpr

parseEBegin :: Parser Expr
parseEBegin = try (string "begin" >> spaces1) >> EBegin <$> many1 pExpr

parseELet :: Parser Expr
parseELet = try (string "let" >> spaces1) >> ELet <$> parseDefns <*> pExpr

parseDefns :: Parser [(Ident,Expr)]
parseDefns = parens (many1 parseDefn)

parseDefn :: Parser (Ident,Expr)
parseDefn = parens ((,) <$> parseIdent <*> pExpr)

parseELetRec :: Parser Expr
parseELetRec = try (string "letrec" >> spaces1) >> ELetRec <$> parseDefns <*> pExpr

parseELetStar :: Parser Expr
parseELetStar = try (string "let*" >> spaces1) >> ELetStar <$> parseDefns <*> pExpr

parseEPrimOp1 :: Parser Expr
parseEPrimOp1 = pPrimOp1 <*> pExpr

pPrimOp1 :: Parser (Expr -> Expr)
pPrimOp1 = choice [ try (string name >> spaces1)  >> return op | (name, op) <- M.toList op1Table ]

parseEPrimOp2 :: Parser Expr
parseEPrimOp2 = pPrimOp2 <*> pExpr <*> pExpr

pPrimOp2 :: Parser (Expr -> Expr -> Expr)
pPrimOp2 = choice [ try (string name >> spaces1) >> return op | (name, op) <- M.toList op2Table ]

parseEPrimOpN :: Parser Expr
parseEPrimOpN = pPrimOpN <*> many pExpr

pPrimOpN :: Parser ([Expr] -> Expr)
pPrimOpN = choice [ try (string name >> spaces1) >> return op | (name, op) <- M.toList opNTable ]

parseECall :: Parser Expr
parseECall = ECall <$> pExpr <*> many pExpr

parseELambda :: Parser Expr
parseELambda = try (string "\\" >> spaces) >> ELambda <$> parens (many parseIdent) <*> pExpr

parseTProj :: Parser Expr
parseTProj = do
  (n,i) <- try $ do    
    _ <- string "tproj_"
    n <- liftA read (many digit)
    _ <- char '_'
    i <- liftA read (many digit)
    _ <- spaces1
    return (n,i)
  e <- pExpr
  return $ tproj n i e
