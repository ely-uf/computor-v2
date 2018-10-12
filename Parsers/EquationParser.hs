module Parsers.EquationParser (parseEquation) where

import Types
import Text.Megaparsec
import Parsers.GenericParsers

parseX :: Parser Char
parseX = (head <$> symbol "x") <|> (head <$> symbol "X")

parseTSign :: Parser TSign
parseTSign = (symbol "+" >> return TPlus) <|> (symbol "-" >> return TMinus)

parseETNum :: Parser ETNum
parseETNum = lexeme $ (try $ ETDouble <$> double) <|> (ETInt <$> integer)

parseCoeff :: Parser ETNum
parseCoeff = lexeme $ do
  x <- parseETNum
  optional $ symbol "*"
  return x

parseDegree :: Parser ETNum
parseDegree = symbol "^" >> parseETNum

parseXTerm :: Parser Term
parseXTerm = lexeme $ do
  coef <- lexeme $ option (ETInt 1) (try parseCoeff)
  parseX
  degree <- lexeme $ option (ETInt 1) (try parseDegree)
  return $ Term coef degree

parseTerm :: Parser Term
parseTerm = try parseXTerm <|> fmap (flip Term (ETInt 0)) parseETNum

parseSignedTerm :: Parser Term
parseSignedTerm = lexeme $ do
  sign <- parseTSign
  term <- parseTerm
  case sign of
    TMinus -> return $ negate term
    _      -> return term

parseTerms :: Parser [Term]
parseTerms = lexeme $ do
  x  <- parseTerm
  xs <- many parseSignedTerm
  return (x:xs)

parseEquation :: Parser Equation
parseEquation = do
  lhs <- parseTerms
  symbol "="
  rhs <- parseTerms
  return $ Equation lhs rhs
