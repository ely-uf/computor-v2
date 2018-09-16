module TNumParser (parseTNum) where

import TNum
import GenericParsers

parseComplex :: Parser TNum
parseComplex = do
  realPart <- option 0.0 (lexeme double)
  symbol "+"
  imaginaryPart <- lexeme double

parseTNum :: Parser TNum
parseTNum =  (try $ TDouble <$> double)
  <|> (try parseComplex)
