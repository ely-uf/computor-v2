module TNumParser (parseTNum) where

import TNum
import GenericParsers
import Text.Megaparsec
import Text.Megaparsec.Char

parseComplex :: Parser TNum
parseComplex = do
  realPart <- option 0.0 (lexeme double)
  c <- (try $ symbol "+") <|> symbol "-"
  imaginaryPart <- option 1.0 (lexeme double <* optional (lexeme $ char '*'))
  lexeme $ char 'i'
  case c of
    "-" -> return (TComplex realPart (negate imaginaryPart))
    _   -> return (TComplex realPart imaginaryPart)

parseTNum :: Parser TNum
parseTNum =  (try $ TDouble <$> double) <|> parseComplex
