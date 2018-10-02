module Parsers.TNumParser (parseTNum) where

import Types
import Parsers.GenericParsers
import Control.Monad.Combinators
import Text.Megaparsec.Char
import Text.Megaparsec

parseComplex :: Parser TNum
parseComplex = do
  imaginaryPart <- option 1.0 $ try $ (lexeme double) <* optional (symbol "*")
  lexeme $ char 'i'
  return $ TComplex 0.0 imaginaryPart

parseTNum :: Parser TNum
parseTNum = (try parseComplex) <|> (try $ TDouble <$> double) <|> (TInteger <$> integer)
