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

parseMatrix :: Parser TNum
parseMatrix = squareParens $ do
  subMatrices <- sepBy (squareParens $ sepBy (lexeme parseTNum') (symbol ",")) (symbol ";")
  if invalidMatrixDimensions subMatrices then
    fail $ "Invalid matrix dimensions."
  else
    return $ TMatrix $ Matrix (length subMatrices) (length.head $ subMatrices) (concat subMatrices)
  where
    squareParens = between (symbol "[") (symbol "]")
    invalidMatrixDimensions subMatrices = not . (all ((== (colN subMatrices)) . length)) $ subMatrices
    colN subMatrices = length . head $ subMatrices

parseTNum' :: Parser TNum
parseTNum' = (try parseComplex) <|> (try $ TDouble <$> double) <|> (TInteger <$> integer)

parseTNum :: Parser TNum
parseTNum = (try parseComplex) <|> (try $ TDouble <$> double) <|> (try $ TInteger <$> integer) <|> parseMatrix
