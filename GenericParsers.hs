module GenericParsers where

import Data.Void
import Data.Char (isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where
    lineCmnt  = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme p = L.lexeme sc p

symbol :: String -> Parser String
symbol s = L.symbol sc s

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

double :: Parser Double
double = (try double') <|> (fromIntegral <$> integer)
  where
    double' = do
      integral <- takeWhile1P Nothing isDigit
      char '.'
      fractional <- takeWhile1P Nothing isDigit
      return $ read (integral ++ '.':fractional)
