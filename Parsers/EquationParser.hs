module EquationParser (parseEquation) where

import Equation.Types
import Text.Megaparsec
import Parsers.GenericParsers

parseX :: Parser Char
parseX = (symbol "+") <|> (symbol "-")
