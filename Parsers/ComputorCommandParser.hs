module Parsers.ComputorCommandParser
  ( parseComputorCommand
  ) where

import Types

import Text.Megaparsec
import Parsers.GenericParsers
import Parsers.AExpressionParser
import Parsers.EquationParser
import Parsers.VariableAssignmentParser
import Parsers.BuiltinParser

parseComputorCommand :: Parser ComputorCommand
parseComputorCommand
  =   (CAssignment <$> (try parseVariableAssignment))
  <|> (CBuiltin <$> parseBuiltin)
  <|> (CEquation <$> (try parseEquation <* (optional $ symbol "=" >> symbol "?")))
  <|> (CAExpr <$> (parseAExpression <* (optional $ symbol "=" >> symbol "?")))
  <|> (return CNothing)
