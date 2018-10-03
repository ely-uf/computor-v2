module Parsers.VariableAssignmentParser
  ( parseVariableAssignment
  ) where

import Types
import Parsers.AExpressionParser
import Parsers.GenericParsers

parseVariableAssignment :: Parser VariableAssignment
parseVariableAssignment = lexeme $ do
  k <- identifier
  symbol "="
  v <- parseVArg
  return $ VariableAssignment k v
