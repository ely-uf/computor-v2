module Parsers.VariableAssignmentParser
  ( parseVariableAssignment
  ) where

import Types
import Parsers.GenericParsers
import Parsers.FunctionParser

parseVariableAssignment :: Parser VariableAssignment
parseVariableAssignment = do
  k <- identifier
  symbol "="
  v <- parseVArg
  return $ VariableAssignment k v
