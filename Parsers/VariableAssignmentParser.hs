module Parsers.VariableAssignmentParser
  ( parseVariableAssignment
  ) where

import Types
import Parsers.AExpressionParser
import Parsers.GenericParsers
import Text.Megaparsec

parseVariableAssignment' :: Parser VariableAssignment
parseVariableAssignment' = lexeme $ do
  k <- identifier
  symbol "="
  v <- parseVArg
  return $ VariableAssignment k v

parseFunctionDeclaration' :: Parser VariableAssignment
parseFunctionDeclaration' = lexeme $ do
  (name, fn) <- parseFunctionDeclaration
  return $ VariableAssignment name (VArgFunc fn)

parseVariableAssignment :: Parser VariableAssignment
parseVariableAssignment = try parseVariableAssignment' <|> parseFunctionDeclaration'
