module VariableParser
  ( VariableAssignment(..)
  , parseVariableAssignment
  ) where

import TNum
import TNumParser
import GenericParsers
import Text.Megaparsec

data VariableAssignment = VariableAssignment
  { key :: String
  , value :: TNum
  }

parseVariableAssignment :: Parser VariableAssignment
parseVariableAssignment = do
  k <- identifier
  symbol "="
  v <- parseTNum
  return $ VariableAssignment k v
