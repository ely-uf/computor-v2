module Parsers.VariableParser
  ( VariableAssignment(..)
  , parseVariableAssignment
  ) where

import Types.TNum
import Parsers.TNumParser
import Parsers.GenericParsers
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
