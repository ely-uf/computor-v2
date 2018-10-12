module Parsers.BuiltinParser
  ( parseBuiltin
  ) where

import Builtins
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Parsers.GenericParsers

parseBuiltin :: Parser String
parseBuiltin = lexeme $ do
  char '@' :: Parser Char
  choice $ ((chunk <$> builtinNames) :: [Parser String])
