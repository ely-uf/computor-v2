module Main where

import Text.Megaparsec
import AExpressionParser

main = do
  i <- getLine
  parseTest parseAExpression "" i
