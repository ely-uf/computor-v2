module AExpressionParser (parseAExpression) where

import TNum
import TNumParser
import GenericParsers
import ArithmeticExpression

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Mul <$ symbol "*")
  , InfixL (ABinary Div   <$ symbol "/") 
  , InfixL (ABinary Pow   <$ symbol "^") ]
  , [ InfixL (ABinary Add <$ symbol "+")
  , InfixL (ABinary Sub   <$ symbol "-") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Variable <$> identifier
  <|> ConstVal <$> parseTNum

parseAExpression :: Parser AExpr
parseAExpression = makeExprParser aTerm aOperators
