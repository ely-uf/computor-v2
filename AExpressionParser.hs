module AExpressionParser (parseAExpression) where

import TNum
import TNumParser
import GenericParsers
import Text.Megaparsec
import ArithmeticExpression
import Control.Monad.Combinators.Expr

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix ( Neg  <$ symbol "-") ]
  , [ InfixL (BinaryExpr Mod <$ symbol "%") ]
  , [ InfixL (BinaryExpr Pow <$ symbol "^") ]
  , [ InfixL (BinaryExpr Div   <$ symbol "/") 
  , InfixL (BinaryExpr Mul   <$ symbol "*") ]
  , [ InfixL (BinaryExpr Add <$ symbol "+")
  , InfixL (BinaryExpr Sub   <$ symbol "-") ]
  ]

aTerm :: Parser AExpr
aTerm = lexeme $ parens parseAExpression
  <|> ConstVal <$> parseTNum
  <|> Variable <$> identifier

parseAExpression :: Parser AExpr
parseAExpression = makeExprParser aTerm aOperators
