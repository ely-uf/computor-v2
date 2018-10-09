module Parsers.AExpressionParser
  ( parseAExpression
  , parseFunctionCall
  , parseFunctionDeclaration
  , parseVArg
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

import Data.List (nub)
import Control.Monad (guard)

import Types
import Parsers.TNumParser
import Parsers.GenericParsers
import Operations.Function

parseAnonymousFunction :: Parser Function
parseAnonymousFunction = lexeme $ do
  argumentsList <- parseFunctionDeclArgs
  lexeme $ chunk "->"
  if argumentsList /= nub argumentsList then
    fail "Function arguments must have unique names."
  else do
    functionBody <- parseAExpression
    return $ Function argumentsList [] functionBody

parseFunctionDeclaration :: Parser (String, Function)
parseFunctionDeclaration = lexeme $ do
  functionName <- (:) <$> letterChar <*> many alphaNumChar
  argumentsList <- parseFunctionDeclArgs
  if argumentsList /= nub argumentsList then
    fail "Function arguments must have unique names."
  else do
    symbol "="
    functionBody <- parseAExpression
    return (functionName, Function argumentsList [] functionBody)

parseFunctionCall :: Parser AExpr
parseFunctionCall = lexeme $
  (try parseAnonymousFunctionCall)
  <|> parseNamedFunctionCall

parseAnonymousFunctionCall :: Parser AExpr
parseAnonymousFunctionCall = lexeme $ do
  fn <- lexeme $ parens parseAnonymousFunction
  fArgs <- lexeme $ parseFunctionArgs
  return $ LambdaApplication fn fArgs

parseNamedFunctionCall :: Parser AExpr
parseNamedFunctionCall = lexeme $ do
  expr <- Variable <$> identifier
  fArgs <- parseFunctionArgs
  return $ FunctionCall expr fArgs

parseFunctionDeclArgs :: Parser [String]
parseFunctionDeclArgs = lexeme . parens $ sepBy identifier (lexeme $ chunk ",")

parseFunctionArgs :: Parser [VArg]
parseFunctionArgs = lexeme . parens $ sepBy parseVArg (lexeme $ chunk ",")

parseVArg :: Parser VArg
parseVArg = lexeme $ VArgFunc <$> (try parseAnonymousFunction)
  <|> VArgAExpr <$> (try parseAExpression)
  <|> VArgTNum  <$> parseTNum

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix ( Neg  <$ symbol "-") ]
  , [ InfixL (BinaryExpr Mod <$ symbol "%") ]
  , [ InfixL (BinaryExpr Pow <$ symbol "^")
  , InfixL (BinaryExpr Div   <$ symbol "/")
  , InfixL (BinaryExpr Mul   <$ symbol "*") ]
  , [ InfixL (BinaryExpr Add <$ symbol "+")
  , InfixL (BinaryExpr Sub   <$ symbol "-") ]
  ]

parseNumVariableMultiplication :: Parser AExpr
parseNumVariableMultiplication = lexeme $ do
  num <- try $ choice [TInteger <$> integer, TDouble <$> double]
  var <- try identifier
  return $ BinaryExpr Mul (ConstVal num) (Variable var)

aTerm :: Parser AExpr
aTerm = lexeme $ (try $ parens parseAExpression)
  <|> (try parseFunctionCall)
  <|> (try parseNumVariableMultiplication)
  <|> Variable <$> (try identifier)
  <|> ConstVal <$> parseTNum

parseAExpression :: Parser AExpr
parseAExpression = makeExprParser aTerm aOperators
