module Parsers.AExpressionParser
  ( parseAExpression
  , parseFunctionCall
  , parseVArg
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

import Debug.Trace

import Types
import Parsers.TNumParser
import Parsers.GenericParsers
import FunctionOperations

parseAnonymousFunction :: Parser Function
parseAnonymousFunction = lexeme $ do
  argumentsList <- parseFunctionDeclArgs
  lexeme $ chunk "->"
  functionBody <- parseAExpression
  return $ Function argumentsList [] functionBody

parseFunctionDeclaration :: Parser (String, Function)
parseFunctionDeclaration = lexeme $ do
  functionName <- (:) <$> letterChar <*> many alphaNumChar
  argumentsList <- parseFunctionDeclArgs
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
parseFunctionDeclArgs = lexeme . parens $ sepBy identifier (lexeme $ chunk ", ") 

parseFunctionArgs :: Parser [VArg]
parseFunctionArgs = lexeme . parens $ sepBy parseVArg (lexeme $ chunk ", ")

parseVArg :: Parser VArg
parseVArg = lexeme $ VArgFunc <$> parseAnonymousFunction
  <|> VArgAExpr <$> (try parseAExpression)
  <|> VArgTNum  <$> (try parseTNum)

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
aTerm = lexeme $ (try $ parens parseAExpression)
  <|> (try parseFunctionCall)
  <|> Variable <$> identifier
  <|> ConstVal <$> (try parseTNum)

parseAExpression :: Parser AExpr
parseAExpression = makeExprParser aTerm aOperators
