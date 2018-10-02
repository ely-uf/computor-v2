module Parsers.FunctionParser
  ( parseAnonymousFunction
  , parseFunctionDeclaration
  , parseFunctionCall
  , parseVArg
  ) where

import Types

import Text.Megaparsec
import Text.Megaparsec.Char
import Parsers.TNumParser
import Parsers.GenericParsers
import Parsers.AExpressionParser
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
parseNamedFunctionCall = fail "aye"

parseFunctionDeclArgs :: Parser [String]
parseFunctionDeclArgs = lexeme . parens $ sepBy identifier (lexeme $ chunk ", ") 

parseFunctionArgs :: Parser [VArg]
parseFunctionArgs = lexeme . parens $ sepBy parseVArg (lexeme $ chunk ", ")

parseVArg :: Parser VArg
parseVArg = lexeme $ FAExpr <$> (try parseAExpression)
  <|> FTNum <$> (try parseTNum)
  <|> FFunc <$> parseAnonymousFunction
