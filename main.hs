module Main where

import Text.Megaparsec
import Text.Megaparsec.Error
import ArithmeticExpression
import AExpressionParser
import VariableParser
import GenericParsers

import ComputorState

import Text.Megaparsec
import Data.Bifunctor
import System.IO (hFlush, stdout)
import Control.Monad.State.Strict

data ComputorCommand
  = CAssignment VariableAssignment
  | CAExpr AExpr

parseComputorCommand :: Parser ComputorCommand
parseComputorCommand
  = (CAssignment <$> (try parseVariableAssignment))
  <|> (CAExpr <$> (try parseAExpression))

computorParser :: Parser ComputorCommand
computorParser = between sc eof parseComputorCommand

formatArithmeticError :: String -> String
formatArithmeticError = unlines . (map indentLog) . (zip [0 .. ]) . lines
  where indentLog (idx, elem) = (replicate (idx * 2) '-') ++ elem

displayArithmeticError :: String -> IO ()
displayArithmeticError = putStr . formatArithmeticError

interactiveConsole :: StateT ComputorState IO ()
interactiveConsole = do
  i <- liftIO promptInput
  case parse computorParser "" i of
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right (CAExpr parsedExpr) -> do
      st <- get
      processInput parsedExpr st
    Right (CAssignment (VariableAssignment k v)) -> do
      modify (setNumVariable k v)
  interactiveConsole
  where
    promptInput = do
      putStr "$> "
      hFlush stdout
      getLine
    processInput expr st = (either (liftIO.displayArithmeticError) (liftIO.print)) $ solveExpression expr st

main :: IO ()
main = evalStateT interactiveConsole initialState
