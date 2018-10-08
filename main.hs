module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Parsers.AExpressionParser
import Parsers.VariableAssignmentParser
import Parsers.GenericParsers

import Types
import ArithmeticExpressionSolver
import ComputorStateOperations
import VariableAssignment

import Control.Monad.State.Strict
import Data.Bifunctor
import System.IO (hFlush, hIsEOF, stdout, stdin)
import System.Exit (exitSuccess)

data ComputorCommand
  = CAssignment VariableAssignment
  | CAExpr AExpr
  | CVarQuery String
  | CNothing

parseComputorCommand :: Parser ComputorCommand
parseComputorCommand
  =   (CAssignment <$> (try parseVariableAssignment))
  <|> (CAExpr <$> parseAExpression)
  <|> (return CNothing)

computorParser :: Parser ComputorCommand
computorParser = between sc eof parseComputorCommand

formatArithmeticError :: String -> String
formatArithmeticError = unlines . (map indentLog) . (zip [0 .. ]) . lines
  where indentLog (idx, elem) = (replicate (idx * 2) '-') ++ elem

displayArithmeticError :: String -> IO ()
displayArithmeticError = putStr . formatArithmeticError

executeCommand :: ComputorCommand -> ComputorStateT IO ()
executeCommand command = do
  st <- get
  executeCommand' command st
  where
    executeCommand' :: ComputorCommand -> ComputorState -> ComputorStateT IO ()
    executeCommand' (CAExpr expr) state = either (liftIO . displayArithmeticError) (liftIO.print) $ solveExpression expr state
    executeCommand' (CAssignment assignment) state = do
      case assignVariable' assignment state of
        Left error -> liftIO . displayArithmeticError $ error
        Right st -> do
          case getVariable (key assignment) st of
            Just v -> liftIO . print $ v
            Nothing -> liftIO . putStrLn $ "Failed to assign variable. Sorry, dunno why."
          put st

interactiveConsole :: ComputorStateT IO ()
interactiveConsole = do
  i <- liftIO promptInput
  case parse computorParser "" i of
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right CNothing -> interactiveConsole
    Right cmd -> executeCommand cmd
  interactiveConsole
  where
    promptInput = do
      putStr "$> "
      hFlush stdout
      eof <- hIsEOF stdin
      if eof then
        exitSuccess
      else
        getLine

main :: IO ()
main = evalStateT interactiveConsole initialState
