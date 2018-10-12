module ComputorCommand
  ( ComputorCommand(..)
  , parseComputorCommand
  , executeCommand
  ) where

import Types

import Text.Megaparsec
import Parsers.GenericParsers
import Parsers.AExpressionParser
import Parsers.EquationParser
import Parsers.VariableAssignmentParser

import VariableAssignment
import Operations.Equation
import Operations.ComputorState
import Operations.ArithmeticExpression

import Data.Bifunctor
import Control.Monad.Combinators
import Control.Monad.State.Strict

data ComputorCommand
  = CAssignment VariableAssignment
  | CEquation Equation
  | CAExpr AExpr
  | CVarQuery String
  | CNothing

parseComputorCommand :: Parser ComputorCommand
parseComputorCommand
  =   (CAssignment <$> (try parseVariableAssignment))
  <|> (CEquation <$> (try parseEquation <* (optional $ symbol "=" >> symbol "?")))
  <|> (CAExpr <$> (parseAExpression <* (optional $ symbol "=" >> symbol "?")))
  <|> (return CNothing)

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
    executeCommand' (CEquation eq) state = liftIO (solveEquation eq) >> return ()
    executeCommand' (CAssignment assignment) state = do
      case assignVariable' assignment state of
        Left error -> liftIO . displayArithmeticError $ error
        Right st -> do
          case getVariable (key assignment) st of
            Just v -> liftIO . print $ v
            Nothing -> liftIO . putStrLn $ "Failed to assign variable. Sorry, dunno why."
          put st

