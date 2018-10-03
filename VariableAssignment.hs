module VariableAssignment
  ( assignVariable
  , assignVariable'
  ) where

import Types
import ArithmeticExpressionSolver
import ComputorStateOperations

assignVariable :: String -> VArg -> ComputorState -> Either String ComputorState
assignVariable key (VArgTNum n) st = return $ setNumVariable key n st
assignVariable key (VArgFunc f) st = return $ setFuncVariable key f st
assignVariable key (VArgAExpr expr) st = case solveExpression expr st of 
  Right n  -> return $ setVariable key n st
  Left err -> Left err

assignVariable' :: VariableAssignment -> ComputorState -> Either String ComputorState
assignVariable' (VariableAssignment k v) = assignVariable k v
