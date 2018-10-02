module ValueFunctionCall
  ( canBeCalled
  , callValueFunction
  ) where

import Types.Value
import Types.ValueFunction
import Types.ComputorState
import ArithmeticExpressionSolver

canBeCalled :: Function a -> Bool
canBeCalled (Function args appliedArgs _) = length args == length appliedArgs

callValueFunction :: ValueFunction -> ComputorState -> Either FunctionError TNum
callValueFunction (Function _ args expr) st = solveExpression expr (injectArgs args st)

injectArgs :: [(String, Value)] -> ComputorState -> ComputorState
injectArgs [] state = state 
injectArgs ((key, val):xs) state = injectArgs xs (setVariable key val state)
