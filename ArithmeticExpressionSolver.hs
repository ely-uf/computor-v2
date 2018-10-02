module ArithmeticExpressionSolver
  ( ExpressionError
  , solveExpression
  ) where

import Types
import ComputorStateOperations
import Data.Bifunctor

(+?) :: TNum -> TNum -> Either ExpressionError TNum
a +? b = return $ a + b

(-?) :: TNum -> TNum -> Either ExpressionError TNum
a -? b = return $ a - b

(*?) :: TNum -> TNum -> Either ExpressionError TNum
a *? b = return $ a * b

(/?) :: TNum -> TNum -> Either ExpressionError TNum
a /? b = return $ a / b

(^?) :: TNum -> TNum -> Either ExpressionError TNum
(TInteger a) ^? (TInteger b) | b > 0 = return $ TInteger (a ^ b)
a ^? b = Left $ "Type Error: Trying to raise " ++ (show a) ++ " to the power of " ++ (show b) ++ ".\n"

(%?) :: TNum -> TNum -> Either ExpressionError TNum
(TInteger a) %? (TInteger b) = return $ TInteger (a `mod` b)
a %? b = Left $ "Type Error: Trying to modulo " ++ (show a) ++ " and " ++ (show b) ++ ".\n"

propagateArithmeticError :: AExpr -> ExpressionError -> ExpressionError
propagateArithmeticError a = ((flip (++)) (("In the expression " ++ (show a)) ++ "\n"))

solveExpression :: AExpr -> ComputorState -> Either ExpressionError TNum
solveExpression (Variable a) st = case getNumVariable a st of
                                    Just n -> return n
                                    Nothing -> Left $ "Variable Error: Undefined numeric variable " ++ (show a) ++ "\n"
solveExpression (ConstVal a) st = return a
solveExpression e@(Neg a) st = bimap (propagateArithmeticError e) negate $ solveExpression a st
solveExpression e@(BinaryExpr op a b) st = bimap (propagateArithmeticError e) id $ do
  a1 <- solveExpression a st
  b1 <- solveExpression b st
  solveBinaryExpression a1 b1
  where
    solveBinaryExpression a1 b1
      = case op of
        Add -> a1 +? b1
        Sub -> a1 -? b1
        Div -> a1 /? b1
        Mul -> a1 *? b1
        Pow -> a1 ^? b1
        Mod -> a1 %? b1
        a -> error $ "Trying to solve binary operation " ++ (show a)
solveExpression _ _ = errorWithoutStackTrace "Variables are still not supported"
