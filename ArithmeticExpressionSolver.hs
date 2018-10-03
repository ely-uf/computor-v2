{-# LANGUAGE MultiWayIf #-}

module ArithmeticExpressionSolver
  ( ExpressionError
  , solveExpression
  ) where

import Types
import FunctionOperations
import ComputorStateOperations
import Data.Bifunctor

callFunction :: Function -> ComputorState -> Either FunctionError Value
callFunction (Function _ args expr) st = solveExpression expr (injectArgs args st)

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

flattenVArgsToValues :: [VArg] -> ComputorState -> Either ExpressionError [Value]
flattenVArgsToValues [] st = return []
flattenVArgsToValues ((VArgTNum n):xs) st = (:) <$> (Right $ VNum n) <*> flattenVArgsToValues xs st
flattenVArgsToValues ((VArgFunc f):xs) st = (:) <$> (Right $ VFunc f) <*> flattenVArgsToValues xs st
flattenVArgsToValues ((VArgAExpr e):xs) st = (:) <$> solveExpression e st <*> flattenVArgsToValues xs st

solveExpression :: AExpr -> ComputorState -> Either ExpressionError Value
solveExpression (LambdaApplication lambda args) st = do
  flattenedArgs <- flattenVArgsToValues args st
  appliedFn <- applyFunctionArgs lambda flattenedArgs
  if canBeCalled appliedFn then
    callFunction appliedFn st
  else
    return $ VFunc appliedFn

solveExpression (FunctionCall expr args) st = do
  res <-  solveExpression expr st
  case res of 
    (VFunc fn) -> solveExpression (LambdaApplication fn args) st
    notFunc -> Left $ "TypeError: " ++ (show notFunc) ++ " is not a function."

solveExpression (Variable a) st = case getVariable a st of
                                    Just n -> return n
                                    Nothing -> Left $ "Variable Error: Undefined variable " ++ (show a) ++ "\n"

solveExpression e st = VNum <$> solveExpression' e st


unsafeExtractTNumFromValue :: Value -> TNum
unsafeExtractTNumFromValue (VNum n) = n
unsafeExtractTNumFromValue _ = error "This is not an error. This is a bonus."

typeCheckTNum :: Value -> Either ExpressionError TNum
typeCheckTNum (VNum n) = return n
typeCheckTNum v = Left $ "TypeError: Expected num, but received function " ++ show v

solveExpression' :: AExpr -> ComputorState -> Either ExpressionError TNum
solveExpression' (ConstVal a) st = return a

solveExpression' e@(Neg a) st = case solveExpression a st of
                                  Left err -> Left $ propagateArithmeticError e err
                                  Right n -> negate <$> typeCheckTNum n

solveExpression' e@(BinaryExpr op a b) st = bimap (propagateArithmeticError e) id $ do
  a1 <- solveExpression a st
  b1 <- solveExpression b st
  if | (not $ isValueTNum a1) -> fail $ "Type Error: The first argument (" ++ show a1 ++ ") of arithmetic expression " ++ show e ++ " is not a number."
     | (not $ isValueTNum b1) -> fail $ "Type Error: The second argument (" ++ show b1 ++ ") of arithmetic expression " ++ show e ++ " is not a number."
     | otherwise -> solveBinaryExpression (unsafeExtractTNumFromValue a1) (unsafeExtractTNumFromValue b1)
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