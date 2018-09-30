module ArithmeticExpression
  ( AExpr(..)
  , BinaryOperation(..)
  , ExpressionError
  , solveExpression
  ) where

import TNum
import TNumParser
import ComputorState
import Data.Bifunctor

data AExpr
  = Variable String
  | ConstVal TNum
  | Neg AExpr
  | BinaryExpr BinaryOperation AExpr AExpr

data BinaryOperation
  = Add
  | Sub
  | Div
  | Mul
  | Pow 
  | Mod
  | MatrixMul

instance Show AExpr where
  show (Variable a) = a
  show (ConstVal a) = show a
  show (Neg a) = '-' : (show a)
  show (BinaryExpr op a1 a2) = '(' : (show a1) ++ " " ++ (show op) ++ " " ++ (show a2) ++ ")"

instance Show BinaryOperation where
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mul = "*"
  show Pow = "^"
  show Mod = "%"
  show MatrixMul = "**"

type ExpressionError = String

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
a ^? b = Left $ "Type error: Trying to raise " ++ (show a) ++ " to the power of " ++ (show b) ++ ".\n"

(%?) :: TNum -> TNum -> Either ExpressionError TNum
(TInteger a) %? (TInteger b) = return $ TInteger (a `mod` b)
a %? b = Left $ "Type error: Trying to modulo " ++ (show a) ++ " and " ++ (show b) ++ ".\n"

propagateArithmeticError :: AExpr -> ExpressionError -> ExpressionError
propagateArithmeticError a = ((flip (++)) (("In the expression " ++ (show a)) ++ "\n"))

solveExpression :: AExpr -> ComputorState -> Either ExpressionError TNum
solveExpression (Variable a) st = case getNumVariable a st of
                                    Just n -> return n
                                    Nothing -> Left $ "ExpansionError: Undefined variable " ++ (show a) ++ "\n"
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
