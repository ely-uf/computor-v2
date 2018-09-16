module ArithmeticExpression where

import TNum
import TNumParser

data AExpr
  = Variable String
  | ConstVal TNum
  | Neg AExpr
  | BinaryExpr BinaryOperation AExpr AExpr
  deriving Show

data BinaryOperation
  = Add
  | Sub
  | Div
  | Mul
  | Pow 
  deriving Show
