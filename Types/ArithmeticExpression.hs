module Types.ArithmeticExpression
  ( AExpr(..)
  , BinaryOperation(..)
  ) where

import Types.TNum

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
