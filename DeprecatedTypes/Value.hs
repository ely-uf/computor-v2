module Types.Value (Value(..)) where

import Types.TNum
import Types.Function
import Types.ArithmeticExpression

data Value
  = VFunc (Function Value AExpr)
  | VNum  TNum

instance Show Value where
  show (VFunc f) = show f
  show (VNum n ) = show n
