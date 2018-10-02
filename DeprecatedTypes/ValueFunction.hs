module Types.ValueFunction
  ( ValueFunction
  ) where

import Types.Value
import Types.Function
import Types.ArithmeticExpression

type ValueFunction = Function Value AExpr

instance Show ValueFunction where
  show (Function _args [] _body) = '(': intercalate ", " _args ++ ") -> " ++ show _body
  show (Function _args _appliedArgs _body) = show (Function substitutedArgs [] _body)
    where substitutedArgs = map substitute _args
          substitute arg = case find ((== arg).fst) _appliedArgs of
                              Nothing     -> arg
                              Just (_, v) -> show v
