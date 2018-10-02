module Types.Function 
  ( Function(..)
  , FunctionError
  , applyFunctionArgs
  ) where

import Data.List (intercalate)

data Function a b = Function
  { args        :: [String]
  , appliedArgs :: [(String, a)]
  , body        :: b
  }

type FunctionError = String

tooManyArgumentsError :: Int -> Int -> FunctionError
tooManyArgumentsError expected applied = "Function Error: Expected " ++ show expected ++ " arguments. But " ++ show applied ++ " applied."

applyFunctionArgs :: Function -> [Value] -> Either String Function
applyFunctionArgs (Function args appliedArgs body) newArgs
  | expectedArgumentsN < newArgumentsN = Left $ tooManyArgumentsError expectedArgumentsN newArgumentsN
  | otherwise = return $ Function args extendedAppliedArguments body
  where expectedArgumentsN = length args - appliedArgumentsN
        appliedArgumentsN  = length appliedArgs
        newArgumentsN      = length newArgs
        extendedAppliedArguments = appliedArgs ++ zip (drop appliedArgumentsN args) newArgs
