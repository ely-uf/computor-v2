module FunctionOperations where

import Types
import Data.List (intercalate)

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
