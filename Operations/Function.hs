module Operations.Function
  ( applyFunctionArgs
  , canBeCalled
  , injectArgs
  ) where

import Types
import Operations.ComputorState
import Data.List (intercalate)

tooManyArgumentsError :: Int -> Int -> FunctionError
tooManyArgumentsError expected applied = "Function Error: Expected " ++ show expected ++ " arguments. But " ++ show applied ++ " applied.\n"

canBeCalled :: Function -> Bool
canBeCalled (Function args appliedArgs _) = length args == length appliedArgs

injectArgs :: [(String, Value)] -> ComputorState -> ComputorState
injectArgs [] state = state 
injectArgs ((key, val):xs) state = injectArgs xs (setVariable key val state)

applyFunctionArgs :: Function -> [Value] -> Either String Function
applyFunctionArgs (Function args appliedArgs body) newArgs
  | expectedArgumentsN < newArgumentsN = Left $ tooManyArgumentsError expectedArgumentsN newArgumentsN
  | otherwise = return $ Function args extendedAppliedArguments body
  where expectedArgumentsN = length args - appliedArgumentsN
        appliedArgumentsN  = length appliedArgs
        newArgumentsN      = length newArgs
        extendedAppliedArguments = appliedArgs ++ zip (drop appliedArgumentsN args) newArgs
