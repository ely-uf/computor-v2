module Types.ComputorState
  ( ComputorState(..)
  , initialState
  , getVariable
  , setVariable
  , getNumVariable
  , setNumVariable
  , getFuncVariable
  , setFuncVariable
  ) where

import Types.TNum
import Types.Value
import Types.ValueFunction
import qualified Data.Map as M

data ComputorState = ComputorState
  { variables :: M.Map String Value
  }

instance Show ComputorState where
  show (ComputorState variables) = showVariables
    where showVariables = "Variables:\n" ++ (show variables)

initialState :: ComputorState
initialState = ComputorState M.empty

getVariable :: String -> ComputorState -> Maybe Value
getVariable key = (M.lookup key) . variables

setVariable :: String -> Value -> ComputorState -> ComputorState
setVariable key value = ComputorState . (M.insert key value) . variables

getNumVariable :: String -> ComputorState -> Maybe TNum
getNumVariable = numTypeCheck . getVariable

setNumVariable :: String -> TNum -> ComputorState -> ComputorState
setNumVariable key value = ComputorState . (M.insert key (VNum value)) . variables

getFuncVariable :: String -> ComputorState -> Maybe ValueFunction
getFuncVariable = funcTypeCheck . getVariable

setFuncVariable :: String -> ValueFunction -> ComputorState -> ComputorState
setFuncVariable key value = ComputorState . (M.insert key (VFunc value)) . variables

numTypeCheck :: Maybe Value -> Maybe TNum
numTypeCheck Just (VNum n) = Just n
numTypeCheck _ = Nothing

funcTypeCheck :: Maybe Value -> Maybe Function
funcTypeCheck Just (VFunc f) = Just f
funcTypeCheck _ = Nothing
