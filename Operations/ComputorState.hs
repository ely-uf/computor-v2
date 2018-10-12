module Operations.ComputorState
  ( initialState
  , getVariable
  , setVariable
  , getNumVariable
  , setNumVariable
  , getFuncVariable
  , setFuncVariable
  ) where

import Types
import PredefinedFunctions
import Data.Char (toLower)
import qualified Data.Map as M

lowerCase :: String -> String
lowerCase = map toLower

initialState :: ComputorState
initialState = addPredefinedFunctions $ ComputorState M.empty

addPredefinedFunctions :: ComputorState -> ComputorState
addPredefinedFunctions = injectFunctions predefinedFunctions

injectFunctions :: [(String, Function)] -> ComputorState -> ComputorState
injectFunctions [] st = st
injectFunctions ((name, fn):xs) st = injectFunctions xs (setFuncVariable name fn st)

getVariable :: String -> ComputorState -> Maybe Value
getVariable key = (M.lookup $ lowerCase key) . variables

setVariable :: String -> Value -> ComputorState -> ComputorState
setVariable key value = ComputorState . (M.insert (lowerCase key) value) . variables

getNumVariable :: String -> ComputorState -> Maybe TNum
getNumVariable = ((.) numTypeCheck) . getVariable

setNumVariable :: String -> TNum -> ComputorState -> ComputorState
setNumVariable key value = ComputorState . (M.insert key (VNum value)) . variables

getFuncVariable :: String -> ComputorState -> Maybe Function
getFuncVariable = ((.) funcTypeCheck) . getVariable

setFuncVariable :: String -> Function -> ComputorState -> ComputorState
setFuncVariable key value = ComputorState . (M.insert (lowerCase key) (VFunc value)) . variables

numTypeCheck :: Maybe Value -> Maybe TNum
numTypeCheck (Just (VNum n)) = Just n
numTypeCheck _ = Nothing

funcTypeCheck :: Maybe Value -> Maybe Function
funcTypeCheck (Just (VFunc f)) = Just f
funcTypeCheck _ = Nothing
