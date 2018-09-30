module Types.ComputorState
  ( ComputorState(..)
  , initialState
  , getNumVariable
  , setNumVariable
  ) where

import Types.TNum
import qualified Data.Map as M

data ComputorState = ComputorState
  { variables :: M.Map String TNum
  }

instance Show ComputorState where
  show (ComputorState variables) = showVariables
    where showVariables = "Variables:\n" ++ (show variables)

initialState :: ComputorState
initialState = ComputorState M.empty

getNumVariable :: String -> ComputorState -> Maybe TNum
getNumVariable key = (M.lookup key) . variables

setNumVariable :: String -> TNum -> ComputorState -> ComputorState
setNumVariable key value = ComputorState . (M.insert key value) . variables
