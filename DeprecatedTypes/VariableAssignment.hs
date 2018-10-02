module Types.VariableAssignment
  ( VariableAssignment(..)
  ) where

import Types.Value

data VariableAssignment = VariableAssignment
  { key :: String
  , value :: Value
  }
