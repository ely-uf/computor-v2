module PredefinedFunctions
  ( predefinedFunctions
  ) where

import Types

idF :: Function
idF = Function {
  args = ["a"],
  appliedArgs = [],
  body = (Variable "a")
}

predefinedFunctions :: [(String, Function)]
predefinedFunctions = [("id", idF)]
