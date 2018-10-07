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

applyF :: Function
applyF = Function {
  args = ["func", "value"],
  appliedArgs = [],
  body = FunctionCall (Variable "func") [VArgAExpr $ Variable "value"]
}

predefinedFunctions :: [(String, Function)]
predefinedFunctions = [("id", idF), ("apply", applyF)]
