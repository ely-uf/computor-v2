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

trueF :: Function
trueF = Function {
  args = ["true", "false"],
  appliedArgs = [],
  body = Variable "true"
}

falseF :: Function
falseF = Function {
  args = ["true", "false"],
  appliedArgs = [],
  body = Variable "false"
}

ifF :: Function
ifF = Function {
  args = ["cond", "then", "else"],
  appliedArgs = [],
  body = FunctionCall (Variable "cond") [VArgAExpr $ Variable "then", VArgAExpr $ Variable "else"]
}

predefinedFunctions :: [(String, Function)]
predefinedFunctions =
  [ ("id", idF)
  , ("apply", applyF)
  , ("true", trueF)
  , ("false", falseF)
  , ("if", ifF)
  ]
