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

andF :: Function
andF = Function {
  args = ["b1", "b2"],
  appliedArgs = [],
  body = FunctionCall (Variable "b1") $ VArgAExpr <$> [ Variable "b2", Variable "b1" ]
}

orF :: Function
orF = Function {
  args = ["b1", "b2"],
  appliedArgs = [],
  body = FunctionCall (Variable "b1") $ VArgAExpr <$> [ Variable "b1", Variable "b2" ]
}

notF :: Function
notF = Function {
  args = ["b"],
  appliedArgs = [],
  body = FunctionCall (Variable "b") $ VArgAExpr <$> [ Variable "false", Variable "true" ]
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
  , ("and", andF)
  , ("or", orF)
  , ("not", notF)
  ]
