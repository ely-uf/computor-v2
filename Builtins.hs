module Builtins
  ( builtinNames
  , executeBuiltin
  ) where

import Types
import Data.List (find)
import Data.Map (toList)
import Control.Monad.State.Strict (get, put, liftIO)

builtinNames :: [String]
builtinNames = map fst builtinCommands

builtinCommands :: [(String, ComputorStateT IO ())]
builtinCommands =
  [ ("dump", dumpB)
  , ("help", helpB)
  ]

dumpB :: ComputorStateT IO ()
dumpB = do
  st <- get
  liftIO $ mapM_ (printVariable) . toList . variables $ st
  where
    printVariable :: (Show a, Show b) => (a, b) -> IO ()
    printVariable (key, val) = putStrLn $ show key ++ " = " ++ show val

helpB :: ComputorStateT IO ()
helpB = liftIO $ do
  putStrLn "============ Computor V2 ============"
  putStrLn ""
  putStrLn "Basic types:"
  putStrLn "\tIntegral -> { 0, 11, 123, -256 }"
  putStrLn "\tRational -> { 1.1, 0.2, -0.771, 0.0 }"
  putStrLn "\tComplex  -> { 2.0 * i, 3 * i, 1 + 4 * i }"
  putStrLn "\tMatrix   -> { [[1, 2];[3, 4]], [[1]], [] }"
  putStrLn "\tFunction -> { () -> a, (a, b) -> a + b, (fn, val) -> fn(val) }"
  putStrLn ""
  putStrLn "Variable assignment:"
  putStrLn "\ta = 2"
  putStrLn "\ta = a + 2"
  putStrLn "\ta = ((val) -> val + 3)(a)"
  putStrLn "\ta = add(a, 3)"
  putStrLn ""
  putStrLn "Function declaration:"
  putStrLn "\tfunc(a, b) = a + b"
  putStrLn "\tfunc = (a, b) -> a + b"
  putStrLn ""
  putStrLn "Higher order functions:"
  putStrLn "\tapply = (func, val) -> func(val)"
  putStrLn "\tadd2(a) = a + 2"
  putStrLn "\tapply(add2, 3) # 5"
  putStrLn "\tapply((a) -> a ^ 2, 3) # 9"
  putStrLn ""
  putStrLn "Currying:"
  putStrLn "\tadd(a, b) = a + b"
  putStrLn "\tapply(add(2), 3) # 5"
  putStrLn "\tadd7 = add(7)"
  putStrLn "\tadd7(3) # 10"
  putStrLn ""
  putStrLn "Builtins:"
  putStrLn "\t@help -> Display help."
  putStrLn "\t@dump -> Dump variables."
  putStrLn ""

executeBuiltin :: String -> ComputorStateT IO ()
executeBuiltin name = case find ((== name).fst) builtinCommands of
                        Just (_, builtin) -> builtin
                        Nothing           -> return ()
