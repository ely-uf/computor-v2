module Main where

import Types
import ComputorCommand

import Text.Megaparsec
import Parsers.GenericParsers

import Operations.ComputorState

import Control.Monad.State.Strict
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.IO (hFlush, hIsEOF, stdout, stdin)
import System.Exit (exitSuccess)

computorParser :: Parser ComputorCommand
computorParser = between sc eof parseComputorCommand

{-
interactiveConsole :: ComputorStateT IO ()
interactiveConsole = do
  i <- liftIO promptInput
  case parse computorParser "" i of
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right CNothing -> interactiveConsole
    Right cmd -> executeCommand cmd
  interactiveConsole
  where
    promptInput = do
      putStr "$> "
      hFlush stdout
      eof <- hIsEOF stdin
      if eof then
        exitSuccess
      else
        getLine
-}

inputTSettings = Settings {
  complete = noCompletion,
  historyFile = Nothing,
  autoAddHistory = True
}

interactiveConsole' :: InputT (ComputorStateT IO) ()
interactiveConsole' = do
  input <- getInputLine "$> "
  case input of
    Nothing -> lift . liftIO $ exitSuccess
    Just i -> do
                case parse computorParser "" i of
                  Left err -> outputStr . errorBundlePretty $ err
                  Right CNothing -> interactiveConsole'
                  Right cmd -> lift $ executeCommand cmd
                interactiveConsole'

main :: IO ()
main = evalStateT (runInputT inputTSettings inputTRoutine) initialState
  where
    inputTRoutine = withInterrupt . (handle interruptHandler) $ interactiveConsole'
    interruptHandler Interrupt = lift . liftIO $ exitSuccess
