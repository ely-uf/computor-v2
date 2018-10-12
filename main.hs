module Main where

import Types
import ComputorCommand

import Text.Megaparsec
import Parsers.GenericParsers

import Operations.ComputorState

import Control.Monad.State.Strict
import System.IO (hFlush, hIsEOF, stdout, stdin)
import System.Exit (exitSuccess)

computorParser :: Parser ComputorCommand
computorParser = between sc eof parseComputorCommand

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

main :: IO ()
main = evalStateT interactiveConsole initialState
