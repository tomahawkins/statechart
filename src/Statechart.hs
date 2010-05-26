module Main (main) where

import System.Environment

import Code
import Model
import Rhapsody

main :: IO ()
main = do
  args <- getArgs
  case args of
    name : args@(_:_) -> readFile (last args) >>= genCode name (init args) . parseModel . parseRhapsody
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  statechart - statechart code generation"
  , ""
  , "SYNOPSIS"
  , "  statechart <name> { <include-files> } <rhapsody-file>"
  , ""
  , "DESCRIPTION"
  , "  Compiles a statechart to <name>.c"
  , ""
  ]

