module Main (main) where

import System.Environment

import Code
import Model
import Rhapsody

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= genCode . parseModel . parseRhapsody
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  statechart - statechart code generation"
  , ""
  , "SYNOPSIS"
  , "  statechart rhapsody-file"
  , ""
  , "DESCRIPTION"
  , "  Compiles a statechart, outputs C to stdout."
  , ""
  ]

