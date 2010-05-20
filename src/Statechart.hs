module Main (main) where

import System.Environment

import Model
import Rhapsody

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      f <- readFile file
      print $ parseModel $ parseRhapsody f
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
  , ""
  ]

