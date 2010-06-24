module Main (main) where

import System.Environment

import Code
import Model
import Rhapsody

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> help
    Just (o, inc, sbs) -> readFile sbs >>= genCode o inc . parseModel . parseRhapsody

parseArgs :: [String] -> Maybe (String, [String], String)
parseArgs a = case a of
  [a] | head a /= '-' -> Just (takeWhile (/= '.') a ++ ".c", [], a)
  "-o" : file : rest -> do
    (_, inc, sbs) <- parseArgs rest
    Just (file, inc, sbs)
  "-i" : file : rest -> do
    (o, inc, sbs) <- parseArgs rest
    Just (o, file : inc, sbs)
  _ -> Nothing

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  statechart - statechart code generation"
  , ""
  , "SYNOPSIS"
  , "  statechart [ -o <output-file> | { -i <header-file> } ] <rhapsody-package-file>"
  , ""
  , "DESCRIPTION"
  , "  Compiles Rhapsody statecharts to C.  Each Rhapsody class will generate a function"
  , "  that computes the next transition for each of its associated statecharts."
  , ""
  , "OPTIONS"
  , "  -o <name>"
  , "    Output name.  Creates both <name>.c and <name>.h."
  , ""
  , "  -i <file>"
  , "    Header file to include in the generated code."
  , ""
  , "LIMITATIONS"
  , "  TODO, enough to say, a bunch."
  , ""
  ]

