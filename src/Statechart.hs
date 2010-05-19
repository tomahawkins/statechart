module Main (main) where

import System.Environment
import Text.ParserCombinators.Poly.Plain

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      f <- readFile file
      print $ parseRecord f
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

data Record = Record String [Field]  deriving (Show, Eq)
data Field
  = Value       String [String]
  | RecordNamed String Record
  | RecordAnon         Record
  deriving (Show, Eq)

-- Lexing.

data Token
  = Eq
  | Dash
  | SemiColon
  | BraceLeft
  | BraceRight
  | String String
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer = tokens . unlines . tail . lines . decomment
  where

  tokens :: String -> [Token]
  tokens "" = []
  tokens a = case a of
    a : b | elem a " \r\n\t" -> tokens b
    '{' : b -> BraceLeft  : tokens b
    '}' : b -> BraceRight : tokens b
    '=' : b -> Eq         : tokens b
    '-' : ' ' : b -> Dash       : tokens b
    ';' : b -> SemiColon  : tokens b
    '"' : b -> String str : tokens rest where (str, rest) = string b
    '#' : b -> tokens $ dropWhile (/= '\n') b
    a -> String (takeWhile (flip notElem " \r\n\t;") a) : tokens (dropWhile (flip notElem " \r\n\t;") a)

  string :: String -> (String, String)
  string a = case a of
    [] -> error "Ending string quote not found."
    '"'  : b     -> ("", b)
    '\\' : a : b -> (a : a', b') where (a', b') = string b
    a : b        -> (a : a', b') where (a', b') = string b

decomment :: String -> String
decomment = decomment' 0
  where
  decomment' :: Int -> String -> String
  decomment' n "" | n == 0    = ""
                  | otherwise = error "reached end of file without closing comment"
  decomment' n ('(':'*':a) = "  " ++ decomment' (n + 1) a
  decomment' n ('*':')':a) | n > 0  = "  " ++ decomment' (n - 1) a
                           | otherwise = error "unexpected closing comment"
  decomment' n (a:b) = (if n > 0 && a /= '\n' then ' ' else a) : decomment' n b



-- Parsing.

parseRecord :: String -> Record
parseRecord a = case runParser (record `discard` eof) $ lexer a of
  (Left msg, remaining) -> error $ msg ++ "\nremaining tokens: " ++ show (take 30 $ remaining) ++ " ..."
  (Right a, []) -> a
  (Right _, remaining) -> error $ "parsed, but with remaining tokens: " ++ show remaining

type Rhap a = Parser Token a

tok :: Token -> Rhap ()
tok a = satisfy (== a) >> return ()

string :: Rhap String
string = do
  a <- satisfy (\ a -> case a of { String _ -> True; _ -> False })
  case a of
    String s -> return s
    _ -> undefined

record :: Rhap Record
record = do { tok BraceLeft; name <- string; fields <- many field; tok BraceRight; return $ Record name fields }

field :: Rhap Field
field = oneOf
  [ do { tok Dash; name <- string; tok Eq; v <- many string; tok SemiColon; return $ Value name v }
  , do { tok Dash; name <- string; tok Eq; r <- record; return $ RecordNamed name r }
  , do { r <- record; return $ RecordAnon r }
  ]

