module Rhapsody
  ( Record (..)
  , Field  (..)
  , parseRhapsody
  , records
  , values
  , value
  , containerRecords
  , ident
  , body
  ) where

import Text.ParserCombinators.Poly.Plain

data Record = Record String [Field]  deriving (Show, Eq)
data Field
  = Value       String [String]
  | RecordNamed String [Record]
  | RecordAnon          Record
  deriving (Show, Eq)

-- | Given a record and a field name, returns a subfield of records.  Null if not found.
records :: String -> Record -> [Record]
records name (Record _ fields) = concat [ recs | RecordNamed n recs <- fields, n == name ]

-- | Given a record and a field name, returns a subfield of values.  Null if not found.
values :: String -> Record -> [String]
values name (Record _ fields) = concat [ vals | Value n vals <- fields, n == name ]

-- | Returns a field with a single value, or an empty string if the field does not exist.
value :: String -> Record -> String
value name r = case values name r of
  [] -> ""
  a : _ -> a

-- | Returns a list of records packaged in an IRPYRawContainer.
containerRecords :: String -> Record -> [Record]
containerRecords name rec = case records name rec of
  [] -> []
  a : _ -> records "value" a
 
-- | A unique identifier field.
ident :: String -> Record -> String
ident name a = case values name a of
  [_, a] -> a
  _ -> ""

-- | Extract the _body string from triggers, guards, and actions.
body :: String -> Record -> String
body name rec = case records name rec of
  [] -> ""
  a : _ -> value "_body" a



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

parseRhapsody :: String -> Record
parseRhapsody a = case runParser (record `discard` eof) $ lexer a of
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
  , do { tok Dash; name <- string; tok Eq; r <- many record; return $ RecordNamed name r }
  , do { r <- record; return $ RecordAnon r }
  ]

