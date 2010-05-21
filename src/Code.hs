module Code
  ( genCode
  ) where

import Text.Printf

import Model

genCode :: [Class] -> IO ()
genCode classes = putStrLn $ indent $ unlines
  [ "{"
  , indent "static uint64_t counter = 0;"
  , indent $ concatMap codeStateChart $ concat [ s | Class _ s <- classes ]
  , indent "counter++;"
  , "}"
  ]

formatId :: String -> String -> String
formatId prefix id = prefix ++ map f id
  where
  f '-' = '_'
  f a = a

sId = formatId "s_"
cId = formatId "c_"
tId = formatId "t_"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

codeStateChart :: StateChart -> String
codeStateChart (StateChart name states connectors transitions) = unlines
  [ "// " ++ name
  , "{"
  , indent $ unlines [ printf "static bool %s = false;" (sId $ stateId state) | state <- states ]
  , indent $ unlines [ printf "static uint64_t %s_entryTime = 0;" (sId $ stateId state) | state <- states ]
  , "}"
  , ""
  ]

{-
data Class = Class Name [StateChart] deriving Show

data StateChart = StateChart Name [State] [Connector] [Transition] deriving Show

data State = State
  { stateName         :: Name
  , stateId           :: Id
  , stateParent       :: Maybe Id
-}
