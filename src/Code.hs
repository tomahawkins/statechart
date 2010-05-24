module Code
  ( genCode
  ) where

import Text.Printf

import Model

genCode :: [Class] -> IO ()
genCode classes = putStrLn $ block $ unlines
  [ "static uint64_t stateChartGlobalClock = 0;"
  , ""
  , concatMap codeStateChart $ concat [ s | Class _ s <- classes ]
  , "stateChartGlobalClock++;"
  ]

formatId :: String -> String -> String
formatId prefix id = prefix ++ map f id
  where
  f '-' = '_'
  f a = a

sId = formatId "s_"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

block :: String -> String
block a = "{\n" ++ indent a ++ "}\n"

codeStateChart :: StateChart -> String
codeStateChart (StateChart name states transitions) = seq defaultState $ "// " ++ name ++ "\n" ++ block (unlines
  [ unlines [ printf "static bool %s = false;"           (sId $ stateId state) | state <- states ]
  , unlines [ printf "static uint64_t %s_entryTime = 0;" (sId $ stateId state) | state <- states ]
  , concatMap (codeTransition states) transitions
  ]) ++ "\n"
  where
  root = head [ state | state <- states, stateParent state == Nothing ]
  defaultTransition = case [ t | t <- transitions, transitionSource t == Nothing ] of
    [t] -> t
    []  -> error "default state not specified (1)"
    _   -> error "lower level default transitions not supported"
  defaultState = if Just (transitionId defaultTransition) /= stateDefaultTrans root
    then error "default state not specified (2)"
    else head [ state | state <- states, stateId state == transitionTarget defaultTransition ]

codeTransition :: [State] -> Transition -> String
codeTransition states t = case transitionSource t of
  Nothing     -> "// -> " ++ stateName (idState states target) ++ "\n"
              ++ "if (period20() && !" ++ sId (head b) ++ ") {\n"
              ++ indent (transAction (transitionAction t) ++ concatMap (stateEntry . idState states) b)
              ++ "}\n\n"
    where
    b = hierarchy states target
    
  Just source -> "// " ++ stateName (idState states source) ++ " -> " ++ stateName (idState states target) ++ "\n"
              ++ "if (period20() && " ++ predicate ++ ") {\n"
              ++ indent (concatMap (stateExit . idState states) a ++ transAction (transitionAction t) ++ concatMap (stateEntry . idState states) b)
              ++ "}\n\n"
    where
    (a, b) = changedStates states source target
    predicate = sId target ++ predicateTimeout ++ predicateGuard
    predicateTimeout = case transitionTimeout t of
      Nothing -> ""
      Just i  -> " && (stateChartGlobalClock >= " ++ sId source ++ "_entryTime + (" ++ i ++ "))"
    predicateGuard = case transitionGuard t of
      Nothing -> ""
      Just g  -> " && (" ++ g ++ ")"
  where
  target = transitionTarget t

stateExit :: State -> String
stateExit s = case stateExitAction s of
  Nothing ->              stateUpdate
  Just a  -> a ++ "\n" ++ stateUpdate
  where
  stateUpdate = sId (stateId s) ++ " = false;\n"

stateEntry :: State -> String
stateEntry s = case stateEntryAction s of
  Nothing ->              stateUpdate
  Just a  -> a ++ "\n" ++ stateUpdate
  where
  stateUpdate = sId (stateId s) ++ " = true;\n" ++ sId (stateId s) ++ "_entryTime = stateChartGlobalClock;\n"

transAction :: Maybe String -> String
transAction Nothing = ""
transAction (Just a) = a ++ "\n"

idState :: [State] -> Id -> State
idState states id = head [ s | s <- states, stateId s == id ]

-- (statesExiting, statesEntering)  with proper order.
changedStates :: [State] -> Id -> Id -> ([Id], [Id])
changedStates states a b = (reverse a', b')
  where
  (a', b') = stripCommonPrefix (hierarchy states a) (hierarchy states b)

hierarchy :: [State] -> Id -> [Id]
hierarchy states id = case stateParent $ idState states id of
  Nothing -> [id]
  Just a  -> hierarchy states a ++ [id]

stripCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
stripCommonPrefix (a : as) (b : bs)
  | a == b    = stripCommonPrefix as bs
  | otherwise = (a : as, b : bs)
stripCommonPrefix a b = (a, b)

{-
data Class = Class Name [StateChart] deriving Show

data StateChart = StateChart Name [State] [Connector] [Transition] deriving Show

data State = State
  { stateName         :: Name
  , stateId           :: Id
  , stateParent       :: Maybe Id
  , stateDefaultTrans :: Maybe Id
  , stateEntryAction  :: Maybe Code
  , stateExitAction   :: Maybe Code
  } deriving Show

data Transition = Transition
  { transitionName    :: Name
  , transitionTimeout :: Maybe Int
  , transitionGuard   :: Maybe Code
  , transitionAction  :: Maybe Code
  , transitionSource  :: Maybe Id
  , transitionTarget  :: Id
  } deriving Show

-}
