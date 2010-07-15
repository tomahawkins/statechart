module Model
  ( Class         (..)
  , Attribute     (..)
  , StateChart    (..)
  , Init          (..)
  , Transition    (..)
  , Name
  , Id
  , Code
  , parseModel
  ) where

import Data.List
import Data.Maybe

import Rhapsody

type Name = String
type Id   = String
type Code = String

data Class = Class Name [Attribute] [StateChart] deriving Show

data Attribute = Attribute
  { attrName :: Name
  , attrType :: String
  , attrInit :: Maybe String
  } deriving Show

data StateChart' = StateChart' Name [State] [Transition'] deriving Show

data State = State
  { stateName         :: Name
  , stateId           :: Id
  , stateParent       :: Maybe Id
  , stateDefaultTrans :: Maybe Id
  , stateEntryAction  :: Maybe Code
  , stateExitAction   :: Maybe Code
  } deriving Show

data Transition' = Transition'
  { transition'Name    :: Name
  , transition'Id      :: Id
  , transition'Timeout :: Maybe Code
  , transition'Guard   :: Maybe Code
  , transition'Action  :: Maybe Code
  , transition'Source  :: Maybe Id
  , transition'Target  :: Id
  } deriving Show

data StateChart = StateChart Name [(Id, Name)] Init [Transition] deriving Show

data Init = Init
  { initAction :: [Code]
  , initTarget :: [Id]
  } deriving Show

data Transition = Transition
  { transitionName    :: (Name, Name)
  , transitionSource  :: [Id]
  , transitionTimeout :: [(Id, Code)]
  , transitionGuard   :: [Code]
  , transitionAction  :: [Code]
  , transitionTarget  :: [Id]
  } deriving Show

formatStateChart :: StateChart' -> StateChart
formatStateChart (StateChart' name states transitions) = if not defaultTransitionsCorrect
  then error "invalid transition:  default transitions must only point to direct sub-states"
  else if not transitionsCorrect
    then error "invalid transition:  transitions must not cross state boundries"
    else if not allParentStatesHaveDefaultTrans
      then error "invalid transition:  all states containing sub-states must have a default transition"
      else if not $ null redundentStateNames
        then error $ "invalid states:  state names are not unique: " ++ show redundentStateNames
        else StateChart name idsNames init transitions'
  where
  defaultTransitionsCorrect = and [ elem (transition'Target t) (children (stateId s)) | s <- states, t <- transitions, Just (transition'Id t) == stateDefaultTrans s ]
  transitionsCorrect = and [ parent (fromJust $ transition'Source t) == parent (transition'Target t) | t <- transitions, isJust $ transition'Source t ]
  allParentStatesHaveDefaultTrans = and [ stateDefaultTrans s /= Nothing | s <- states, not $ null $ children $ stateId s ] 
  redundentStateNames = foldr delete names (nub names)
  names = [ stateName s | s <- states ]
  idsNames = [ (id, stateName $ state id) | id <- nub $ concat [ transitionSource t ++ transitionTarget t | t <- transitions' ] ++ initTarget init ]

  transitions' = [ appendTransitions (appendTransitions pre $ convertTrans t) $ entryTransition $ transition'Target t | t <- transitions, isJust $ transition'Source t, pre <- exitTransitions $ fromJust $ transition'Source t ]

  initTrans = entryTransition $ head [ stateId s | s <- states, stateParent s == Nothing ]
  init = Init
    { initAction = transitionAction initTrans
    , initTarget = transitionTarget initTrans
    }

  state :: Id -> State
  state id = case [ s | s <- states, stateId s == id ] of
    [a] -> a
    _ -> error $ "state not found: " ++ id
    
  parent :: Id -> Id
  parent = fromJust . stateParent . state
  
  children :: Id -> [Id]
  children id = [ stateId s | s <- states, stateParent s == Just id ]

  trans :: Id -> Transition'
  trans id = case [ t | t <- transitions, transition'Id t == id ] of
    [a] -> a
    _ -> error $ "transition not found: " ++ id

  -- Elaborated entry transition given a state id.
  entryTransition :: Id -> Transition
  entryTransition id = case stateDefaultTrans $ state id of
    Nothing  -> entry
    Just tId -> appendTransitions (appendTransitions entry $ convertTrans t) $ entryTransition $ transition'Target t
      where
      t = trans tId
    where
    entry = Transition
      { transitionName    = ("", "")
      , transitionSource  = []
      , transitionTimeout = []
      , transitionGuard   = []
      , transitionAction  = maybeToList $ stateEntryAction $ state id
      , transitionTarget  = [id]
      }

  -- Elaborated exit transitions given a state id.
  exitTransitions :: Id -> [Transition]
  exitTransitions id = if null subExits then [exit] else [ appendTransitions a exit | a <- subExits ]
    where
    exit = Transition
      { transitionName    = ("", "")
      , transitionSource  = [id]
      , transitionTimeout = []
      , transitionGuard   = []
      , transitionAction  = maybeToList $ stateExitAction $ state id
      , transitionTarget  = []
      }
    subExits = concat [ exitTransitions a | a <- children id ]


  convertTrans :: Transition' -> Transition
  convertTrans t = Transition
    { transitionName = case transition'Source t of
        Nothing -> ("", "")
        Just src -> (stateName $ state $ src, stateName $ state $ transition'Target t)
    , transitionSource  = maybeToList $ transition'Source t
    , transitionTimeout = case (transition'Source t, transition'Timeout t) of
        (Just id, Just code) -> [(id, code)]
        _ -> []
    , transitionGuard   = maybeToList $ transition'Guard  t
    , transitionAction  = maybeToList $ transition'Action t
    , transitionTarget  = [transition'Target t]
    }
  
appendTransitions :: Transition -> Transition -> Transition
appendTransitions a b = Transition
  { transitionName    = (fst (transitionName   a) ++ fst (transitionName    b), snd (transitionName   a) ++ snd (transitionName    b))
  , transitionSource  = nub $ transitionSource  a ++ transitionSource  b  --XXX Will b every have sources?
  , transitionTimeout = transitionTimeout a ++ transitionTimeout b
  , transitionGuard   = transitionGuard   a ++ transitionGuard   b
  , transitionAction  = transitionAction  a ++ transitionAction  b
  , transitionTarget  = nub $ transitionTarget  a ++ transitionTarget  b
  }

parseModel :: Record -> [Class]
parseModel file = map parseClass $ containerRecords "Classes" file

parseClass :: Record -> Class
parseClass a = Class (head $ values "_name" a) (map parseAttribute $ containerRecords "Attrs" a)  (map (formatStateChart . parseStateChart) $ containerRecords "StateCharts" a)

parseAttribute :: Record -> Attribute
parseAttribute a = Attribute
  { attrName = value "_name" a
  , attrType = value "_name" $ head $ records "_typeOf" a
  , attrInit = case containerRecords "ValueSpecifications" a of
      []  -> Nothing
      [a] -> Just $ value "_value" a
      _   -> error "too many value specifications"
  }

parseStateChart :: Record -> StateChart'
parseStateChart a = if not $ null $ containerRecords "Connectors" a then error "connectors not supported" else StateChart'
  (value "_name" a)
  (map parseState      $ containerRecords "States"      a)
  (map parseTransition $ containerRecords "Transitions" a)

parseState :: Record -> State
parseState a = if values "_stateType" a == ["And"] then error $ "invalid state: AND states not supported: " ++ value "_name" a else State
  { stateName         = if null (value "_name" a) then "root" else value "_name" a
  , stateId           = ident "_id"           a
  , stateParent       = maybeList $ ident "_parent"       a
  , stateDefaultTrans = maybeList $ ident "_defaultTrans" a
  , stateEntryAction  = maybeList $ body  "_entryAction"  a
  , stateExitAction   = maybeList $ body  "_exitAction"   a
  } 

parseTransition :: Record -> Transition'
parseTransition a = Transition'
  { transition'Name    = value "_name" a
  , transition'Id      = ident "_id"   a
  , transition'Timeout = timeout
  , transition'Guard   = maybeList $ if guard == "else" then error "else guards not supported" else guard
  , transition'Action  = maybeList $ body "_itsAction"  label
  , transition'Source  = maybeList $ ident "_itsSource" a
  , transition'Target  = ident "_itsTarget" a
  }
  where
  label = head $ records "_itsLabel" a
  timeout = case body "_itsTrigger" label of
    a | isPrefixOf "tm(" a -> Just $ drop 3 $ init a
      | null a             -> Nothing
      | otherwise          -> error $ "not supported non tm() events: " ++ a
  guard = body "_itsGuard" label

maybeList :: [a] -> Maybe [a]
maybeList a = if null a then Nothing else Just a

