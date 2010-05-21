module Model
  ( Class         (..)
  , StateChart    (..)
  , State         (..)
  , Connector     (..)
  , ConnectorType (..)
  , Name
  , Id
  , Code
  , parseModel
  ) where

import Data.List

import Rhapsody

type Name = String
type Id   = String
type Code = String

data Class = Class Name [StateChart] deriving Show

data StateChart = StateChart Name [State] [Connector] [Transition] deriving Show

data State = State
  { stateName         :: Name
  , stateId           :: Id
  , stateParent       :: Maybe Id
  , stateDefaultTrans :: Maybe Id
  , stateEntryAction  :: Maybe Code
  , stateExitAction   :: Maybe Code
  , stateAnd          :: Bool
  } deriving Show

data Connector = Connector
  { connectorName   :: Name
  , connectorId     :: Id
  , connectorParent :: Id
  , connectorType   :: ConnectorType
  } deriving Show

data ConnectorType = Junction | Condition | Fork | Join deriving Show

data Transition = Transition
  { transitionName    :: Name
  , transitionTimeout :: Maybe Int
  , transitionGuard   :: Maybe Code
  , transitionAction  :: Maybe Code
  , transitionSource  :: Maybe Id
  , transitionTarget  :: Id
  } deriving Show

parseModel :: Record -> [Class]
parseModel file = map parseClass $ containerRecords "Classes" file

parseClass :: Record -> Class
parseClass a = Class (head $ values "_name" a) $ map parseStateChart $ containerRecords "StateCharts" a

parseStateChart :: Record -> StateChart
parseStateChart a = StateChart
  (value "_name" a)
  (map parseState      $ containerRecords "States"      a)
  (map parseConnector  $ containerRecords "Connectors"  a)
  (map parseTransition $ containerRecords "Transitions" a)

parseState :: Record -> State
parseState a = State
  { stateName         = value "_name"         a
  , stateId           = ident "_id"           a
  , stateParent       = maybeList $ ident "_parent"       a
  , stateDefaultTrans = maybeList $ ident "_defaultTrans" a
  , stateEntryAction  = maybeList $ body  "_entryAction"  a
  , stateExitAction   = maybeList $ body  "_exitAction"   a
  , stateAnd          = case values "_stateType" a of { ["And"] -> True; _ -> False }
  }

parseConnector :: Record -> Connector
parseConnector a = Connector
  { connectorName   = value "_name"   a
  , connectorId     = ident "_id"     a
  , connectorParent = ident "_parent" a
  , connectorType   = case value "_connectorType" a of
      "Junction"  -> Junction
      "Condition" -> Condition
      "Fork"      -> Fork
      "Join"      -> Join
      a           -> error $ "unknown connector type: " ++ a
  }

parseTransition :: Record -> Transition
parseTransition a = Transition
  { transitionName    = value "_name" a
  , transitionTimeout = timeout
  , transitionGuard   = maybeList $ body "_itsGuard"   label
  , transitionAction  = maybeList $ body "_itsAction"  label
  , transitionSource  = maybeList $ ident "_itsSource" a
  , transitionTarget  = ident "_itsTarget" a
  }
  where
  label = head $ records "_itsLabel" a
  timeout = case body "_itsTrigger" label of
    a | isPrefixOf "tm(" a -> Just $ read $ drop 3 $ init a
      | null a             -> Nothing
      | otherwise          -> error $ "not supported non tm() events: " ++ a

maybeList :: [a] -> Maybe [a]
maybeList a = if null a then Nothing else Just a

