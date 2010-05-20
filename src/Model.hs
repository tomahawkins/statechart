module Model
  ( Class         (..)
  , StateChart    (..)
  , State         (..)
  , Connector     (..)
  , ConnectorType (..)
  , parseModel
  ) where

import Data.List

import Rhapsody

data Class = Class String [StateChart] deriving Show

data StateChart = StateChart String [State] [Connector] [Transition] deriving Show

data State = State
  { stateName         :: String
  , stateId           :: String
  , stateParent       :: Maybe String
  , stateDefaultTrans :: Maybe String
  , stateEntryAction  :: Maybe String
  , stateExitAction   :: Maybe String
  , stateAnd          :: Bool
  } deriving Show

data Connector = Connector
  { connectorName
  , connectorId
  , connectorParent :: String
  , connectorType   :: ConnectorType
  } deriving Show

data ConnectorType = Junction | Condition | Fork | Join deriving Show

data Transition = Transition
  { transitionName    :: String
  , transitionTrigger :: Maybe Trigger
  , transitionGuard   :: Maybe String
  , transitionAction  :: Maybe String
  , transitionSource  :: Maybe String
  , transitionTarget  :: String
  } deriving Show

data Trigger = Event String | Timeout Int deriving Show

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
  , transitionTrigger = trigger
  , transitionGuard   = maybeList $ body "_itsGuard"   label
  , transitionAction  = maybeList $ body "_itsAction"  label
  , transitionSource  = maybeList $ ident "_itsSource" a
  , transitionTarget  = ident "_itsTarget" a
  }
  where
  label = head $ records "_itsLabel" a
  trigger = case body "_itsTrigger" label of
    a | isPrefixOf "tm(" a -> Just $ Timeout $ read $ drop 3 $ init a
      | null a             -> Nothing
      | otherwise          -> Just $ Event a

maybeList :: [a] -> Maybe [a]
maybeList a = if null a then Nothing else Just a

