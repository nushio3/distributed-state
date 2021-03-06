{-# LANGUAGE DeriveDataTypeable, RankNTypes, RecordWildCards #-}

module Control.Distributed.Application.State (
  start, Interface(..)
  ) where

import           Control.Applicative
import           Control.Distributed.Process as DP
import           Control.Distributed.Process.Serializable (Serializable(..))
import           Control.Concurrent.STM.TVar as STM
import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Control.Monad.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.Binary as Bin
import           Data.Data

salt :: String
salt = "Control.Distributed.Application.State."

data Interface a =
  Interface
  { get    :: Process a
  , put    :: a -> Process ()
  , modify :: (a->a) -> Process ()
  , getMap :: Process (Map.Map NodeId a)
  }

data Internal a =
  Internal
  { myState  :: TVar a
  , stateMap :: TVar (Map.Map NodeId a)
  }

data MsgUpdate a
  = MsgUpdate NodeId a
    deriving (Eq, Show, Typeable)

data MsgQuery
  = MsgQuery ProcessId
    deriving (Eq, Show, Typeable)

instance (Bin.Binary a) => Bin.Binary (MsgUpdate a) where
  put (MsgUpdate nid x) = Bin.put nid >> Bin.put x
  get = MsgUpdate <$> Bin.get <*> Bin.get

instance Bin.Binary MsgQuery where
  put (MsgQuery pid) = Bin.put pid
  get = MsgQuery <$> Bin.get

start :: (Serializable a)
  => String
  -> a
  -> (forall b.Serializable b => String -> b -> Process ())
  -> Process (Interface a)
start label initialState broadcast = do
  myState0 <- liftIO $ newTVarIO initialState
  stateMap0 <- liftIO $ newTVarIO $ Map.empty
  let ourLabel = salt ++ label
      internal = Internal myState0 stateMap0
  controllerPid <- spawnLocal $ do
    controllerPid <- getSelfPid
    register ourLabel controllerPid
    forever $ receiveWait
      [ match $ onMsgQuery  internal
      , match $ onMsgUpdate internal
      , match $ onMonitor   internal]

  link controllerPid
  let broadcastNewState1 Internal{..} = do
        nid <- getSelfNode
        x <- liftIO $ atomically $ readTVar myState
        broadcast ourLabel $ MsgUpdate nid x
      broadcastNewState = broadcastNewState1 internal

      put1 Internal{..} newState = do
        nid <- getSelfNode
        liftIO $ atomically $ writeTVar myState newState
        broadcastNewState
      get1 Internal{..} = liftIO $ atomically $ readTVar myState
      modify1 Internal{..} f = do
        liftIO $ atomically $ modifyTVar myState f
        x <- liftIO $ atomically $ readTVar myState
        nid <- getSelfNode
        broadcastNewState
      getMap1 Internal{..} =
        liftIO $ atomically $ readTVar stateMap

      notifier = do
        broadcast ourLabel $ MsgQuery controllerPid
        broadcastNewState

  liftIO $ threadDelay 1000000
  spawnLocal $ do
    notifier >> (liftIO $ threadDelay 2000000)
    notifier >> (liftIO $ threadDelay 4000000)
    notifier >> (liftIO $ threadDelay 8000000)

  return $ Interface (get1 internal) (put1 internal) (modify1 internal) (getMap1 internal)

onMsgQuery :: (Serializable a) => Internal a -> MsgQuery -> Process ()
onMsgQuery Internal{..} msg@(MsgQuery pid) = do
  nid <- getSelfNode
  x <- liftIO $ atomically $ readTVar $ myState
  send pid $ MsgUpdate nid x

onMsgUpdate :: Internal a -> MsgUpdate a -> Process ()
onMsgUpdate Internal{..} (MsgUpdate nid x) = do
  monitorNode nid
  liftIO $ atomically $ modifyTVar stateMap (Map.insert nid x)

onMonitor :: Internal a ->  NodeMonitorNotification -> Process ()
onMonitor Internal{..} (NodeMonitorNotification _ nid _) = do
  liftIO $ atomically $ modifyTVar stateMap (Map.delete nid)