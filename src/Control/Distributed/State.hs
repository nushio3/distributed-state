module Control.Distributed.Service.State (
  start
  ) where

import Control.Distributed.Process as CH

start :: Process ()
start = liftIO $ putStrLn "Starting"