module Control.Distributed.Application.State (
  start
  ) where

import Control.Distributed.Process as CH

start :: Process ()
start = liftIO $ putStrLn "Starting"