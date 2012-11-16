import           Control.Distributed.Process           as DP
import qualified Control.Distributed.Backend.P2P       as P2P
import qualified Control.Distributed.Application.State as DSt

import           Control.Monad (forever)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs     

  case args of
    host:port:seeds -> P2P.bootstrap host port (map P2P.makeNodeId seeds) mainProcess

mainProcess :: Process ()
mainProcess = do
  api <- DSt.start "counter" (0::Int) P2P.nsendPeers          
  forever $ do
    cmd <- liftIO getLine
    case words cmd of
      "whoami":_   ->  do getSelfNode >>= (liftIO . print)
                          getSelfPid >>= (liftIO . print)
      "ls":_    -> DSt.getMap api >>=  (liftIO . print)
      "my":_    -> DSt.get api >>=  (liftIO . print)
      "add":x:_ -> DSt.modify api ((read x)+)
      _ -> liftIO $ putStrLn "unknown command."