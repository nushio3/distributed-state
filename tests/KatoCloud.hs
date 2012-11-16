import           Control.Distributed.Process           as DP
import qualified Control.Distributed.Backend.P2P       as P2P
import qualified Control.Distributed.Application.State as DSt

import System.Environmend (getArgs)

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
      "nid" -> getSelfNode >>= liftIO print
      "ls" -> getMap 