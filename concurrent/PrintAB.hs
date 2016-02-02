-- PrintAB.hs
-- demonstration of thread interleaving

import Control.Concurrent
import Control.Monad
import System.IO

main = do
  -- hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')
