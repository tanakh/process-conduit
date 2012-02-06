import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Process
import System.IO

main :: IO ()
main = C.runResourceT $ do
  sourceCmd "ls" C.$= conduitCmd "sort" C.$$ CB.sinkHandle stdout
