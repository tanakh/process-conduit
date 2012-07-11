import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Process
import System.IO

main :: IO ()
main = runResourceT $ do
  sourceCmd "ls" $$ sinkHandle stdout
  sourceCmd "ls" $$ conduitCmd "sort" =$= sinkHandle stdout
