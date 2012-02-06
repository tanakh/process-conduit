{-# LANGUAGE QuasiQuotes #-}

import System.Process.QQ
import System.Environment

main :: IO ()
main = do
  [url] <- getArgs
  print =<< [cmd|curl #{url}|]
