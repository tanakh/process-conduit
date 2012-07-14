{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.Conduit.Process
import System.Process.QQ

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Test.Hspec
import Test.HUnit

main :: IO ()
main = hspec $ do
  describe "process conduit" $ do
    it "get process's output" $ do
      r <- runResourceT $ sourceCmd "echo abc def" $$ CB.take (10^9)
      L.words r @?= ["abc", "def"]

    it "act as conduit" $ do
      r <- runResourceT $
        sourceProcess (proc "echo" ["zxc\nasd\nqwe"])
        $$ conduitCmd "sort"
        =$ CB.take (10^9)
      L.words r @?= ["asd", "qwe", "zxc"]

  describe "quasiquoter" $ do
    it "get process's output" $ do
      r <- [cmd|echo abc def|]
      L.words r @?= ["abc", "def"]

    it "act as conduit" $ do
      r <- runResourceT $
        sourceProcess (proc "echo" ["zxc\nasd\nqwe"])
        $$ [ccmd|sort|]
        =$ CB.take (10^9)
      L.words r @?= ["asd", "qwe", "zxc"]
