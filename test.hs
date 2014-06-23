{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.Conduit.Process
import System.Process.QQ

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "process conduit" $ do
    it "get process's output" $ do
      r <- runResourceT $ sourceCmd "echo abc def" $$ CB.take (10^9)
      L.words r `shouldBe` ["abc", "def"]

    it "get process's large binary output" $ do
      let binData = L.pack $ replicate (1 * 1024 * 1024) '\xff'
      r <- runResourceT $
        CB.sourceLbs binData
        $$ conduitCmd "cat"
        =$ CB.sinkLbs
      L.length (L.takeWhile (== '\xff') r) `shouldBe` L.length binData

    it "act as conduit" $ do
      r <- runResourceT $
        sourceProcess (proc "echo" ["zxc\nasd\nqwe"])
        $$ conduitCmd "sort"
        =$ CB.take (10^9)
      L.words r `shouldBe` ["asd", "qwe", "zxc"]

  describe "quasiquoter" $ do
    it "get process's output" $ do
      r <- [cmd|echo abc def|]
      L.words r `shouldBe` ["abc", "def"]

    it "act as conduit" $ do
      r <- runResourceT $
        sourceProcess (proc "echo" ["zxc\nasd\nqwe"])
        $$ [ccmd|sort|]
        =$ CB.take (10^9)
      L.words r `shouldBe` ["asd", "qwe", "zxc"]
