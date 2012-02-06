{-# LANGUAGE TemplateHaskell #-}

module System.Process.QQ (
  cmd,
  lcmd,
  scmd,
  ccmd,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.Lazy as CL
import qualified Data.Conduit.List as CL
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Shakespeare.Text

import Data.Conduit.Process

def :: QuasiQuoter
def = QuasiQuoter
  { quoteExp  = undefined
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

cmd :: QuasiQuoter
cmd = def { quoteExp = \str -> [|
  BL.fromChunks <$> C.runResourceT (sourceCmd (LT.unpack $(quoteExp lt str)) C.$$ CL.consume)
|] }

lcmd :: QuasiQuoter
lcmd = def { quoteExp = \str -> [|
  BL.fromChunks <$> (CL.lazyConsume $ sourceCmd $ LT.unpack $(quoteExp lt str))
|] }

scmd :: QuasiQuoter
scmd = def { quoteExp = \str -> [| sourceCmd $(quoteExp lt str) |] }

ccmd :: QuasiQuoter
ccmd = def { quoteExp = \str -> [| conduitCmd $(quoteExp lt str) |] }
