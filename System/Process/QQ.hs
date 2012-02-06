{-# LANGUAGE TemplateHaskell #-}

module System.Process.QQ (
  cmd,
  scmd,
  ccmd,
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text.Lazy as LT
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

scmd :: QuasiQuoter
scmd = def { quoteExp = \str -> [| sourceCmd $(quoteExp lt str) |] }

ccmd :: QuasiQuoter
ccmd = def { quoteExp = \str -> [| conduitCmd $(quoteExp lt str) |] }
