{-# LANGUAGE TemplateHaskell #-}

module System.Process.QQ (
  -- * Quasi Quoters
  cmd,
  scmd,
  ccmd,
  ) where

import Control.Applicative
import Control.Monad.Trans.Resource as R
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

-- | Command result of (Lazy) ByteString.
cmd :: QuasiQuoter
cmd = def { quoteExp = \str -> [|
  BL.fromChunks <$> R.runResourceT (sourceCmd (LT.unpack $(quoteExp lt str)) C.$$ CL.consume)
|] }

-- | Source of shell command
scmd :: QuasiQuoter
scmd = def { quoteExp = \str -> [| sourceCmd (LT.unpack $(quoteExp lt str)) |] }

-- | Conduit of shell command
ccmd :: QuasiQuoter
ccmd = def { quoteExp = \str -> [| conduitCmd (LT.unpack $(quoteExp lt str)) |] }
