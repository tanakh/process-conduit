{-# LANGUAGE TemplateHaskell #-}

module System.Process.QQ (
  cmd,
  enumCmd,
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import Data.Enumerator as E
import Data.Enumerator.Binary as EB
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Exit
import System.IO
import System.Process
import Text.Shakespeare.Text

def :: QuasiQuoter
def = QuasiQuoter {
  quoteExp = undefined,
  quotePat = undefined,
  quoteType = undefined,
  quoteDec = undefined
  }

cmd :: QuasiQuoter
cmd = def { quoteExp = genCmd }

enumCmd :: QuasiQuoter
enumCmd = def { quoteExp = genEnumCmd }

genCmd :: String -> ExpQ
genCmd str =
  [| E.run_ $ enumProcess $(quoteExp lt str) $$ EB.consume |]

genEnumCmd :: String -> ExpQ
genEnumCmd str = 
  [| enumProcess $(quoteExp lt str) |]

enumProcess :: MonadIO m => LT.Text -> E.Enumerator B.ByteString m a
enumProcess s step = do
  h <- liftIO $ openProcess s
  EB.enumHandle 65536 h step

openProcess :: LT.Text -> IO Handle
openProcess s = do
  (_, Just h, _, ph) <- createProcess (shell $ LT.unpack s) { std_out = CreatePipe }
  _ <- forkIO $ do
    ec <- waitForProcess ph
    when (ec /= ExitSuccess) $ do
      throwIO ec
  return h
