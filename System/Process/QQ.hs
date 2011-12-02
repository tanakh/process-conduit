{-# LANGUAGE TemplateHaskell #-}

module System.Process.QQ (
  cmd,
  lcmd,
  enumCmd,
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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

lcmd :: QuasiQuoter
lcmd = def { quoteExp = genLCmd }

enumCmd :: QuasiQuoter
enumCmd = def { quoteExp = genEnumCmd }

genCmd :: String -> ExpQ
genCmd str =
  [| do l <- E.run_ $ enumProcess $(quoteExp lt str) $$ EB.consume
        let s = B.concat $ BL.toChunks l
        B.length s `seq` return s
   |]

genLCmd :: String -> ExpQ
genLCmd str =
  [| E.run_ $ enumProcess $(quoteExp lt str) $$ EB.consume |]

genEnumCmd :: String -> ExpQ
genEnumCmd str = 
  [| enumProcess $(quoteExp lt str) |]

enumProcess :: MonadIO m => LT.Text -> E.Enumerator B.ByteString m a
enumProcess s step = do
  (h, ph) <- liftIO $ openProcess s
  r <- EB.enumHandle 65536 h step
  checkRet ph
  return r

openProcess :: LT.Text -> IO (Handle, ProcessHandle)
openProcess s = do
  (_, Just h, _, ph) <- createProcess (shell $ LT.unpack s) { std_out = CreatePipe }
  return (h, ph)

checkRet :: MonadIO m => ProcessHandle -> E.Iteratee a m ()
checkRet ph = liftIO $ do
  ec <- waitForProcess ph
  when (ec /= ExitSuccess) $ do
    throwIO ec

