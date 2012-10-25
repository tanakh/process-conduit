{-# LANGUAGE FlexibleContexts, OverloadedStrings, BangPatterns #-}
module Data.Conduit.Process (
  -- * Run process
  sourceProcess,
  conduitProcess,

  -- * Run shell command
  sourceCmd,
  conduitCmd,

  -- * Convenience re-exports
  shell,
  proc,
  CreateProcess(..),
  CmdSpec(..),
  StdStream(..),
  ProcessHandle,
  ) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Loop
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import System.Exit (ExitCode(..))
import System.IO
import System.Process

bufSize :: Int
bufSize = 64 * 1024

-- | Conduit of process
conduitProcess
  :: MonadResource m
     => CreateProcess
     -> GConduit S.ByteString m S.ByteString
conduitProcess cp = bracketP createp closep $ \(Just cin, Just cout, _, ph) -> do
  end <- repeatLoopT $ do
    -- if process's outputs are available, then yields them.
    repeatLoopT $ do
      b <- liftIO $ hReady' cout
      when (not b) exit
      out <- liftIO $ S.hGetSome cout bufSize
      void $ lift . lift $ yield out

    -- if process exited, then exit
    end <- liftIO $ getProcessExitCode ph
    when (isJust end) $ exitWith end

    -- if upper stream ended, then exit
    inp <- lift await
    when (isNothing inp) $ exitWith Nothing

    -- put input to process
    liftIO $ S.hPut cin $ fromJust inp
    liftIO $ hFlush cin

  -- uppstream or process is done.
  -- process rest outputs.
  liftIO $ hClose cin
  repeatLoopT $ do
    out <- liftIO $ S.hGetSome cout bufSize
    when (S.null out) exit
    lift $ yield out

  ec <- liftIO $ maybe (waitForProcess' ph) return end
  lift $ when (ec /= ExitSuccess) $ monadThrow ec

  where
    createp = createProcess cp
      { std_in  = CreatePipe
      , std_out = CreatePipe
      }

    closep (Just cin, Just cout, _, ph) = do
      hClose cin
      hClose cout
      _ <- waitForProcess ph
      return ()

    hReady' h =
      hReady h `E.catch` \(E.SomeException _) -> return False
    waitForProcess' ph =
      waitForProcess ph `E.catch` \(E.SomeException _) -> return ExitSuccess

-- | Source of process
sourceProcess :: MonadResource m => CreateProcess -> GSource m S.ByteString
sourceProcess cp = CL.sourceNull >+> conduitProcess cp

-- | Conduit of shell command
conduitCmd :: MonadResource m => String -> GConduit S.ByteString m S.ByteString
conduitCmd = conduitProcess . shell

-- | Source of shell command
sourceCmd :: MonadResource m => String -> GSource m S.ByteString
sourceCmd = sourceProcess . shell
