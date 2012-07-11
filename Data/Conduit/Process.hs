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

import Control.Concurrent hiding (yield)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Loop
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import System.Exit
import System.IO
import System.Process

bufSize :: Int
bufSize = 64 * 1024

-- | Conduit of process
conduitProcess
  :: MonadResource m
     => CreateProcess
     -> GConduit S.ByteString m S.ByteString
conduitProcess cp = do
  (_, (Just cin, Just cout, _, ph)) <- lift $ allocate createp closep

  repeatLoopT $ do
    repeatLoopT $ do
      liftIO $ print 123
      out <- liftIO $ S.hGetNonBlocking cout bufSize
      liftIO $ print 456
      when (S.null out) exit
      lift . lift $ yield out

    end <- liftIO $ getProcessExitCode ph
    when (isJust end) $ do
      let ec = fromJust end
      lift . lift $ when (ec /= ExitSuccess) $ monadThrow ec
      exit

    mb <- lift await
    case mb of
      Just inp -> do
        liftIO $ do
          S.hPut cin inp
          hFlush cin
      Nothing -> do
        liftIO $ hClose cin

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

-- | Source of process
sourceProcess :: MonadResource m => CreateProcess -> GSource m S.ByteString
sourceProcess cp = CL.sourceNull >+> conduitProcess cp

-- | Conduit of shell command
conduitCmd :: MonadResource m => String -> GConduit S.ByteString m S.ByteString
conduitCmd = conduitProcess . shell

-- | Source of shell command
sourceCmd :: MonadResource m => String -> GSource m S.ByteString
sourceCmd = sourceProcess . shell
