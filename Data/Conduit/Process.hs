{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse, BangPatterns #-}
module Data.Conduit.Process (
  -- * Run process
  pipeProcess,
  sourceProcess,
  conduitProcess,

  -- * Run shell command
  pipeCmd,
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
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.Exit
import System.IO
import System.Process

bufSize :: Int
bufSize = 64 * 1024

-- | Pipe of process
pipeProcess
  :: MonadResource m
     => CreateProcess
     -> GConduit S.ByteString m S.ByteString
pipeProcess cp = do
  (_, (Just cin, Just cout, _, ph)) <- lift $ allocate createp closep
  mvar <- liftIO newEmptyMVar
  go cin cout ph mvar False S.hGetNonBlocking
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

    go !cin !cout !ph !mvar !wait !rd = do
      out <- liftIO $ rd cout bufSize
      if S.null out then do
        end <- liftIO $ getProcessExitCode ph
        case end of
          Just ec -> do
            lift $ when (ec /= ExitSuccess) $ monadThrow ec
            return ()
          Nothing ->
            if wait then do
              emp <- liftIO $ isEmptyMVar mvar
              if emp then do
                go cin cout ph mvar wait rd
              else do
                liftIO $ takeMVar mvar
                go cin cout ph mvar False rd
            else do
              mb <- await
              case mb of
                Just inp -> do
                  liftIO $ do
                    S.hPut cin inp
                    forkIO (hFlush cin >>= putMVar mvar)
                  go cin cout ph mvar True rd
                Nothing -> do
                  liftIO (hClose cin)
                  go cin cout ph mvar wait S.hGetSome
      else do
        yield out
        go cin cout ph mvar wait rd

-- | Source of process
sourceProcess :: MonadResource m => CreateProcess -> GSource m S.ByteString
sourceProcess cp = CL.sourceNull >+> conduitProcess cp

-- | Conduit of process
conduitProcess :: MonadResource m
                  => CreateProcess -> GConduit S.ByteString m S.ByteString
conduitProcess = pipeProcess

-- | Pipe of shell command
pipeCmd :: MonadResource m
           => String -> GConduit S.ByteString m S.ByteString
pipeCmd = pipeProcess . shell

-- | Source of shell command
sourceCmd :: MonadResource m => String -> GSource m S.ByteString
sourceCmd = sourceProcess . shell

-- | Conduit of shell command
conduitCmd :: MonadResource m => String -> GConduit S.ByteString m S.ByteString
conduitCmd = conduitProcess . shell
