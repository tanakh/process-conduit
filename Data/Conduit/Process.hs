{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse #-}
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

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.List as C
import System.Exit
import System.IO
import System.Process

bufSize :: Int
bufSize = 64 * 1024

-- | Pipe of process
pipeProcess
  :: MonadResource m
     => CreateProcess
     -> Pipe B.ByteString B.ByteString m ()
pipeProcess cp = flip PipeM (return ()) $ do
  (_, (Just cin, Just cout, _, ph)) <- allocate createp closep
  return $ go cin cout ph B.hGetNonBlocking
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

    go cin cout ph rd = do
      out <- liftIO $ rd cout bufSize
      if B.null out then do
        end <- liftIO $ getProcessExitCode ph
        case end of
          Just ec -> do
            lift $ when (ec /= ExitSuccess) $ monadThrow ec
            Done Nothing ()
          Nothing -> do
            NeedInput
              (\inp -> do
                  liftIO $ B.hPut cin inp >> hFlush cin
                  go cin cout ph rd)
              (do liftIO (hClose cin)
                  go cin cout ph B.hGetSome)
      else do
        HaveOutput (go cin cout ph rd) (return ()) out

-- | Source of process
sourceProcess :: MonadResource m => CreateProcess -> Source m B.ByteString
sourceProcess cp = C.sourceNull $= conduitProcess cp

-- | Conduit of process
conduitProcess :: MonadResource m => CreateProcess -> Conduit B.ByteString m B.ByteString
conduitProcess = pipeProcess

-- | Pipe of shell command
pipeCmd :: MonadResource m => String -> Pipe B.ByteString B.ByteString m ()
pipeCmd = pipeProcess . shell

-- | Source of shell command
sourceCmd :: MonadResource m => String -> Source m B.ByteString
sourceCmd = sourceProcess . shell

-- | Conduit of shell command
conduitCmd :: MonadResource m => String -> Conduit B.ByteString m B.ByteString
conduitCmd = conduitProcess . shell
