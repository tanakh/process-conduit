{-# LANGUAGE CPP #-}
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

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import System.Exit
import System.IO
import System.Process

bufSize :: Int
bufSize = 64 * 1024

-- | Source of process
sourceProcess :: C.MonadResource m => CreateProcess -> C.Source m B.ByteString
sourceProcess cp = CL.sourceNull C.$= conduitProcess cp

-- | Conduit of process
conduitProcess :: C.MonadResource m => CreateProcess -> C.Conduit B.ByteString m B.ByteString
conduitProcess cp = C.conduitIO alloc cleanup push close
  where
    alloc = createProcess cp
      { std_in = CreatePipe
      , std_out = CreatePipe
      }

    cleanup _ =
      return ()
  
    push (Just cin, Just cout, _, ph) bstr = liftIO $ do
      B.hPutStr cin bstr
      -- hFlush cin
      mbec <- getProcessExitCode ph
      case mbec of
        Nothing -> do
          str <- B.hGetNonBlocking cout bufSize
          return $ C.IOProducing [str]
        Just ec -> do
          when (ec /= ExitSuccess) $ throwIO ec
          return $ C.IOFinished Nothing []

    close (Just cin, Just cout, _, ph) = liftIO $ do
      hClose cin
      ret <- getRest
      hClose cout
      return ret
      where
        getRest = do
          mbec <- getProcessExitCode ph
          case mbec of
            Nothing -> do
              str <- B.hGetNonBlocking cout bufSize
              (str:) <$> getRest
            Just ec -> do
              when (ec /= ExitSuccess) $ throwIO ec
              str <- B.hGetContents cout
              return [str]

-- | Source of shell command
sourceCmd :: C.MonadResource m => String -> C.Source m B.ByteString
sourceCmd cmd = CL.sourceNull C.$= conduitCmd cmd

-- | Conduit of shell command
conduitCmd :: C.MonadResource m
              => String
              -> C.Conduit B.ByteString m B.ByteString
conduitCmd cmd = conduitProcess (shell cmd)
