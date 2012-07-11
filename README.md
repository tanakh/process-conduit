process-conduit: Conduit for processes
======================================

# About

This package provides [conduit](http://hackage.haskell.org/package/conduit)
for processes.
Also this provides quasi-quoters for process using it.

# Install

~~~ {.bash}
$ cabal update
$ cabal install process-conduit
~~~

# Document

Haddock documents are here:

<http://hackage.haskell.org/package/process-conduit>

# Quasi Quoters

process-conduit has three quasi-quoters, `cmd`, `scmd` and `ccmd`.

The result type of `cmd` is Lazy `ByteString`,
but execution will perform strictly.

The result type of `scmd` and `ccmd` are
`GSource m ByteString` and
`GConduit ByteString m ByteString` respectively.

If a command is failed, an exception is thrown.

Commands are executed in ***run-time***, not compile-time.

# Examples

* Create a Source and a Conduit of process

~~~ {.haskell}
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Process
import System.IO

main :: IO ()
main = runResourceT $ do
  sourceCmd "ls" $= conduitCmd "sort" $$ CB.sinkHandle stdout
~~~

* Invoke a process simply

~~~ {.haskell}
{-# LANGUAGE QuasiQuotes #-}
import System.Process.QQ

main = print =<< [cmd|ls|]
~~~

* Conduit Quasi-Quoters

~~~ {.haskell}
main :: IO ()
main = runResourceT $ do
  [scmd|ls|] $= [ccmd|sort|] $$ CB.sinkHandle stdout
~~~

* Unquoting (syntax is same as [shakespeare-text](http://hackage.haskell.org/package/shakespeare-text))

~~~ {.haskell}
main = do
  [url] <- getArgs
  print =<< [cmd|curl #{url}|]
~~~
