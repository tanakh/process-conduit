process-qq: A Quasi-Quoter to execute processes
===============================================

# About

This is a simple package for executing external process using quasi-quoters.

# Install

~~~ {.bash}
$ cabal update
$ cabal install process-qq
~~~

# API

process-qq has two quasi-quoters, `cmd` and `enumCmd`.

The result type of `cmd` is `IO Data.ByteString.Lazy.ByteString`,
`enumCmd`'s is `MonadIO m => Enumerator ByteString m a`.

Command is failed, an Exception is thrown.

Command is executed in ***run-time***, not compile-time.

# Example

* Invoke a process simply

~~~ {.haskell}
{-# LANGUAGE QuasiQuotes #-}
import System.Process.QQ

main = print =<< [cmd|ls|]
~~~

* Enumerate a process

~~~ {.haskell}
main =
  run_ $ [enumCmd|curl http://www.google.com/|] $$ iterHandle stdout
~~~

* Unquoting (syntax is same as [shakespeare-text](http://hackage.haskell.org/package/shakespeare-text))

~~~ {.haskell}
main = do
  [url] <- getArgs
  print =<< [cmd|curl #{url}|]
~~~
