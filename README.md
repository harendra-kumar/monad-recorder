# Monad Recorder

[![Build Status](https://travis-ci.org/harendra-kumar/monad-recorder.svg?branch=master)](https://travis-ci.org/harendra-kumar/monad-recorder)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/nag8lvguh5upe540?svg=true)](https://ci.appveyor.com/project/harendra-kumar/monad-recorder)
[![Coverage Status](https://coveralls.io/repos/harendra-kumar/monad-recorder/badge.svg?branch=master&service=github)](https://coveralls.io/github/harendra-kumar/monad-recorder?branch=master)

A monad transformer that allows recording the results of monadic actions and
allows replaying them later so that the application can resume from the same
point.

Results of a `RecorderT` computation are recorded in a running journal
using the `record` combinator. A computation can be paused at any point
using the `pause` primitive returning a `Recording` that can be used to
restart the computation from the same point later. When the recording is
replayed, the `record` combinator returns the previously recorded result of
the computation from the journal being replayed instead of actually running the
computation again.

```haskell
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Recorder (runRecorderT, record, pause, Paused(..), blank)
import Control.Exception (catch)

main = do
    recording <- (runRecorderT blank computation >> return blank)
                 `catch` \(Paused r) -> return r
    putStrLn "Computation paused, resuming again with recorded logs"
    runRecorderT recording computation
    return ()

    where

    computation = do
         x1 <- record $ liftIO $ return 1
         record $ liftIO $ print ("A", x1)
         x2 <- record $ liftIO $ return 2
         record pause
         record $ liftIO $ print ("B", x1, x2)
```

This package is inspired by the logging implementation in the `transient`
package by Alberto G. Corona. Related packages:

* https://hackage.haskell.org/package/transient
* https://hackage.haskell.org/package/Workflow
