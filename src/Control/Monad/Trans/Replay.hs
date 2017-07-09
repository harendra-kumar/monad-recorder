{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |
-- Module      : Replay
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Logging is done using the 'logged' combinator. It remembers the last value
-- returned by a given computation and replays it the next time the computation
-- is resumed. However this could be problematic if we do not annotate all
-- impure computations. The program can take a different path due to a
-- non-logged computation returning a different value. In that case we may
-- replay a wrong value. To detect this we can use a unique id for each logging
-- site and abort if the id does not match on replay.

module Control.Monad.Trans.Replay
    ( ReplayT (..)
    , MonadReplay (..)
    , Loggable (..)
    , LogSuspend (..)
    , Journal
    , logged
    , suspend
    , replay
    )
where

import           Control.Exception           (Exception)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              throwM)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.State         (StateT (..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT)

------------------------------------------------------------------------------
-- Loggable
------------------------------------------------------------------------------

-- | A type that can be logged.
class Loggable a where
    toLog :: a -> String
    fromLog :: String -> a

instance (Show a, Read a) => Loggable a where
    toLog = show
    fromLog = read

------------------------------------------------------------------------------
-- The journal
------------------------------------------------------------------------------

data LogEntry =
      Executing         -- we are inside this computation, not yet done
    | Result String     -- computation done, we have the result to replay
    deriving (Read, Show)

-- | The log entries returned when an action is 'suspend'ed.
data Journal = Journal [LogEntry] deriving Show

-- log entries and replay entries
data LogState = LogState [LogEntry] [LogEntry] deriving (Read, Show)

------------------------------------------------------------------------------
-- The ReplayT transformer
------------------------------------------------------------------------------

-- | The monad log and replay transformer. Maintains a running log of the
-- results of monadic actions.
newtype ReplayT m a = ReplayT { runReplayT :: StateT LogState m a }
    deriving ( Functor, Applicative, Monad                  -- monad
             , MonadIO, MonadTrans                          -- transformer
             , MonadThrow, MonadCatch, MonadMask            -- exceptions
             )

deriving instance (MonadBase b m) => MonadBase b (ReplayT m)

instance MonadTransControl ReplayT where
    type StT ReplayT a = StT (StateT LogState) a
    liftWith             = defaultLiftWith ReplayT runReplayT
    restoreT             = defaultRestoreT ReplayT

instance MonadBaseControl b m => MonadBaseControl b (ReplayT m) where
    type StM (ReplayT m) a = ComposeSt ReplayT m a
    liftBaseWith           = defaultLiftBaseWith
    restoreM               = defaultRestoreM

------------------------------------------------------------------------------
-- The MonadReplay class
------------------------------------------------------------------------------

-- | Interface for monads having the ability to log and replay the results of
-- monadic actions.
class Monad m => MonadReplay m where
    getLog :: m LogState
    putLog :: LogState -> m ()

------------------------------------------------------------------------------
-- Logging
------------------------------------------------------------------------------

-- | Add the result of an action to the running log journal. The journal can be
-- retrieved at any point by calling 'suspend' and later replayed. When
-- replaying, if the result of an action is available in the replay journal
-- then get it from the journal instead of running the action.
logged :: (Loggable a, Read a, Show a, MonadReplay m) => m a -> m a
logged m = do
    let enable = True
    logs <- getLog
    case logs of
        -- no replay
        LogState ls [] ->
            case enable of
                False -> m
                True -> do
                    putLog $ LogState (Executing : ls) []
                    runAndLogResult m

        -- replaying the log
        LogState ls (r:rs) -> do
        --    dbg $ "Replay: j: " ++ show j
            case r of
                Executing -> do
                    putLog $ LogState (r : ls) rs
                    runAndLogResult m
                Result val -> do
                    let x = fromLog val
                    putLog $ LogState (r : ls) rs
                    return x
    where

    runAndLogResult action = do
        x <- action
        -- replace the head of the log with the result
        LogState (_ : ls) _ <- getLog
        putLog $ LogState (Result (toLog x) : ls) []
        return x

-- | Exception thrown when 'suspend' is called.
data LogSuspend = LogSuspend Journal deriving Show
instance Exception LogSuspend

-- | Suspend a computation before completion for resuming later using 'replay'.
-- Throws 'LogSuspend' exception which carries the current logs.
suspend :: (MonadReplay m, MonadThrow m) => m ()
suspend = logged $ do
    logs <- getLog
    let enable = True
    case enable of
        False -> return ()
        True ->
            case logs of
                LogState ls [] -> do
                    -- replace the "Executing" entry at the head of the log
                    -- with a "()" so that we do not enter suspend on replay
                    throwM $ LogSuspend
                           $ Journal (logResult () : tail ls)
                _ -> error "Bug: replay inside suspend"
    where logResult x = Result (show x)

-- | Replay the logs previous captured using 'suspend'. The action resumes
-- after the 'suspend' that collected these logs. The previous state of the
-- action is restored from the logs.
replay :: MonadReplay m => m a -> Journal -> m a
replay m (Journal entries) = do
    putLog $ LogState [] (reverse entries)
    m
