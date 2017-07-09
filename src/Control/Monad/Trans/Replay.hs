{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : Replay
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Results of the 'ReplayT' computations are logged using the 'logged'
-- combinator. A computation can be suspended at any point using the 'suspend'
-- primitive returning the logs which can be used to restart the computation
-- later. When logs are replayed using 'replay' the 'logged' combinator returns
-- the results of the previously logged computations from the log journal. Note
-- that only those computations are replayed that are explicitly logged.
-- Unlogged impure computations can result in the program misbehaving if it
-- takes a different path upon replay.

module Control.Monad.Trans.Replay
    ( ReplayT
    , MonadReplay (..)
    , replay
    , Loggable (..)
    , Journal
    , LogState
    , emptyJournal
    , logged
    , Suspended (..)
    , suspend
    )
where

import           Control.Exception           (Exception)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              throwM)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.State         (StateT (..), get, put)
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
    deriving (Eq, Read, Show)

-- | The log entries returned when an action is 'suspend'ed.
data Journal = Journal [LogEntry] deriving (Eq, Show)

-- | Create an empty log 'Journal'.
emptyJournal :: Journal
emptyJournal = Journal []

-- | The internal log state kept when logging or replaying.
data LogState = LogState [LogEntry] [LogEntry] deriving (Read, Show)

------------------------------------------------------------------------------
-- The ReplayT transformer
------------------------------------------------------------------------------

-- | The monad log and replay transformer. Maintains a running log of the
-- results of monadic actions.
newtype ReplayT m a = ReplayT { unReplayT :: StateT LogState m a }
    deriving ( Functor, Applicative, Monad                  -- monad
             , MonadIO, MonadTrans                          -- transformer
             , MonadThrow, MonadCatch, MonadMask            -- exceptions
             )

deriving instance (MonadBase b m) => MonadBase b (ReplayT m)

instance MonadTransControl ReplayT where
    type StT ReplayT a = StT (StateT LogState) a
    liftWith             = defaultLiftWith ReplayT unReplayT
    restoreT             = defaultRestoreT ReplayT

instance MonadBaseControl b m => MonadBaseControl b (ReplayT m) where
    type StM (ReplayT m) a = ComposeSt ReplayT m a
    liftBaseWith           = defaultLiftBaseWith
    restoreM               = defaultRestoreM

------------------------------------------------------------------------------
-- The MonadReplay class
------------------------------------------------------------------------------

-- | A monad with the ability to log and replay the results of monadic actions.
class Monad m => MonadReplay m where
    getLog :: m LogState
    putLog :: LogState -> m ()

instance Monad m => MonadReplay (ReplayT m) where
    getLog = ReplayT $ get
    putLog logs = ReplayT $ put logs

------------------------------------------------------------------------------
-- Logging
------------------------------------------------------------------------------

-- | Add the result of an action to the running log journal.  When replaying,
-- if the result of an action is available in the replay journal then get it
-- from the journal instead of running the action.
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
data Suspended = Suspended Journal deriving Show
instance Exception Suspended

-- | Suspend a computation before completion for resuming later using 'replay'.
-- Throws 'Suspended' exception which carries the current logs.
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
                    throwM $ Suspended
                           $ Journal (logResult () : tail ls)
                _ -> error "Bug: replay inside suspend"
    where logResult x = Result (show x)

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run a fresh 'ReplayT' computation using 'emptyJournal' logs or resume a
-- suspended computation using previously captured logs.  The previous state of
-- the action is restored and the action resumes after the 'suspend' call that
-- collected the logs.
replay :: Monad m => Journal -> ReplayT m a -> m a
replay (Journal entries) m =
    runStateT (unReplayT m) (LogState [] (reverse entries)) >>= return . fst
