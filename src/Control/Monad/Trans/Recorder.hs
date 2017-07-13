{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : Recorder
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Results of the 'RecorderT' computations are recorded in a running journal
-- using the 'record' combinator. A computation can be paused at any point
-- using the 'pause' primitive returning a 'Recording' that can be used to
-- restart the computation from the same point later. When the recording is
-- replayed, the 'record' combinator returns the previously recorded result of
-- the computation from the journal instead of actually running the
-- computation.
--
-- Note that only those computations are replayed that are explicitly recorded.
-- Unrecorded impure computations can result in the program misbehaving if it
-- takes a different path upon replay.  Instead of recording selectively you
-- can enforce recording of each and every operation using the 'AutoRecorder'
-- module.

module Control.Monad.Trans.Recorder
    ( RecorderT
    , Journal
    , MonadRecorder (..)
    , runRecorderT
    , Recordable (..)
    , Recording
    , blank
    , record
    , Paused (..)
    , pause
    )
where

import           Control.Exception           (Exception)
import           Control.Monad               (when)
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
-- Recordable
------------------------------------------------------------------------------

-- | A type that can be recorded.
class Recordable a where
    toJournal :: a -> String
    fromJournal :: String -> a

instance (Show a, Read a) => Recordable a where
    toJournal = show
    fromJournal = read

------------------------------------------------------------------------------
-- The journal
------------------------------------------------------------------------------

data LogEntry =
      Executing         -- we are inside this computation, not yet done
    | Result String     -- computation done, we have the result to replay
    deriving (Eq, Read, Show)

-- | The log entries returned when an action is 'suspend'ed.
data Recording = Recording [LogEntry] deriving (Eq, Show)

-- | An empty 'Recording'.
blank :: Recording
blank = Recording []

-- | The internal log state kept when recording or replaying.
data Journal = Journal [LogEntry] [LogEntry] deriving (Read, Show)

------------------------------------------------------------------------------
-- The RecorderT transformer
------------------------------------------------------------------------------

-- | The monad record and play transformer. Maintains a running log of the
-- results of monadic actions.
newtype RecorderT m a = RecorderT { unRecorderT :: StateT Journal m a }
    deriving ( Functor, Applicative, Monad                  -- monad
             , MonadIO, MonadTrans                          -- transformer
             , MonadThrow, MonadCatch, MonadMask            -- exceptions
             )

deriving instance (MonadBase b m) => MonadBase b (RecorderT m)

instance MonadTransControl RecorderT where
    type StT RecorderT a = StT (StateT Journal) a
    liftWith             = defaultLiftWith RecorderT unRecorderT
    restoreT             = defaultRestoreT RecorderT

instance MonadBaseControl b m => MonadBaseControl b (RecorderT m) where
    type StM (RecorderT m) a = ComposeSt RecorderT m a
    liftBaseWith           = defaultLiftBaseWith
    restoreM               = defaultRestoreM

------------------------------------------------------------------------------
-- The MonadRecorder class
------------------------------------------------------------------------------

-- | A monad with the ability to record and play the results of monadic actions.
class Monad m => MonadRecorder m where
    -- Note: we cannot have the "record" function here as it requires (Show a,
    -- Read a) constraint.
    getJournal :: m Journal
    -- ^ Retrieve the record and replay journal. Used by the implementation of
    -- 'record' and 'play'.
    putJournal :: Journal -> m ()
    -- ^ Replace the record and replay journal. Used by the implementation of
    -- 'record' and 'play'.

    -- TBD create recording based on play points. Each play starts a new
    -- recording. There could be nested plays starting a nested recording.
    play :: Recording -> m ()
    -- ^ Play a previously recorded journal. This function can be used to set a
    -- replay journal at any point.

instance Monad m => MonadRecorder (RecorderT m) where
    getJournal = RecorderT $ get
    putJournal logs = RecorderT $ put logs
    play (Recording entries) = do
        Journal recordings replay <- getJournal
        when (recordings /= [] || replay /= []) $
            error "The journal must be empty when a play is initiated"
        putJournal $ Journal [] (reverse entries)

------------------------------------------------------------------------------
-- Logging
------------------------------------------------------------------------------

-- | Add the result of an action to the recording journal.  During replay,
-- if the result of an action is available in the replay journal then get it
-- from the journal instead of running the action.
record :: (Recordable a, Read a, Show a, MonadRecorder m) => m a -> m a
record m = do
    let enable = True
    logs <- getJournal
    case logs of
        -- no replay
        Journal ls [] ->
            case enable of
                False -> m
                True -> do
                    putJournal $ Journal (Executing : ls) []
                    runAndLogResult m

        -- replaying the log
        Journal ls (r:rs) -> do
            case r of
                Executing -> do
                    putJournal $ Journal (r : ls) rs
                    runAndLogResult m
                Result val -> do
                    let x = fromJournal val
                    putJournal $ Journal (r : ls) rs
                    return x
    where

    runAndLogResult action = do
        x <- action
        -- replace the head of the log with the result
        Journal (_ : ls) _ <- getJournal
        putJournal $ Journal (Result (toJournal x) : ls) []
        return x

-- | Exception thrown when 'pause' is called.
data Paused = Paused Recording deriving Show
instance Exception Paused

-- | Pause a computation before completion for resuming later.
-- Throws 'Paused' exception which carries the current recorded logs.
pause :: (MonadRecorder m, MonadThrow m) => m ()
pause = do
    logs <- getJournal
    let enable = True
    case enable of
        False -> return ()
        True ->
            case logs of
                Journal ls [] -> do
                    -- replace the "Executing" entry at the head of the log
                    -- with a "()" so that we do not enter suspend on replay
                    throwM $ Paused
                           $ Recording (logResult () : tail ls)
                _ -> error "Bug: replay inside suspend"
    where logResult x = Result (show x)

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run a fresh 'RecorderT' computation using 'blank' recording or resume a
-- paused computation using captured recording.  The captured state of the
-- action is restored and the action resumes right after the 'pause' call that
-- paused the action.
runRecorderT :: Monad m => Recording -> RecorderT m a -> m a
runRecorderT (Recording entries) m =
    runStateT (unRecorderT m) (Journal [] (reverse entries)) >>= return . fst
