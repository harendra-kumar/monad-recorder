{-# LANGUAGE GADTs                      #-}

-- |
-- Module      : AutoRecorder
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unlike 'RecorderT' which records selective operations using the 'record'
-- combinator 'AutoRecorderT' monad enforces recording of all operations in the
-- monad. This ensures that we do not miss recording any monadic operation that
-- can cause problems on replay.

module Control.Monad.Trans.AutoRecorder
    ( AutoRecorderT (R)
    , recorder
    )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Recorder

------------------------------------------------------------------------------
-- Constrained monad allowing automatic logging in bind operation
------------------------------------------------------------------------------

-- | A monad that enforces recording of the results of all monadic actions.
-- The constructor 'R' lifts a 'MonadRecorder' monad to 'AutoRecorderT'.
data AutoRecorderT m a where
    R  :: (MonadRecorder m, Show a, Read a) => m a -> AutoRecorderT m a
    FMap   :: (a -> b) -> AutoRecorderT m a -> AutoRecorderT m b
    Return :: a -> AutoRecorderT m a
    Apply  :: AutoRecorderT m (a -> b) -> AutoRecorderT m a -> AutoRecorderT m b
    Bind   :: AutoRecorderT m a -> (a -> AutoRecorderT m b) -> AutoRecorderT m b

instance Functor (AutoRecorderT f) where
    fmap = FMap

instance Applicative (AutoRecorderT f) where
    pure = Return
    (<*>) = Apply

instance Monad (AutoRecorderT m) where
    return = Return
    (>>=) = Bind

-- Only bind is logged, return is not logged
bind :: (MonadRecorder m, Read a, Show a)
    => m a -> (a -> m b) -> m b
bind m f = record m >>= f

-- | Run the 'AutoRecorderT' monad recording all operations in it.
recorder :: (MonadRecorder m, MonadThrow m, Show a, Read a)
    => AutoRecorderT m a -> m a

recorder (R m) = m
recorder (Return v) = return v

recorder (Bind (R m) f)     = m `bind` (recorder . f)
recorder (Bind (Return v) f)    = recorder (f v)
recorder (Bind (Bind m f) g)    = recorder (Bind m (\x -> Bind (f x) g))
recorder (Bind (FMap f m1) g)   = recorder (Bind m1 (g . f))
recorder (Bind (Apply m1 m2) g) =
    recorder (Bind (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))) g)

recorder (FMap f (R m))     = fmap f m
recorder (FMap f (Return a))    = recorder (Return (f a))
recorder (FMap f (Bind m g))    = recorder (Bind m (FMap f . g))
recorder (FMap f (FMap g m))    = recorder (FMap (f . g) m)
recorder (FMap f (Apply m1 m2)) =
    recorder (FMap f (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))))

recorder (Apply (R m1) (R m2))    = m1 <*> m2
recorder (Apply (R m1) (Return a))    = m1 `bind` (\f -> return (f a))
recorder (Apply (R m1) (Bind m2 g))   =
    m1 `bind` (\f -> recorder (FMap f (Bind m2 g)))

recorder (Apply (R m1) (FMap g m2))   =
    m1 `bind` (\f -> recorder (FMap f (FMap g m2)))

recorder (Apply (R m1) (Apply m2 m3)) =
    m1 `bind` (\f -> recorder (FMap f (Apply m2 m3)))

recorder (Apply (Return f) (R m))     = m `bind` (return . f)
recorder (Apply (Return f) (Return x))    = return (f x)
recorder (Apply (Return f) (Bind m g))    = recorder (FMap f (Bind m g))
recorder (Apply (Return f) (FMap g m))    = recorder (FMap f (FMap g m))
recorder (Apply (Return f) (Apply m1 m2)) = recorder (FMap f (Apply m1 m2))

-- FMap, Apply and Bind as the first argument of an Apply
recorder (Apply _ _) = error "This applicative operation is not supported \
    \because it requires the result of a monadic action to be a function \
    \which is not serializable."
