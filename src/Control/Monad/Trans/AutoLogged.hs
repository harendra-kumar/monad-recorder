{-# LANGUAGE GADTs                      #-}

module Control.Monad.Trans.AutoLogged
    ( AutoReplayT (..)
    , unEmbed
    )
where

import Control.Monad.Trans.Replay

------------------------------------------------------------------------------
-- Restricted monad allowing automatic logging in bind operation
------------------------------------------------------------------------------

class LogFunctor f where
    --logFmap :: (Show a, Read a) => (a -> b) -> f a -> f b
    logFmap :: (Read a) => (a -> b) -> f a -> f b

instance Functor m => LogFunctor (ReplayT m) where
    logFmap = fmap

class LogApplicative f where
    -- logAp :: (Show a, Read a) => f (a -> b) -> f a -> f b
    logApply :: (Show a, Read a) => f (a -> b) -> f a -> f b

instance Monad m => LogApplicative (ReplayT m) where
    logApply = (<*>)

class LogMonadOps m where
    logReturn :: a -> m a
    logBind :: (Show a, Read a) => m a -> (a -> m b) -> m b

instance Monad m => LogMonadOps (ReplayT m) where
    logReturn = return
    logBind m f = logged m >>= f

data AutoReplayT m a where
    Embed :: (LogMonadOps m, Show a, Read a) => m a -> AutoReplayT m a
    Return :: LogMonadOps m => a -> AutoReplayT m a
    Bind :: LogMonadOps m => AutoReplayT m a -> (a -> AutoReplayT m b) -> AutoReplayT m b
    FMap :: LogFunctor m => (a -> b) -> AutoReplayT m a -> AutoReplayT m b
    Apply :: (LogMonadOps m, LogApplicative m, LogFunctor m) => AutoReplayT m (a -> b) -> AutoReplayT m a -> AutoReplayT m b

instance LogFunctor f => Functor (AutoReplayT f) where
    fmap = FMap

instance (LogApplicative f, LogMonadOps f, LogFunctor f) => Applicative (AutoReplayT f) where
    pure = Return
    (<*>) = Apply

instance (LogMonadOps m, LogApplicative m, LogFunctor m) => Monad (AutoReplayT m) where
    return = Return
    (>>=) = Bind

-- XXX can we remove these constraints?
unEmbed :: (Show a, Read a) => AutoReplayT m a -> m a

unEmbed (Embed m) = m

-- return/pure
unEmbed (Return v) = logReturn v

-- bind
unEmbed (Bind (Embed m) f) = m `logBind` (unEmbed . f)
unEmbed (Bind (Return v) f) = unEmbed (f v)
unEmbed (Bind (Bind m f) g) = unEmbed (Bind m (\x -> Bind (f x) g))
unEmbed (Bind (FMap f m1) g) = unEmbed (Bind m1 (g . f))
unEmbed (Bind (Apply m1 m2) g) = unEmbed (Bind (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))) g)

-- fmap
unEmbed (FMap f (Embed m)) = logFmap f m
unEmbed (FMap f (Return a)) = unEmbed (Return (f a))
unEmbed (FMap f (Bind m g)) = unEmbed (Bind m (FMap f . g))
unEmbed (FMap f (FMap g m)) = unEmbed (FMap (f . g) m)
unEmbed (FMap f (Apply m1 m2)) = unEmbed (FMap f (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))))

-- Apply
unEmbed (Apply (Embed m1) (Embed m2)) = logApply m1 m2
unEmbed (Apply (Embed m1) (Return a)) = m1 `logBind` (\x -> logReturn (x a))
unEmbed (Apply (Embed m1) (Bind m2 g)) = m1 `logBind` (\x -> unEmbed (FMap x (Bind m2 g)))
unEmbed (Apply (Embed m1) (FMap g m2)) = m1 `logBind` (\x -> unEmbed (FMap x (FMap g m2)))
unEmbed (Apply (Embed m1) (Apply m2 m3)) = m1 `logBind` (\x -> unEmbed (FMap x (Apply m2 m3)))

unEmbed (Apply (Return f) (Embed m2)) = m2 `logBind` (\x -> logReturn (f x))
unEmbed (Apply (Return f) (Return x)) = logReturn (f x)
unEmbed (Apply (Return f) (Bind m g)) = unEmbed (FMap f (Bind m g))
unEmbed (Apply (Return f) (FMap g m)) = unEmbed (FMap f (FMap g m))
unEmbed (Apply (Return f) (Apply m1 m2)) = unEmbed (FMap f (Apply m1 m2))

-- The result of bind cannot be a function, we cannot serialize a function.
-- Cannot unEmbed a function.
unEmbed (Apply (Bind _ _) _) = error "Result of bind is a function"
unEmbed (Apply (FMap _ _) _) = error "Cannot serialize a function"
unEmbed (Apply (Apply _ _) _) = error "Cannot serialize a function"
