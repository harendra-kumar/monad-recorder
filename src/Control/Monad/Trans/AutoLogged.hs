{-# LANGUAGE GADTs                      #-}

module Control.Monad.Trans.AutoLogged
    ( AutoReplayT (..)
    , unEmbed
    )
where

import Control.Monad.Trans.Replay

------------------------------------------------------------------------------
-- Constrained monad allowing automatic logging in bind operation
------------------------------------------------------------------------------

data AutoReplayT m a where
    Embed  :: (Show a, Read a) => ReplayT m a -> AutoReplayT m a
    Return :: a -> AutoReplayT m a
    Bind   :: AutoReplayT m a -> (a -> AutoReplayT m b) -> AutoReplayT m b
    FMap   :: (a -> b) -> AutoReplayT m a -> AutoReplayT m b
    Apply  :: AutoReplayT m (a -> b) -> AutoReplayT m a -> AutoReplayT m b

instance Functor (AutoReplayT f) where
    fmap = FMap

instance Applicative (AutoReplayT f) where
    pure = Return
    (<*>) = Apply

instance Monad (AutoReplayT m) where
    return = Return
    (>>=) = Bind

bind :: (Monad m, Read a, Show a)
    => ReplayT m a -> (a -> ReplayT m b) -> ReplayT m b
bind m f = logged m >>= f

unEmbed :: (Monad m, Show a, Read a) => AutoReplayT m a -> ReplayT m a
unEmbed (Embed m) = m

unEmbed (Return v) = return v

unEmbed (Bind (Embed m) f) = m `bind` (unEmbed . f)
unEmbed (Bind (Return v) f) = unEmbed (f v)
unEmbed (Bind (Bind m f) g) = unEmbed (Bind m (\x -> Bind (f x) g))
unEmbed (Bind (FMap f m1) g) = unEmbed (Bind m1 (g . f))
unEmbed (Bind (Apply m1 m2) g) = unEmbed (Bind (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))) g)

unEmbed (FMap f (Embed m)) = fmap f m
unEmbed (FMap f (Return a)) = unEmbed (Return (f a))
unEmbed (FMap f (Bind m g)) = unEmbed (Bind m (FMap f . g))
unEmbed (FMap f (FMap g m)) = unEmbed (FMap (f . g) m)
unEmbed (FMap f (Apply m1 m2)) = unEmbed (FMap f (Bind m1 (\x -> Bind m2 (\y -> Return (x y)))))

-- XXX use do notation
unEmbed (Apply (Embed m1) (Embed m2)) = m1 <*> m2
unEmbed (Apply (Embed m1) (Return a)) = m1 >>= (\x -> return (x a))
unEmbed (Apply (Embed m1) (Bind m2 g)) = m1 >>= (\x -> unEmbed (FMap x (Bind m2 g)))
unEmbed (Apply (Embed m1) (FMap g m2)) = m1 >>= (\x -> unEmbed (FMap x (FMap g m2)))
unEmbed (Apply (Embed m1) (Apply m2 m3)) = m1 >>= (\x -> unEmbed (FMap x (Apply m2 m3)))

unEmbed (Apply (Return f) (Embed m2)) = m2 >>= (\x -> return (f x))
unEmbed (Apply (Return f) (Return x)) = return (f x)
unEmbed (Apply (Return f) (Bind m g)) = unEmbed (FMap f (Bind m g))
unEmbed (Apply (Return f) (FMap g m)) = unEmbed (FMap f (FMap g m))
unEmbed (Apply (Return f) (Apply m1 m2)) = unEmbed (FMap f (Apply m1 m2))

unEmbed (Apply (FMap _ _) _) = error "Cannot serialize a function"

-- Apply and Bind as the first argument of an Apply
unEmbed (Apply _ _) = error "This applicative operation is supported because \
    \it requires the result of a monadic action to be a function which is \
    \not serializable."
