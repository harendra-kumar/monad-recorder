{-# LANGUAGE FlexibleContexts          #-}

import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Exception

import Control.Monad.Trans.Replay
import Control.Monad.Trans.AutoLogged

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runAll explicit
    putStrLn ""
    runAll auto

    where

    runAll m = do
        j1 <- runOnce emptyJournal m
        j2 <- runOnce j1 m
        j3 <- runOnce j2 m
        assert (j3 == emptyJournal) (return ())
        return ()

    runOnce journal m = do
        res <- try $ replay journal m
        case res of
            Left (Suspended j) -> do
                putStrLn $ "suspended: " ++ show j
                return j
            Right r -> do
                putStrLn $ "done: " ++ show r
                return emptyJournal

    explicit = logged $ do
         r <- logged $ liftIO $ return 2
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         x <- liftIO $ return 3
         suspend
         liftIO $ print ("C", r, x)

    auto = unEmbed $ do
         r <- Embed $ ReplayT $ liftIO $ return 2
         Embed $ ReplayT $ liftIO $ print ("A",r)
         Embed $ pause
         Embed $ ReplayT $ liftIO $ print ("B",r)
         x <- Embed $ ReplayT $ liftIO $ return 3
         Embed $ pause
         Embed $ ReplayT $ liftIO $ print ("C", r, x)
