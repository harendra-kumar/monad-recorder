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
            Left (Paused j) -> do
                putStrLn $ "suspended: " ++ show j
                return j
            Right r -> do
                putStrLn $ "done: " ++ show r
                return emptyJournal

    explicit = record $ do
         r <- record $ liftIO $ return 2
         record $ liftIO $ print ("A",r)
         record pause
         record $ liftIO $ print ("B",r)
         x <- liftIO $ return 3
         record pause
         liftIO $ print ("C", r, x)

    auto = do
        recorder $ do
            r <- return 2
            R $ liftIO $ print ("A",r)
            R $ pause
            R $ liftIO $ print ("B",r)
            x <- return 3
            R $ pause
            R $ liftIO $ print ("C", r, x)
        record $ liftIO $ print ("X")
