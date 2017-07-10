{-# LANGUAGE FlexibleContexts          #-}

import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Exception

import Control.Monad.Trans.Recorder
import Control.Monad.Trans.AutoRecorder

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runAll explicit
    putStrLn ""
    runAll auto

    where

    runAll m = do
        r1 <- runOnce blank m
        r2 <- runOnce r1 m
        r3 <- runOnce r2 m
        assert (r3 == blank) (return ())
        return ()

    runOnce recording m = do
        res <- try $ play recording m
        case res of
            Left (Paused r) -> do
                putStrLn $ "suspended: " ++ show r
                return r
            Right r -> do
                putStrLn $ "done: " ++ show r
                return blank

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
