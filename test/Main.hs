{-# LANGUAGE FlexibleContexts          #-}

import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Monad.Trans.Replay
import Control.Exception

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    j1 <- runOnce emptyJournal comp
    print j1
    j2 <- runOnce j1 comp
    print j2
    j3 <- runOnce j2 comp
    assert (j3 == emptyJournal) (return ())

    where

    runOnce journal m = do
        res <- try $ replay journal m
        case res of
            Left (Suspended j) -> return j
            Right r -> do
                putStrLn $ "done: " ++ show r
                return emptyJournal

    comp = logged $ do
         r <- logged $ liftIO $ return 2
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         x <- liftIO $ return 3
         suspend
         liftIO $ print ("C", r, x)
