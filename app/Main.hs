module Main where

import Control.Concurrent
import User
import ConcurrentUtils
import Types

main :: IO ()
main = do
    -- Create users
    aliceMVar <- createUser "Alice"
    bobMVar <- createUser "Bob"
    charlieMVar <- createUser "Charlie"
    davidMVar <- createUser "David"
    emmaMVar <- createUser "Emma"
    frankMVar <- createUser "Frank"
    graceMVar <- createUser "Grace"
    henryMVar <- createUser "Henry"
    ireneMVar <- createUser "Irene"
    jackMVar <- createUser "Jack"
    
    -- Shared message counter
    messageCounter <- newMVar 0

    let users = [aliceMVar, bobMVar, charlieMVar, davidMVar, emmaMVar, frankMVar, graceMVar, henryMVar, ireneMVar, jackMVar]  -- List of all users
    let messageLimit = 100

    -- Start a thread for each user
    mapM_ (\userMVar -> forkIO $ userThreadLimited userMVar (filter (/= userMVar) users) messageCounter messageLimit) users

    -- Wait until 100 messages are sent
    waitForMessages messageCounter messageLimit

    putStrLn "Messages received by each user:"
    mapM_ (\userMVar -> do
        user <- readMVar userMVar
        count <- messageCount userMVar
        putStrLn $ username user ++ ": " ++ show count
        ) users
