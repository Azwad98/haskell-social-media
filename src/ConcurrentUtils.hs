module ConcurrentUtils where

import Control.Concurrent (MVar, takeMVar, putMVar, readMVar, forkIO, threadDelay)
import Control.Monad (forever, when)
import System.Random (randomRIO)
import User
import Message
import Types

-- | Generate a random delay.
randomDelay :: IO ()
randomDelay = do
    delay <- randomRIO (1000000, 5000000)  -- Random delay between 1 and 5 seconds
    threadDelay delay

-- | Generate a random message.
randomMessage :: IO String
randomMessage = do
    let messages = ["Hi there!", "Hello!", "How are you?", "Good day!", "Nice to meet you!"]
    index <- randomRIO (0, length messages - 1)
    return $ messages !! index

-- | Function to run in each user thread with a message limit.
userThreadLimited :: MVar User -> [MVar User] -> MVar Int -> Int -> IO ()
userThreadLimited userMVar otherUsers messageCounter limit = do
    forever $ do
        currentCount <- takeMVar messageCounter
        when (currentCount < limit) $ do
            randomDelay
            message <- randomMessage
            receiverMVar <- selectRandomUser otherUsers
            sendMessage receiverMVar (createMessage message)
            putMVar messageCounter (currentCount + 1)
        when (currentCount >= limit) $ do
            putMVar messageCounter currentCount
            return ()

-- | Wait until a certain number of messages have been sent.
waitForMessages :: MVar Int -> Int -> IO ()
waitForMessages messageCounter limit = do
    currentCount <- readMVar messageCounter
    when (currentCount < limit) $ do
        threadDelay 100000  -- Check every 0.1 second
        waitForMessages messageCounter limit

-- | Selects a random user from a list.
selectRandomUser :: [MVar User] -> IO (MVar User)
selectRandomUser users = do
    index <- randomRIO (0, length users - 1)
    return $ users !! index
