-- | Module      : Module for concurrency utilities in the Haskell Social Media application.
-- Description : This module includes functions for simulating message sending, user actions, and managing concurrent processes.
module ConcurrentUtils where

import Control.Concurrent (MVar, takeMVar, putMVar, readMVar, threadDelay)
import Control.Monad (forever, when)
import System.Random (randomRIO)
import User
import Message
import Types
import Text.Printf (printf)
import Utils (unwrapUsername)

-- | Generate a random delay.
randomDelay :: IO ()
randomDelay = do
    delay <- randomRIO (100000, 500000)
    threadDelay delay

-- | Generate a random message from a prefixed set.
randomMessage :: IO String
randomMessage = do
    let messages = ["Hi there!", "Hello!", "How are you?", "Good day!", "Nice to meet you!"]
    index <- randomRIO (0, length messages - 1)
    return $ messages !! index

-- | Simulates sending a message from one user to another and prints the action.
simulateMessageSending :: MVar User -> MVar User -> String -> IO ()
simulateMessageSending senderMVar receiverMVar content = do
    sender <- readMVar senderMVar
    receiver <- readMVar receiverMVar

    let formattedMessage = printf "%-10s opened a chat with %-10s  %-10s is typing...      Message: \"%s\"" 
                           (unwrapUsername $ username sender) 
                           (unwrapUsername $ username receiver) 
                           (unwrapUsername $ username sender) 
                           content
    putStrLn formattedMessage
    sendMessage (unwrapUsername $ username sender) receiverMVar (createMessage (Sender $ unwrapUsername $ username sender) (Content content))

-- | Function to run in each user thread with a message limit.
userThreadLimited :: MVar User -> [MVar User] -> MVar Int -> Int -> IO ()
userThreadLimited userMVar otherUsers messageCounter limit = do
    forever $ do
        currentCount <- takeMVar messageCounter
        when (currentCount < limit) $ do
            randomDelay
            message <- randomMessage
            receiverMVar <- selectRandomUser otherUsers
            simulateMessageSending userMVar receiverMVar message
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
