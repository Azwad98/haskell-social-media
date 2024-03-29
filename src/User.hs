-- | Module      : Module for user-related functionalities in the Haskell Social Media application.
-- Description : This module defines functions for creating users, sending messages, and retrieving user information.
module User where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, readMVar)
import Types
import Message

-- | Create a new user with a given username.
-- The function takes a 'String' representing the username and returns an 'MVar User'.
createUser :: Username -> IO (MVar User)
createUser name = newMVar $ User name []

-- | Send a message to a user.
-- This function takes an 'MVar User' and a 'Message', modifies the 'User' safely, and updates the 'MVar'.
sendMessage :: Username -> MVar User -> Message -> IO ()
sendMessage (Username senderName) receiverMVar msg = do
    let newMsg = Message senderName (messageContent msg)  -- Include the sender's name
    receiver <- takeMVar receiverMVar
    let updatedReceiver = receiver { messages = newMsg : messages receiver }
    putMVar receiverMVar updatedReceiver

-- | Retrieves the current state of a user for reading.
-- This is a blocking operation and should be used carefully to avoid deadlocks.
readUser :: MVar User -> IO User
readUser = readMVar

-- | Display all messages of a user.
-- This function takes an 'MVar User' and returns a 'String' representing all the messages received by the user.
displayUserMessages :: MVar User -> IO String
displayUserMessages userMVar = do
    user <- readMVar userMVar
    return $ unlines $ map (\msg -> "New Message from " ++ messageSender msg ++ ": \"" ++ messageContent msg ++ "\"") (messages user)

-- | Get the count of messages received by a user.
-- This function takes an 'MVar User' and returns the number of messages received.
messageCount :: MVar User -> IO Int
messageCount userMVar = do
    user <- readMVar userMVar
    return $ length $ messages user
