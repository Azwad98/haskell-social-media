-- | Main module for the Haskell Social Media application.
-- This module handles user interaction, user creation, and starts the message simulation.
module Main where

import Control.Concurrent
import User
import ConcurrentUtils
import Types
import Utils
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Maybe (maybeToList)

-- | The 'main' function runs the entire application.
-- It prompts the user for sign-up, creates users, and starts the message simulation.
main :: IO ()
main = do

    -- Create the default users and extract their usernames
    defaultUserMVars <- createDefaultUsers

    -- Call the user signup process function from Utils
    newUserMVar <- userSignupProcess

    -- Create the user list for the simulation, including the new user if added
    let allUsers = defaultUserMVars ++ maybeToList newUserMVar
    
    -- Shared message counter
    messageCounter <- newMVar 0
    
    -- Set message limit to 100
    let messageLimit = 100

    -- Start a thread for each user
    mapM_ (\userMVar -> forkIO $ userThreadLimited userMVar (filter (/= userMVar) allUsers) messageCounter messageLimit) allUsers

    -- Wait until 100 messages are sent
    waitForMessages messageCounter messageLimit

    -- Display notifications for the new user (if signed up)
    case newUserMVar of
        Just newUser -> handleNewUserNotifications newUser
        Nothing -> return ()

    -- Display summary of messages received by each user
    displayMessagesSummary allUsers
