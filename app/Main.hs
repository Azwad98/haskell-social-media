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

-- | The 'main' function runs the entire application.
-- It prompts the user for sign-up, creates users, and starts the message simulation.
main :: IO ()
main = do

    -- Ask if the user wants to sign up
    signUpResponse <- prompt (Question "Would you like to sign up to the social media? (y/n) ")
    newUserMVar <- if signUpResponse `elem` ["y", "Y", "yes", "Yes"]
        then do
            newName <- prompt (Question "Enter your name: ")
            Just <$> createUser (Username newName)
        else return Nothing

    -- Create default users
    randomUsers <- createRandomUsers
    
    -- Shared message counter
    messageCounter <- newMVar 0

    -- Combine new user (if signed up) with base users
    let users = maybe randomUsers (: baseUsers) newUserMVar
    
    -- Set message limit to 100
    let messageLimit = 100

    -- Start a thread for each user
    mapM_ (\userMVar -> forkIO $ userThreadLimited userMVar (filter (/= userMVar) users) messageCounter messageLimit) users

    -- Wait until 100 messages are sent
    waitForMessages messageCounter messageLimit

    -- Display notifications for the new user (if signed up)
    case newUserMVar of
        Just mvar -> do
            count <- messageCount mvar
            putStrLn $ "You have " ++ show count ++ " new notifications. Would you like to view them?"
            viewNotifs <- promptYesNo (Question "Would you like to view them?")
            when viewNotifs $ do
                messages <- displayUserMessages mvar
                putStrLn messages
        Nothing -> return ()

    -- Display summary of messages received by each user
    putStrLn "--------------------------------------------"
    putStrLn "Messages received by each user:"
    mapM_ (\userMVar -> do
        user <- readMVar userMVar
        count <- messageCount userMVar
        putStrLn $ unwrapUsername (username user) ++ ": " ++ show count
        ) users
    putStrLn "--------------------------------------------"
