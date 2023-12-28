module Main where

import Control.Concurrent
import User
import ConcurrentUtils
import Types
import Utils
import Text.Read (readMaybe)
import Control.Monad (when)

main :: IO ()
main = do

    -- Ask if the user wants to sign up
    signUpResponse <- prompt "Would you like to sign up to the social media? (y/n) "
    newUserMVar <- if signUpResponse `elem` ["y", "Y", "yes", "Yes"]
        then do
            newName <- prompt "Enter your name: "
            Just <$> createUser newName
        else return Nothing

    -- Function to prompt the user with a question and get a response.


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

    let baseUsers = [aliceMVar, bobMVar, charlieMVar, davidMVar, emmaMVar, frankMVar, graceMVar, henryMVar, ireneMVar, jackMVar]
    let users = maybe baseUsers (: baseUsers) newUserMVar  -- Add new user if signed up
    let messageLimit = 100

    -- Start a thread for each user
    mapM_ (\userMVar -> forkIO $ userThreadLimited userMVar (filter (/= userMVar) users) messageCounter messageLimit) users

    -- Wait until 100 messages are sent
    waitForMessages messageCounter messageLimit

    case newUserMVar of
        Just mvar -> do
            count <- messageCount mvar
            putStrLn $ "You have " ++ show count ++ " new notifications. Would you like to view them?"
            viewNotifs <- promptYesNo "Would you like to view them?"
            when viewNotifs $ do
                messages <- displayUserMessages mvar
                putStrLn messages
        Nothing -> return ()

    putStrLn "--------------------------------------------"
    putStrLn "Messages received by each user:"
    mapM_ (\userMVar -> do
        user <- readMVar userMVar
        count <- messageCount userMVar
        putStrLn $ username user ++ ": " ++ show count
        ) users
    putStrLn "--------------------------------------------"
