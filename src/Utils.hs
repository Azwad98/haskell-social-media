-- | Module      : Module containing utility functions for the Haskell Social Media application.
-- Description : This module provides general utility functions like user prompts and unwrapping usernamea.
module Utils where

import System.IO (hFlush, stdout)
import User (createUser)
import Types (Username(..), Question(..))
import qualified Data.Set as Set
import Control.Concurrent (MVar)

-- | Populates the social media with 10 random users
-- The function creates 10 new users using the createUser function from the User module
createDefaultUsers :: IO [MVar User]
createDefaultUsers = sequence
  [ createUser (Username "Alice")
  , createUser (Username "Bob")
  , createUser (Username "Charlie")
  , createUser (Username "David")
  , createUser (Username "Emma")
  , createUser (Username "Frank")
  , createUser (Username "Grace")
  , createUser (Username "Henry")
  , createUser (Username "Irene")
  , createUser (Username "Jack")
  ]

-- | Returns a set of default usernames
getDefaultUsernames :: Set.Set Username
getDefaultUsernames = Set.fromList
  [ Username "Alice"
  , Username "Bob"
  , Username "Charlie"
  , Username "David"
  , Username "Emma"
  , Username "Frank"
  , Username "Grace"
  , Username "Henry"
  , Username "Irene"
  , Username "Jack"
  ]

-- | Handles the user signup process.
userSignupProcess :: Set.Set Username -> IO (Maybe (MVar User))
userSignupProcess existingUsernames = do
    signUpResponse <- promptYesNo (Question "Would you like to sign up to the social media? (y/n) ")
    if signUpResponse then do
        newName <- prompt (Question "Enter your name: ")
        let newUsername = Username newName
        mvar <- createUser newUsername
        return $ Just mvar
    else
        return Nothing

-- | Prompt the user with a Yes/No question and get a response.
-- The function displays a question and expects a 'y' (yes) or 'n' (no) response.
-- It returns 'True' for 'y' or 'yes' and 'False' for any other response.
promptYesNo :: Question -> IO Bool 
promptYesNo (Question question) = do
    putStr $ question ++ " (y/n) "
    hFlush stdout
    response <- getLine
    return $ response `elem` ["y", "Y", "yes", "Yes"]

-- | Prompt the user with a question and get a response.
-- The function displays a question and returns the user's response as a string.
prompt :: Question -> IO String
prompt (Question question) = do
    putStr question
    hFlush stdout
    getLine

-- | Helper function to unwrap the Username newtype.
unwrapUsername :: Username -> String
unwrapUsername (Username uname) = uname

-- | Handles displaying notifications for a new user.
handleNewUserNotifications :: MVar User -> IO ()
handleNewUserNotifications newUser = do
    count <- messageCount newUser
    putStrLn $ "You have " ++ show count ++ " new notifications. Would you like to view them?"
    viewNotifs <- promptYesNo (Question "Would you like to view them?")
    when viewNotifs $ do
        messages <- displayUserMessages newUser
        putStrLn messages

-- | Displays a summary of messages received by each user.
displayMessagesSummary :: [MVar User] -> IO ()
displayMessagesSummary users = do
    putStrLn "--------------------------------------------"
    putStrLn "Messages received by each user:"
    mapM_ (\userMVar -> do
        user <- readMVar userMVar
        count <- messageCount userMVar
        putStrLn $ unwrapUsername (username user) ++ ": " ++ show count
    ) users
    putStrLn "--------------------------------------------"