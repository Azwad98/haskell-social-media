-- | Module      : Module containing utility functions for the Haskell Social Media application.
-- Description : This module provides general utility functions like user prompts and unwrapping usernamea.
module Utils where

import System.IO (hFlush, stdout)
import Types
import User (createUser)

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