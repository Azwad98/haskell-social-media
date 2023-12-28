module Utils where

import System.IO (hFlush, stdout)

-- Function to prompt the user with a question and get a Yes/No response.
promptYesNo :: String -> IO Bool
promptYesNo question = do
    putStr $ question ++ " (y/n) "
    hFlush stdout
    response <- getLine
    return $ response `elem` ["y", "Y", "yes", "Yes"]


prompt :: String -> IO String
prompt question = do
    putStr question
    hFlush stdout
    getLine