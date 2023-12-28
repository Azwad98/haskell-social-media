-- | Module      : Types
-- Description : The Types module contains all the data types required for this project.

module Types where

-- | A 'Message' represents a single message in the social network with its content.
data Message = Message { messageSender :: String, messageContent :: String} 
  deriving (Eq, Show)

-- | A 'User' represents a user in the social network with a username and a list of received messages.
data User = User { username :: String, messages :: [Message] }
  deriving (Show)