-- | Module      : Types
-- Description : The Types module contains all the data types required for this project.

module Types where

-- | A 'Message' represents a single message in the social network with its content.
data Message = Message { messageContent :: String} 
  deriving (show)