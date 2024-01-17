-- | Module      : Types
-- Description : The Types module contains all the data types required for this project.

module Types where

-- | Represents the name of the message sender.
newtype Sender = Sender String deriving (Eq, Show)

-- | Represents the content of the message.
newtype Content = Content String deriving (Eq, Show)

-- | Represents the username of a user in the social network.
newtype Username = Username String deriving (Eq, Ord, Show)

-- | Represents a question asked to the user.
newtype Question = Question String deriving (Eq, Show)

-- | A 'Message' represents a single message in the social network with its content.
data Message = Message { messageSender :: String, messageContent :: String} 
  deriving (Eq, Show)

-- | A 'User' represents a user in the social network with a username and a list of received messages.
data User = User { username :: Username, messages :: [Message] }
  deriving (Show)