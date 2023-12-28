-- | Module      : Module for message-related functionalities in the Haskell Social Media application.
-- Description : This module defines the structure of messages and functions for message creation and display.
module Message where

import Types

-- | Create a new message.
-- Given a sender's name and message content, it creates a 'Message'.
createMessage :: Sender -> Content -> Message
createMessage (Sender sender) (Content content) = Message sender content

-- | Display the content of a message.
-- Given a 'Message', it returns the message content as a 'String'.
displayMessageString :: Message -> String
displayMessageString (Message _ content) = content
