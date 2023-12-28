module Message where

import Types

-- | Create a new message with a sender and content.
createMessage :: String -> String -> Message
createMessage sender content = Message sender content

-- | Displays the content of a message.
displayMessageString :: Message -> String
displayMessageString (Message _ content) = content
