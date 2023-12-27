module Message where

import Types

-- | Create a new message with some content.
-- The function takes a string which will be the content of the message.
createMessage :: String -> Message
createMessage content = Message content

-- | Displays the content of a message.
-- The function takes a 'Message' and returns the content as a 'String'.
displayMessageString :: Message -> String
displayMessageString (Message content) = content
