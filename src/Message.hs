module Message where

import Types

-- | Create a new message with some content
-- The function takes a string which will be the content of the message
createMessage :: String -> Message
createMessage content = Message content
