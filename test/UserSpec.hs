module UserSpec (spec) where

import Test.Hspec
import Control.Concurrent
import User
import Message
import Types

spec :: Spec
spec = do
  describe "User module functions" $ do
    -- Test for createUser function
    describe "createUser" $ do
      it "creates a user with the given username and an empty message list" $ do
        userMVar <- createUser "Alice"
        user <- readMVar userMVar
        username user `shouldBe` "Alice"
        messages user `shouldBe` []

    -- Test for sendMessage function
    describe "sendMessage" $ do
      it "sends a message to a user" $ do
        userMVar <- createUser "Bob"
        let msg = createMessage "Hello, Bob!"
        sendMessage userMVar msg
        user <- readMVar userMVar
        length (messages user) `shouldBe` 1

    -- Test for readUser function
    describe "readUser" $ do
      it "reads the current state of a user" $ do
        userMVar <- createUser "Charlie"
        user <- readUser userMVar
        username user `shouldBe` "Charlie"

    -- Test for displayUserMessages function
    describe "displayUserMessages" $ do
      it "displays all messages of a user" $ do
        userMVar <- createUser "Dave"
        let msg = createMessage "Hello, Dave!"
        sendMessage userMVar msg
        messagesStr <- displayUserMessages userMVar
        messagesStr `shouldBe` "Hello, Dave!\n"

    -- Test for messageCount function
    describe "messageCount" $ do
      it "counts the number of messages received by a user" $ do
        userMVar <- createUser "Eve"
        msgCount <- messageCount userMVar
        msgCount `shouldBe` 0