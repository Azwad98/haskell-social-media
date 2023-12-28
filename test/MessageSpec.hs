module MessageSpec (spec) where

import Test.Hspec
import Message
import Types

spec :: Spec
spec = do
  describe "createMessage" $ do
    it "creates a message with the specified sender and content" $ do
      let sender = "Alice"
      let content = "test message"
      let message = createMessage sender content
      messageContent message `shouldBe` content
      messageSender message `shouldBe` sender

  describe "displayMessageString" $ do
    it "displays the content of a given message" $ do
      let sender = "Alice"
      let content = "test message"
      let message = createMessage sender content
      displayMessageString message `shouldBe` content