module MessageSpec (spec) where

import Test.Hspec
import Message
import Types

spec :: Spec
spec = do
  describe "createMessage" $ do
    it "creates a message with the specified content" $ do
      let content = "test message"
      let message = createMessage content
      messageContent message `shouldBe` content

  describe "displayMessageString" $ do
    it "displays the content of a given message" $ do
      let content = "test message"
      let message = createMessage content
      displayMessageString message `shouldBe` content