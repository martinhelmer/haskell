module Day08Spec where

import SpecHelper

spec :: Spec
spec = 
  describe "myRead" $ do
    context "with plus" $ do
      it "should be 55" $
        myRead "+55" `shouldBe` 55
      it "should be 0" $
        myRead "+0" `shouldBe` 0   

    context "with minus" $ do
      it "should be -55" $
        myRead "-55" `shouldBe` (-55)
      it "should be 0" $
        myRead "-0" `shouldBe` 0  

main :: IO ()
main = hspec spec
    