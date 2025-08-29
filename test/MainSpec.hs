{-# LANGUAGE OverloadedStrings #-}

module MainSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Main Integration Tests" $ do
    it "should compile and run basic test" $ do
      1 + 1 `shouldBe` 2