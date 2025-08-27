{-|
Module      : ProfileSpec
Description : Unit tests for Profile module using HSpec and QuickCheck
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Unit tests for the Profile module, testing P atoms functionality
including profile creation, validation, and updates.
-}

{-# LANGUAGE OverloadedStrings #-}

module ProfileSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)

import Profile

-- | Main test specification for Profile module
spec :: Spec
spec = do
  describe "Profile" $ do
    describe "createProfile" $ do
      it "creates a profile with given parameters" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "test@example.com" currentTime
        profileId profile `shouldBe` "test-id"
        profileName profile `shouldBe` "Test User"
        profileEmail profile `shouldBe` "test@example.com"
        profileCreatedAt profile `shouldBe` currentTime
        profileUpdatedAt profile `shouldBe` currentTime

    describe "updateProfile" $ do
      it "updates profile name and email with new timestamp" $ do
        currentTime <- getCurrentTime
        let originalProfile = createProfile "test-id" "Old Name" "old@example.com" currentTime
        laterTime <- getCurrentTime
        let updatedProfile = updateProfile originalProfile "New Name" "new@example.com" laterTime
        
        profileId updatedProfile `shouldBe` "test-id"
        profileName updatedProfile `shouldBe` "New Name"
        profileEmail updatedProfile `shouldBe` "new@example.com"
        profileCreatedAt updatedProfile `shouldBe` currentTime
        profileUpdatedAt updatedProfile `shouldBe` laterTime

    describe "validateProfile" $ do
      it "validates a correct profile" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "test@example.com" currentTime
        validateProfile profile `shouldBe` Right profile

      it "rejects profile with empty name" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "" "test@example.com" currentTime
        validateProfile profile `shouldBe` Left "Profile name cannot be empty"

      it "rejects profile with empty email" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "" currentTime
        validateProfile profile `shouldBe` Left "Profile email cannot be empty"

-- | QuickCheck property tests
prop_profileIdPreserved :: Text -> Text -> Text -> Property
prop_profileIdPreserved pid name email = 
  not (T.null name) && not (T.null email) ==> \currentTime ->
    let profile = createProfile pid name email currentTime
    in profileId profile == pid

prop_validateAcceptsNonEmptyFields :: Text -> Text -> Text -> Property
prop_validateAcceptsNonEmptyFields pid name email = 
  not (T.null name) && not (T.null email) ==> \currentTime ->
    let profile = createProfile pid name email currentTime
    in case validateProfile profile of
         Right _ -> True
         Left _ -> False
