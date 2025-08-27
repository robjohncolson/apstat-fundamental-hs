{-|
Module      : PersistenceSpec
Description : Unit tests for Persistence module using HSpec and QuickCheck
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Unit tests for the Persistence module, testing P2 atoms functionality
including state serialization, JSON encoding/decoding, and file operations.
-}

{-# LANGUAGE OverloadedStrings #-}

module PersistenceSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Aeson (encode, decode)

import Persistence
import Profile (createProfile)
import Blockchain (BlockchainState(..))

-- | Main test specification for Persistence module
spec :: Spec
spec = do
  describe "Persistence" $ do
    describe "saveState and loadState" $ do
      it "round-trips application state through JSON" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "test@example.com" currentTime
        let blockchain = BlockchainState [] [] [] 0.75
        let appState = AppState [profile] blockchain "1.0.0" "test metadata"
        
        let encoded = saveState appState
        let decoded = loadState encoded
        
        decoded `shouldBe` Right appState

      it "handles empty state correctly" $ do
        let blockchain = BlockchainState [] [] [] 0.75
        let appState = AppState [] blockchain "1.0.0" ""
        
        let encoded = saveState appState
        let decoded = loadState encoded
        
        decoded `shouldBe` Right appState

    describe "encodeState and decodeState" $ do
      it "are aliases for saveState and loadState" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "test@example.com" currentTime
        let blockchain = BlockchainState [] [] [] 0.75
        let appState = AppState [profile] blockchain "1.0.0" "test metadata"
        
        encodeState appState `shouldBe` saveState appState
        decodeState (encodeState appState) `shouldBe` loadState (saveState appState)

    describe "JSON serialization invariants" $ do
      it "preserves all fields through serialization" $ do
        currentTime <- getCurrentTime
        let profile = createProfile "test-id" "Test User" "test@example.com" currentTime
        let blockchain = BlockchainState [] [] [] 0.75
        let appState = AppState [profile] blockchain "1.0.0" "test metadata"
        
        case loadState (saveState appState) of
          Right decoded -> do
            appStateProfiles decoded `shouldBe` appStateProfiles appState
            appStateVersion decoded `shouldBe` appStateVersion appState
            appStateMetadata decoded `shouldBe` appStateMetadata appState
          Left err -> expectationFailure $ "Decoding failed: " ++ err

-- | QuickCheck property tests
prop_stateRoundTrip :: Text -> Text -> Property
prop_stateRoundTrip version metadata = \currentTime ->
  let blockchain = BlockchainState [] [] [] 0.75
      appState = AppState [] blockchain version metadata
      encoded = saveState appState
      decoded = loadState encoded
  in decoded == Right appState

prop_nonEmptyVersionPreserved :: Text -> Property
prop_nonEmptyVersionPreserved version = 
  not (T.null version) ==> \currentTime ->
    let blockchain = BlockchainState [] [] [] 0.75
        appState = AppState [] blockchain version ""
        encoded = saveState appState
        decoded = loadState encoded
    in case decoded of
         Right state -> appStateVersion state == version
         Left _ -> False
