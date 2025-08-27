{-|
Module      : ProfileSpec
Description : Unit tests for Profile module P atoms (Invariant 12: Atomicity)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Atomic tests for the Profile module with 11 P atoms, ensuring each atom
is independently testable per Invariant 12 of the mathematical specification.
-}

{-# LANGUAGE OverloadedStrings #-}

module ProfileSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Profile

-- | Main test specification for Profile module (Invariant 12: Atomicity)
spec :: Spec
spec = do
  describe "Profile P Atoms" $ do
    
    -- Test individual P atoms for atomicity (Invariant 12)
    describe "P atom 1: userId" $ do
      it "stores and retrieves userId independently" $ do
        let profile = createProfile "test-user" "pk123" "sk123"
        userId profile `shouldBe` "test-user"
    
    describe "P atom 2: pubKey" $ do
      it "stores and retrieves pubKey independently" $ do
        let profile = createProfile "user" "test-pubkey" "sk123"
        pubKey profile `shouldBe` "test-pubkey"
    
    describe "P atom 3: privKey" $ do
      it "stores and retrieves privKey independently" $ do
        let profile = createProfile "user" "pk123" "test-privkey"
        privKey profile `shouldBe` "test-privkey"
    
    describe "P atom 4: reputationScore" $ do
      it "initializes with default reputation" $ do
        let profile = createProfile "user" "pk123" "sk123"
        reputationScore profile `shouldBe` 100.0
    
    describe "P atom 5: attestationHistory" $ do
      it "initializes empty and updates independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        attestationHistory profile `shouldBe` []
        
      it "updates history with new question ID" $ do
        let profile = createProfile "user" "pk123" "sk123"
            updated = updateProfile profile (True, "Q1")
        attestationHistory updated `shouldBe` ["Q1"]
    
    describe "P atom 6: confidenceLevel" $ do
      it "stores and retrieves confidenceLevel independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        confidenceLevel profile `shouldBe` 0.5
    
    describe "P atom 7: lastAttestationTimestamp" $ do
      it "stores and retrieves timestamp independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        lastAttestationTimestamp profile `shouldBe` 0.0
    
    describe "P atom 8: outlierFlags" $ do
      it "stores and retrieves outlierFlags independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        outlierFlags profile `shouldBe` 0
    
    describe "P atom 9: minorityBonusCount" $ do
      it "stores and retrieves minorityBonusCount independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        minorityBonusCount profile `shouldBe` 0
    
    describe "P atom 10: streak" $ do
      it "initializes at zero" $ do
        let profile = createProfile "user" "pk123" "sk123"
        streak profile `shouldBe` 0
        
      it "increments streak on correct answer" $ do
        let profile = createProfile "user" "pk123" "sk123"
            updated = updateProfile profile (True, "Q1")
        streak updated `shouldBe` 1
        
      it "resets streak on incorrect answer" $ do
        let profile = createProfile "user" "pk123" "sk123"
            withStreak = updateProfile profile (True, "Q1")
            resetStreak = updateProfile withStreak (False, "Q2")
        streak resetStreak `shouldBe` 0
    
    describe "P atom 11: archetype" $ do
      it "stores and retrieves archetype independently" $ do
        let profile = createProfile "user" "pk123" "sk123"
        archetype profile `shouldBe` Explorers

  -- Test P function atoms
  describe "Profile Functions" $ do
    
    describe "createProfile" $ do
      it "creates profile with all 11 P atoms initialized" $ do
        let profile = createProfile "alice" "pk_alice" "sk_alice"
        userId profile `shouldBe` "alice"
        pubKey profile `shouldBe` "pk_alice"
        privKey profile `shouldBe` "sk_alice"
        reputationScore profile `shouldBe` 100.0
        attestationHistory profile `shouldBe` []
        streak profile `shouldBe` 0
        archetype profile `shouldBe` Explorers
    
    describe "updateProfile (P function atom)" $ do
      it "updates attestation history by prepending question ID" $ do
        let profile = createProfile "user" "pk123" "sk123"
            updated1 = updateProfile profile (True, "Q1")
            updated2 = updateProfile updated1 (False, "Q2")
        attestationHistory updated2 `shouldBe` ["Q2", "Q1"]
        
      it "maintains streak for consecutive correct answers" $ do
        let profile = createProfile "user" "pk123" "sk123"
            step1 = updateProfile profile (True, "Q1")
            step2 = updateProfile step1 (True, "Q2")
            step3 = updateProfile step2 (True, "Q3")
        streak step3 `shouldBe` 3
        
      it "resets streak on incorrect answer" $ do
        let profile = createProfile "user" "pk123" "sk123"
            step1 = updateProfile profile (True, "Q1") 
            step2 = updateProfile step1 (True, "Q2")
            step3 = updateProfile step2 (False, "Q3")
        streak step3 `shouldBe` 0

  -- Test Invariant 1: Identity validation  
  describe "Invariant 1: Identity" $ do
    it "validates profile with non-empty pubkey" $ do
      let profile = createProfile "user" "valid-pubkey" "sk123"
      validateProfile profile `shouldBe` Right profile
      
    it "rejects profile with empty pubkey" $ do
      let profile = createProfile "user" "" "sk123"
      validateProfile profile `shouldBe` Left "Invalid attester public key: empty pubkey"
      
    it "rejects profile with empty userId" $ do  
      let profile = createProfile "" "pk123" "sk123"
      validateProfile profile `shouldBe` Left "Invalid user ID: empty userId"
  
  -- Include new P function atom tests
  testNewFunctions

-- | Property-based tests for atomicity (Invariant 12)
prop_updateProfilePreservesOtherAtoms :: String -> String -> String -> Bool -> String -> Bool
prop_updateProfilePreservesOtherAtoms uid pk sk isMatch qid =
  let original = createProfile uid pk sk
      updated = updateProfile original (isMatch, qid)
  in userId updated == userId original &&
     pubKey updated == pubKey original &&
     privKey updated == privKey original &&
     reputationScore updated == reputationScore original &&
     archetype updated == archetype original

prop_streakCalculationCorrect :: [Bool] -> Bool
prop_streakCalculationCorrect matches =
  let profile = createProfile "user" "pk123" "sk123"
      applyMatches prof [] = prof
      applyMatches prof (m:ms) = applyMatches (updateProfile prof (m, "Q")) ms
      final = applyMatches profile matches
      expectedStreak = length $ takeWhile id $ reverse matches
  in streak final == expectedStreak

-- | Test new P function atoms
testNewFunctions :: Spec
testNewFunctions = do
  describe "calculateArchetype (P function atom)" $ do
    it "classifies Aces archetype correctly" $ do
      let archetype = calculateArchetype 0.95 2500 75 0.6
      archetype `shouldBe` Aces
      
    it "classifies Strategists archetype correctly" $ do
      let archetype = calculateArchetype 0.88 7000 50 0.8
      archetype `shouldBe` Strategists
      
    it "classifies Socials archetype correctly" $ do
      let archetype = calculateArchetype 0.7 5000 60 0.9
      archetype `shouldBe` Socials
      
    it "classifies Learners archetype correctly" $ do  
      let archetype = calculateArchetype 0.65 4000 25 0.4
      archetype `shouldBe` Learners
      
    it "defaults to Explorers archetype" $ do
      let archetype = calculateArchetype 0.5 3000 5 0.2
      archetype `shouldBe` Explorers

  describe "updateReputationScore (P function atom)" $ do
    it "increases reputation for correct answers" $ do
      let profile = createProfile "user" "pk123" "sk123"
          updated = updateReputationScore profile 0.8 False 2.0
      reputationScore updated `shouldSatisfy` (> reputationScore profile)
      
    it "applies minority bonus correctly" $ do
      let profile = createProfile "user" "pk123" "sk123"
          normalUpdate = updateReputationScore profile 0.8 False 0.0
          minorityUpdate = updateReputationScore profile 0.8 True 0.0
      reputationScore minorityUpdate `shouldSatisfy` (> reputationScore normalUpdate)
      
    it "enforces bounds (0-1000)" $ do
      let profile = createProfile "user" "pk123" "sk123"
          highProfile = profile { reputationScore = 950.0 }
          maxed = updateReputationScore highProfile 1.0 True 10.0
      reputationScore maxed `shouldSatisfy` (<= 1000.0)
      
    it "handles negative reputation correctly" $ do
      let profile = createProfile "user" "pk123" "sk123"
          decreased = updateReputationScore profile 0.2 False 0.0
      reputationScore decreased `shouldSatisfy` (>= 0.0)

  describe "updateProfileWithConsensus (P function atom)" $ do
    it "updates all relevant P atoms correctly" $ do
      let profile = createProfile "user" "pk123" "sk123"
          updated = updateProfileWithConsensus profile True "Q1" 0.8 12345.0 True True
      attestationHistory updated `shouldBe` ["Q1"]
      lastAttestationTimestamp updated `shouldBe` 12345.0
      streak updated `shouldBe` 1
      outlierFlags updated `shouldBe` 1
      minorityBonusCount updated `shouldBe` 1
      
    it "updates confidence level with weighted average" $ do
      let profile = createProfile "user" "pk123" "sk123"
          updated = updateProfileWithConsensus profile True "Q1" 0.9 12345.0 False False
      confidenceLevel updated `shouldNotBe` confidenceLevel profile

  describe "validateConfidence" $ do
    it "accepts valid confidence values" $ do
      validateConfidence 0.5 `shouldBe` Right 0.5
      validateConfidence 0.0 `shouldBe` Right 0.0
      validateConfidence 1.0 `shouldBe` Right 1.0
      
    it "rejects invalid confidence values" $ do
      validateConfidence (-0.1) `shouldSatisfy` isLeft
      validateConfidence 1.1 `shouldSatisfy` isLeft
      
  describe "Enhanced validateProfile" $ do
    it "validates reputation bounds" $ do
      let profile = (createProfile "user" "pk123" "sk123") { reputationScore = -1.0 }
      validateProfile profile `shouldSatisfy` isLeft
      
    it "validates confidence bounds" $ do  
      let profile = (createProfile "user" "pk123" "sk123") { confidenceLevel = 2.0 }
      validateProfile profile `shouldSatisfy` isLeft

-- Helper function for Either tests
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False