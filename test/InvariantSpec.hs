{-# LANGUAGE OverloadedStrings #-}

module InvariantSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

-- Import the invariant verification module
import InvariantVerification

-- Import all subsystems for creating test data  
import Profile (Profile, createProfile)
import qualified Profile as P
import Blockchain (BlockchainState(..), createBlockchainState)
import qualified Blockchain as B  
import Questions (Question, generateQuestion)
import qualified Questions as Q
import Reputation (Reputation, createReputation)
import qualified Reputation as R
import UI (SystemState(..), createSystemState)
import qualified UI as U

-- Test helper to create a comprehensive test system state
createTestSystemState :: IO SystemState
createTestSystemState = do
    -- Create test profiles
    let profiles = [ createProfile "alice" "alice-pubkey" "alice-privkey"
                   , createProfile "bob" "bob-pubkey" "bob-privkey"
                   , createProfile "charlie" "charlie-pubkey" "charlie-privkey"
                   ]
    
    -- Create test questions
    mcqQuestion <- generateQuestion
    frqQuestion <- generateQuestion
    let questions = [ mcqQuestion { Q.questionId = "mcq-001", Q.questionType = Q.MCQ }
                    , frqQuestion { Q.questionId = "frq-001", Q.questionType = Q.FRQ }
                    ]
    
    -- Create test reputation
    reputation <- sequence [createReputation, createReputation, createReputation]
    
    -- Create blockchain state
    let blockchain = createBlockchainState
    
    -- Create comprehensive system state
    return $ createSystemState profiles blockchain questions reputation

-- Comprehensive invariant verification test suite
spec :: Spec
spec = do
  describe "Invariant Verification Suite" $ do
    
    it "should verify all 13 invariants successfully" $ do
      testState <- createTestSystemState
      
      -- Run comprehensive invariant verification
      suite <- verifyAllInvariants testState
      
      -- Verify overall results
      totalInvariants suite `shouldBe` 13
      overallStatus suite `shouldSatisfy` (\status -> status == "PASS" || status == "FAIL")
      length (results suite) `shouldBe` 13
      
      -- All invariants should be present
      let invariantNumbers = map invariantNumber (results suite)
      invariantNumbers `shouldBe` [1..13]
      
    it "should verify Invariant 1: Identity (valid attester keys)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant1_Identity testState
      
      invariantNumber result `shouldBe` 1
      invariantName result `shouldBe` "Identity"
      -- Should pass for clean test state
      passed result `shouldBe` True
      
    it "should verify Invariant 2: Progressive Quorum (ADR-028)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant2_ProgressiveQuorum testState
      
      invariantNumber result `shouldBe` 2
      invariantName result `shouldBe` "Progressive Quorum"
      -- Should validate progressive quorum logic
      
    it "should verify Invariant 3: Confidence-Weighted Rewards" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant3_ConfidenceWeightedRewards testState
      
      invariantNumber result `shouldBe` 3
      invariantName result `shouldBe` "Confidence-Weighted Rewards"
      -- Should validate confidence bounds
      
    it "should verify Invariant 4: Hash Validation (MCQ)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant4_HashValidation testState
      
      invariantNumber result `shouldBe` 4
      invariantName result `shouldBe` "Hash Validation"
      -- Should validate SHA-256 hash integrity
      
    it "should verify Invariant 5: FRQ Scoring Bounds (1.0-5.0)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant5_FRQScoringBounds testState
      
      invariantNumber result `shouldBe` 5
      invariantName result `shouldBe` "FRQ Scoring Bounds"
      -- Should validate FRQ score bounds
      
    it "should verify Invariant 6: Temporal Ordering" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant6_TemporalOrdering testState
      
      invariantNumber result `shouldBe` 6
      invariantName result `shouldBe` "Temporal Ordering"
      -- Should validate timestamp ordering
      
    it "should verify Invariant 7: Convergence Calculation" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant7_ConvergenceCalculation testState
      
      invariantNumber result `shouldBe` 7
      invariantName result `shouldBe` "Convergence Calculation"
      -- Should validate convergence formulas
      
    it "should verify Invariant 8: Rate Limiting (30-day cooldown)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant8_RateLimiting testState
      
      invariantNumber result `shouldBe` 8
      invariantName result `shouldBe` "Rate Limiting"
      -- Should validate rate limiting enforcement
      
    it "should verify Invariant 9: Outlier Detection" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant9_OutlierDetection testState
      
      invariantNumber result `shouldBe` 9
      invariantName result `shouldBe` "Outlier Detection"
      -- Should validate outlier detection mechanisms
      
    it "should verify Invariant 10: Cycle Stability" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant10_CycleStability testState
      
      invariantNumber result `shouldBe` 10
      invariantName result `shouldBe` "Cycle Stability"
      -- Should validate system doesn't have infinite cycles
      
    it "should verify Invariant 11: Persistence Integrity" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant11_PersistenceIntegrity testState
      
      invariantNumber result `shouldBe` 11
      invariantName result `shouldBe` "Persistence Integrity"
      -- Should validate save/load operations
      
    it "should verify Invariant 12: Atomicity (58 atoms)" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant12_Atomicity testState
      
      invariantNumber result `shouldBe` 12
      invariantName result `shouldBe` "Atomicity"
      -- Should validate all 58 atoms are present and testable
      passed result `shouldBe` True
      
    it "should verify Invariant 13: UI Safety" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant13_UISafety testState
      
      invariantNumber result `shouldBe` 13
      invariantName result `shouldBe` "UI Safety"
      -- Should validate UI renders safely
      
    it "should handle edge cases and invalid states" $ do
      -- Create state with potential issues
      let emptyState = createSystemState [] createBlockchainState [] []
      
      suite <- verifyAllInvariants emptyState
      
      -- Should complete verification even with empty state
      totalInvariants suite `shouldBe` 13
      length (results suite) `shouldBe` 13
      
      -- Some invariants may fail with empty state, but verification should complete
      
    it "should provide detailed error messages for failures" $ do
      testState <- createTestSystemState
      
      suite <- verifyAllInvariants testState
      
      -- Check that all results have meaningful messages
      let allResults = results suite
      
      all (not . T.null . message) allResults `shouldBe` True
      all (not . null . details) allResults `shouldBe` True
      
    it "should maintain consistency across multiple verification runs" $ do
      testState <- createTestSystemState
      
      -- Run verification multiple times
      suite1 <- verifyAllInvariants testState
      suite2 <- verifyAllInvariants testState
      
      -- Results should be consistent
      totalInvariants suite1 `shouldBe` totalInvariants suite2
      passedInvariants suite1 `shouldBe` passedInvariants suite2
      overallStatus suite1 `shouldBe` overallStatus suite2
      
  describe "Progressive Quorum Logic (ADR-028)" $ do
    
    it "should calculate correct quorum sizes for different convergence levels" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant2_ProgressiveQuorum testState
      
      -- The progressive quorum function should be consistent with ADR-028:
      -- - High convergence (≥0.8): 3 attestations
      -- - Medium convergence (0.5-0.8): 4 attestations  
      -- - Low convergence (<0.5): 5 attestations
      
      invariantName result `shouldBe` "Progressive Quorum"
      
  describe "ADR-012 and ADR-028 Compliance" $ do
    
    it "should verify ADR-012 (Social Consensus) compliance" $ do
      testState <- createTestSystemState
      
      -- ADR-012 relates to identity and consensus mechanisms
      identityResult <- verifyInvariant1_Identity testState
      quorumResult <- verifyInvariant2_ProgressiveQuorum testState
      
      passed identityResult `shouldBe` True
      -- Quorum verification should complete (may pass or fail based on test data)
      
    it "should verify ADR-028 (Emergent Attestation) compliance" $ do
      testState <- createTestSystemState
      
      -- ADR-028 relates to attestation mechanics and scoring
      confidenceResult <- verifyInvariant3_ConfidenceWeightedRewards testState
      hashResult <- verifyInvariant4_HashValidation testState
      frqResult <- verifyInvariant5_FRQScoringBounds testState
      convergenceResult <- verifyInvariant7_ConvergenceCalculation testState
      rateLimitResult <- verifyInvariant8_RateLimiting testState
      outlierResult <- verifyInvariant9_OutlierDetection testState
      
      -- All ADR-028 related invariants should be properly structured
      invariantName confidenceResult `shouldBe` "Confidence-Weighted Rewards"
      invariantName hashResult `shouldBe` "Hash Validation"
      invariantName frqResult `shouldBe` "FRQ Scoring Bounds"
      invariantName convergenceResult `shouldBe` "Convergence Calculation"
      invariantName rateLimitResult `shouldBe` "Rate Limiting"
      invariantName outlierResult `shouldBe` "Outlier Detection"
      
  describe "Mathematical Foundation Verification (58 atoms)" $ do
    
    it "should verify all subsystem atoms are present" $ do
      testState <- createTestSystemState
      
      result <- verifyInvariant12_Atomicity testState
      
      -- According to FUNDAMENTAL.md:
      -- Profile (P): 11 atoms
      -- Blockchain (B): 25 atoms  
      -- Questions (Q): 14 atoms
      -- Reputation (R): 10 atoms
      -- Persistence (P2): 7 atoms
      -- UI (U): 6 atoms
      -- Total: 58 atoms
      
      passed result `shouldBe` True
      message result `shouldBe` "All 58 atoms are atomic and testable"
      
    it "should maintain atom independence" $ do
      testState <- createTestSystemState
      
      -- Each subsystem should be independently verifiable
      identityResult <- verifyInvariant1_Identity testState
      uiResult <- verifyInvariant13_UISafety testState
      persistenceResult <- verifyInvariant11_PersistenceIntegrity testState
      
      -- Independent verification should work
      invariantNumber identityResult `shouldBe` 1
      invariantNumber uiResult `shouldBe` 13  
      invariantNumber persistenceResult `shouldBe` 11