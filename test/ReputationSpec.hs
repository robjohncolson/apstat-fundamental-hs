{-|
Module      : ReputationSpec
Description : Unit tests for Reputation module R atoms (Invariant 12: Atomicity)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Atomic tests for the Reputation module with 10 R atoms, ensuring each atom
is independently testable per Invariant 12 of the mathematical specification.
-}

{-# LANGUAGE OverloadedStrings #-}

module ReputationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isLeft, isRight)

import Reputation
import Blockchain (Confidence(..), validateConfidence)
import qualified Blockchain as B

-- | Main test specification for Reputation module (Invariant 12: Atomicity)
spec :: Spec
spec = do
  describe "Reputation R Atoms" $ do
    
    -- Test individual R data atoms for atomicity (Invariant 12)
    describe "R Data Atoms (10 atoms)" $ do
      
      describe "R atom 1: reputationScore" $ do
        it "stores and retrieves reputationScore independently" $ do
          rep <- createReputation
          reputationScore rep `shouldSatisfy` (>= 0.0)
          reputationScore rep `shouldSatisfy` (<= 1000.0)
          reputationScore rep `shouldBe` 100.0
          
      describe "R atom 2: minorityBonus" $ do
        it "stores and retrieves minorityBonus independently" $ do
          rep <- createReputation
          minorityBonus rep `shouldBe` 0.5
          minorityBonus rep `shouldSatisfy` (>= 0.0)
          
      describe "R atom 3: decayRate" $ do
        it "stores and retrieves decayRate independently" $ do
          rep <- createReputation
          decayRate rep `shouldBe` 0.1
          decayRate rep `shouldSatisfy` (>= 0.0)
          decayRate rep `shouldSatisfy` (<= 1.0)
          
      describe "R atom 4: timeWindow" $ do
        it "stores and retrieves timeWindow independently" $ do
          rep <- createReputation
          timeWindow rep `shouldBe` 86400
          timeWindow rep `shouldSatisfy` (> 0)
          
      describe "R atom 5: consensusThreshold" $ do
        it "stores and retrieves consensusThreshold independently" $ do
          rep <- createReputation
          consensusThreshold rep `shouldBe` 0.6
          consensusThreshold rep `shouldSatisfy` (>= 0.0)
          consensusThreshold rep `shouldSatisfy` (<= 1.0)
          
      describe "R atom 6: baseAccuracyScore" $ do
        it "stores and retrieves baseAccuracyScore independently" $ do
          rep <- createReputation
          baseAccuracyScore rep `shouldBe` 0.5
          baseAccuracyScore rep `shouldSatisfy` (>= 0.0)
          baseAccuracyScore rep `shouldSatisfy` (<= 1.0)
          
      describe "R atom 7: streakBonus" $ do
        it "stores and retrieves streakBonus independently" $ do
          rep <- createReputation
          streakBonus rep `shouldBe` 0
          streakBonus rep `shouldSatisfy` (>= 0)
          
      describe "R atom 8: socialScore" $ do
        it "stores and retrieves socialScore independently" $ do
          rep <- createReputation
          socialScore rep `shouldBe` 0.0
          socialScore rep `shouldSatisfy` (>= 0.0)
          
      describe "R atom 9: leaderboardRank" $ do
        it "stores and retrieves leaderboardRank independently" $ do
          rep <- createReputation
          leaderboardRank rep `shouldBe` Nothing
          
      describe "R atom 10: lastUpdated" $ do
        it "stores and retrieves lastUpdated independently" $ do
          rep <- createReputation
          lastUpdated rep `shouldSatisfy` (> 0.0)
    
    -- Test R function atoms for atomicity
    describe "R Function Atoms" $ do
      
      describe "updateReputationScore (Invariant 3: Confidence Weighting)" $ do
        it "applies confidence weighting correctly" $ do
          rep <- createReputation
          let confidence = B.Confidence 0.8
              isMinority = False
              accuracy = 1.0
          updatedRep <- updateReputationScore rep confidence isMinority accuracy
          reputationScore updatedRep `shouldSatisfy` (> reputationScore rep)
          
        it "applies minority bonus correctly" $ do
          rep <- createReputation
          let confidence = B.Confidence 0.8
              accuracy = 1.0
          repNormal <- updateReputationScore rep confidence False accuracy
          repMinority <- updateReputationScore rep confidence True accuracy
          reputationScore repMinority `shouldSatisfy` (> reputationScore repNormal)
          
        it "enforces reputation score bounds (0-1000)" $ do
          rep <- createReputation
          let confidence = B.Confidence 1.0
              isMinority = True
              highAccuracy = 10.0  -- Artificially high to test bounds
          updatedRep <- updateReputationScore rep confidence isMinority highAccuracy
          reputationScore updatedRep `shouldSatisfy` (<= 1000.0)
          reputationScore updatedRep `shouldSatisfy` (>= 0.0)
      
      describe "calculateMinorityBonus" $ do
        it "returns 1.0 for non-minority positions" $ do
          let bonus = calculateMinorityBonus 0.5 False
          bonus `shouldBe` 1.0
          
        it "returns bonus multiplier for minority positions" $ do
          let bonus = calculateMinorityBonus 0.5 True
          bonus `shouldBe` 1.5
      
      describe "applyTimeDecay" $ do
        it "reduces score over time" $ do
          let originalScore = 100.0
              decayRate' = 0.1
              timeDiff = 86400.0  -- 1 day
              decayedScore = applyTimeDecay originalScore decayRate' timeDiff
          decayedScore `shouldSatisfy` (< originalScore)
          
        it "preserves score at time 0" $ do
          let originalScore = 100.0
              decayRate' = 0.1
              timeDiff = 0.0
              decayedScore = applyTimeDecay originalScore decayRate' timeDiff
          decayedScore `shouldBe` originalScore
      
      describe "updateStreak" $ do
        it "increments streak on correct prediction" $ do
          let newStreak = updateStreak 5 True
          newStreak `shouldBe` 6
          
        it "resets streak on incorrect prediction" $ do
          let newStreak = updateStreak 5 False
          newStreak `shouldBe` 0
      
      describe "calculateLeaderboardRank" $ do
        it "calculates correct rank among peers" $ do
          rep1 <- createReputation
          rep2 <- createReputation
          rep3 <- createReputation
          let rep1' = rep1 { reputationScore = 200.0 }
              rep2' = rep2 { reputationScore = 150.0 }
              rep3' = rep3 { reputationScore = 100.0 }
              allReps = [rep1', rep2', rep3']
              rank = calculateLeaderboardRank allReps rep2'
          rank `shouldBe` Just 2
          
        it "handles empty list" $ do
          rep <- createReputation
          let rank = calculateLeaderboardRank [] rep
          rank `shouldBe` Nothing
    
    -- Test validation functions for bounds enforcement
    describe "Validation Functions" $ do
      
      describe "validateReputationScore" $ do
        it "accepts valid scores" $ do
          validateReputationScore 500.0 `shouldSatisfy` isRight
          validateReputationScore 0.0 `shouldSatisfy` isRight
          validateReputationScore 1000.0 `shouldSatisfy` isRight
          
        it "rejects invalid scores" $ do
          validateReputationScore (-1.0) `shouldSatisfy` isLeft
          validateReputationScore 1001.0 `shouldSatisfy` isLeft
      
      describe "validateDecayRate" $ do
        it "accepts valid decay rates" $ do
          validateDecayRate 0.5 `shouldSatisfy` isRight
          validateDecayRate 0.0 `shouldSatisfy` isRight
          validateDecayRate 1.0 `shouldSatisfy` isRight
          
        it "rejects invalid decay rates" $ do
          validateDecayRate (-0.1) `shouldSatisfy` isLeft
          validateDecayRate 1.1 `shouldSatisfy` isLeft
      
      describe "validateReputation" $ do
        it "accepts valid reputation objects" $ do
          rep <- createReputation
          validateReputation rep `shouldSatisfy` isRight
          
        it "rejects invalid reputation score bounds" $ do
          rep <- createReputation
          let invalidRep = rep { reputationScore = -1.0 }
          validateReputation invalidRep `shouldSatisfy` isLeft
          
        it "rejects invalid decay rate bounds" $ do
          rep <- createReputation
          let invalidRep = rep { decayRate = 1.5 }
          validateReputation invalidRep `shouldSatisfy` isLeft

    -- Property-based tests for invariant preservation
    describe "Property-Based Tests (Invariants)" $ do
      
      it "reputation scores always stay within bounds after updates" $
        property $ \conf acc -> monadicIO $ do
          rep <- run createReputation
          let confidence = B.Confidence (max 0.0 (min 1.0 conf))
              accuracy = max 0.0 (min 1.0 acc)
              isMinority = False
          updatedRep <- run $ updateReputationScore rep confidence isMinority accuracy
          assert $ reputationScore updatedRep >= 0.0 && reputationScore updatedRep <= 1000.0
          
      it "time decay never increases reputation score" $
        property $ \originalScore rate timeDiff ->
          let boundedScore = max 0.0 (min 1000.0 originalScore)
              boundedRate = max 0.0 (min 1.0 rate)
              boundedTime = max 0.0 timeDiff
              decayed = applyTimeDecay boundedScore boundedRate boundedTime
          in decayed <= boundedScore
          
      it "streak bonus is always non-negative" $
        property $ \currentStreak isCorrect ->
          let newStreak = updateStreak (abs currentStreak) isCorrect
          in newStreak >= 0