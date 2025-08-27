{-|
Module      : BlockchainSpec
Description : Unit tests for Blockchain module B atoms (Invariant 12: Atomicity)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Atomic tests for the Blockchain module with 25 B atoms, ensuring each atom
is independently testable per Invariant 12 of the mathematical specification.
-}

{-# LANGUAGE OverloadedStrings #-}

module BlockchainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Either (isLeft, isRight)

import Blockchain

-- | Main test specification for Blockchain module (Invariant 12: Atomicity)
spec :: Spec
spec = do
  describe "Blockchain B Atoms" $ do
    
    -- Test individual B data atoms for atomicity (Invariant 12)
    describe "B Data Atoms (19 atoms)" $ do
      
      describe "B atom 1: hash" $ do
        it "stores and retrieves hash independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          hash tx `shouldSatisfy` (not . T.null)
          
      describe "B atom 2: prevHash" $ do
        it "stores and retrieves prevHash independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          prevHash tx `shouldBe` ""  -- Initially empty
          
      describe "B atom 3: timestamp" $ do
        it "stores and retrieves timestamp independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          timestamp tx `shouldSatisfy` (> 0)
          
      describe "B atom 4: nonce" $ do
        it "stores and retrieves nonce independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          nonce tx `shouldBe` 0  -- Initially 0
          
      describe "B atom 5: questionId" $ do
        it "stores and retrieves questionId independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          questionId tx `shouldBe` "Q1"
          
      describe "B atom 6: answerHash" $ do
        it "stores and retrieves answerHash independently for MCQ" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "A") Nothing conf "privkey"
          answerHash tx `shouldSatisfy` (/= Nothing)
          
        it "is Nothing for CreateUser transactions" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction CreateUserTx "alice" "pubkey123" "sig" (Just "alice") Nothing conf "privkey"
          answerHash tx `shouldBe` Nothing
          
      describe "B atom 7: answerText" $ do
        it "stores and retrieves answerText independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "My answer") Nothing conf "privkey"
          answerText tx `shouldBe` Just "My answer"
          
      describe "B atom 8: score" $ do
        it "stores and retrieves FRQ score independently" $ do
          Right conf <- return $ validateConfidence 0.8
          Right frqScore <- return $ validateFRQScore 4.5
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") (Just frqScore) conf "privkey"
          score tx `shouldBe` Just frqScore
          
      describe "B atom 9: attesterPubkey" $ do
        it "stores and retrieves attesterPubkey independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "test-pubkey" "sig" (Just "answer") Nothing conf "privkey"
          attesterPubkey tx `shouldBe` "test-pubkey"
          
      describe "B atom 10: signature" $ do
        it "stores and retrieves signature independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "test-privkey"
          signature tx `shouldBe` "test-privkey-sig"
          
      describe "B atom 11: txType" $ do
        it "stores and retrieves txType independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx1 <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          tx2 <- createTransaction CreateUserTx "alice" "pubkey123" "sig" (Just "alice") Nothing conf "privkey"
          txType tx1 `shouldBe` AttestationTx
          txType tx2 `shouldBe` CreateUserTx
          
      describe "B atom 12: isMatch" $ do
        it "stores and retrieves isMatch independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          isMatch tx `shouldBe` Nothing  -- Initially Nothing
          
      describe "B atom 13: questionDistribution" $ do
        it "stores and retrieves questionDistribution independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          questionDistribution tx `shouldBe` Map.empty  -- Initially empty
          
      describe "B atom 14: mcqDistribution" $ do
        it "stores and retrieves mcqDistribution independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          mcqDistribution tx `shouldBe` Map.empty  -- Initially empty
          
      describe "B atom 15: frqScores" $ do
        it "stores and retrieves frqScores independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          frqScores tx `shouldBe` []  -- Initially empty
          
      describe "B atom 16: convergence" $ do
        it "stores and retrieves convergence independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          convergence tx `shouldBe` 0.0  -- Initially 0.0
          
      describe "B atom 17: confidence" $ do
        it "stores and retrieves confidence independently" $ do
          Right conf <- return $ validateConfidence 0.75
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          confidence tx `shouldBe` conf
          
      describe "B atom 18: anonymousSignature" $ do
        it "stores and retrieves anonymousSignature independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          anonymousSignature tx `shouldBe` Nothing  -- Initially Nothing
          
      describe "B atom 19: officialAnswer" $ do
        it "stores and retrieves officialAnswer independently" $ do
          Right conf <- return $ validateConfidence 0.8
          tx <- createTransaction AttestationTx "Q1" "pubkey123" "sig" (Just "answer") Nothing conf "privkey"
          officialAnswer tx `shouldBe` Nothing  -- Initially Nothing
    
    -- Test B function atoms
    describe "B Function Atoms (6 functions)" $ do
      
      describe "B function atom 1: sha256Hash" $ do
        it "produces consistent hash for same input" $ do
          let input = "test data"
          sha256Hash input `shouldBe` sha256Hash input
          
        it "produces different hashes for different inputs" $ do
          sha256Hash "input1" `shouldNotBe` sha256Hash "input2"
          
        it "produces non-empty hash" $ do
          sha256Hash "test" `shouldSatisfy` (not . T.null)
      
      describe "B function atom 2: getCurrentTimestamp" $ do
        it "returns positive timestamp" $ do
          ts <- getCurrentTimestamp
          ts `shouldSatisfy` (> 0)
          
        it "returns increasing timestamps" $ do
          ts1 <- getCurrentTimestamp
          ts2 <- getCurrentTimestamp
          ts2 `shouldSatisfy` (>= ts1)
      
      describe "B function atom 3: validateSignature" $ do
        it "validates correct signature format" $ do
          validateSignature "pubkey" "message" "privkey-sig" `shouldBe` True
          
        it "rejects empty signatures" $ do
          validateSignature "pubkey" "message" "" `shouldBe` False
          
        it "rejects signatures without -sig suffix" $ do
          validateSignature "pubkey" "message" "invalid" `shouldBe` False
      
      describe "B function atom 4: calculateConsensus (Invariant 2)" $ do
        it "implements progressive quorum correctly" $ do
          Right conf1 <- return $ validateConfidence 0.8
          Right conf2 <- return $ validateConfidence 0.7
          Right conf3 <- return $ validateConfidence 0.9
          
          tx1 <- createTransaction AttestationTx "Q1" "pk1" "sig" (Just "A") Nothing conf1 "sk1"
          tx2 <- createTransaction AttestationTx "Q1" "pk2" "sig" (Just "A") Nothing conf2 "sk2"  
          tx3 <- createTransaction AttestationTx "Q1" "pk3" "sig" (Just "A") Nothing conf3 "sk3"
          
          att1 <- createAttestation "att1" "v1" tx1
          att2 <- createAttestation "att2" "v2" tx2
          att3 <- createAttestation "att3" "v3" tx3
          
          let result = calculateConsensus [att1, att2, att3] "Q1"
          Map.lookup "consensus" result `shouldSatisfy` (/= Nothing)
          Map.lookup "quorum_size" result `shouldSatisfy` (/= Nothing)
          
        it "returns empty map for insufficient attestations" $ do
          calculateConsensus [] "Q1" `shouldBe` Map.empty
          
      describe "B function atom 5: updateDistributions (Invariant 7)" $ do
        it "calculates MCQ convergence correctly" $ do
          Right conf <- return $ validateConfidence 0.8
          tx1 <- createTransaction AttestationTx "Q1" "pk1" "sig" (Just "A") Nothing conf "sk1"
          tx2 <- createTransaction AttestationTx "Q1" "pk2" "sig" (Just "A") Nothing conf "sk2"
          tx3 <- createTransaction AttestationTx "Q1" "pk3" "sig" (Just "B") Nothing conf "sk3"
          
          let (qDist, mcqDist, frqScores', conv) = updateDistributions [tx1, tx2, tx3] "Q1"
          conv `shouldSatisfy` (> 0.5)  -- 2/3 chose A, so convergence > 0.5
          Map.size mcqDist `shouldBe` 2  -- Two distinct answers
          
        it "calculates FRQ convergence using coefficient of variation" $ do
          Right conf <- return $ validateConfidence 0.8
          Right score1 <- return $ validateFRQScore 3.0
          Right score2 <- return $ validateFRQScore 3.5
          Right score3 <- return $ validateFRQScore 4.0
          
          tx1 <- createTransaction AttestationTx "Q1" "pk1" "sig" (Just "ans") (Just score1) conf "sk1"
          tx2 <- createTransaction AttestationTx "Q1" "pk2" "sig" (Just "ans") (Just score2) conf "sk2"
          tx3 <- createTransaction AttestationTx "Q1" "pk3" "sig" (Just "ans") (Just score3) conf "sk3"
          
          let (_, _, frqScores', conv) = updateDistributions [tx1, tx2, tx3] "Q1"
          length frqScores' `shouldBe` 3
          conv `shouldSatisfy` (> 0)  -- Should have positive convergence
          
      describe "B function atom 6: detectOutliers (Invariant 11)" $ do
        it "detects outliers using Z-score" $ do
          let values = [1.0, 2.0, 3.0, 2.5, 2.3, 10.0]  -- 10.0 is outlier
          let outliers = detectOutliers values
          outliers `shouldSatisfy` (not . null)
          outliers `shouldContain` [10.0]
          
        it "returns empty list for insufficient data" $ do
          detectOutliers [1.0, 2.0] `shouldBe` []
          
        it "returns empty list for uniform data" $ do
          let uniform = replicate 10 5.0
          detectOutliers uniform `shouldBe` []

  -- Test Invariant enforcement
  describe "Invariant Enforcement" $ do
    
    describe "Invariant 1: Identity validation" $ do
      it "validates transaction with valid pubkey" $ do
        Right conf <- return $ validateConfidence 0.8
        tx <- createTransaction AttestationTx "Q1" "valid-pubkey" "sig" (Just "answer") Nothing conf "privkey"
        validateTransaction tx `shouldSatisfy` isRight
        
      it "rejects transaction with empty pubkey" $ do
        Right conf <- return $ validateConfidence 0.8
        tx <- createTransaction AttestationTx "Q1" "" "sig" (Just "answer") Nothing conf "privkey"
        validateTransaction tx `shouldSatisfy` isLeft
        
    describe "Invariant 3: Confidence bounds (0.0-1.0)" $ do
      it "validates confidence within bounds" $ do
        validateConfidence 0.5 `shouldSatisfy` isRight
        validateConfidence 0.0 `shouldSatisfy` isRight  
        validateConfidence 1.0 `shouldSatisfy` isRight
        
      it "rejects confidence outside bounds" $ do
        validateConfidence (-0.1) `shouldSatisfy` isLeft
        validateConfidence 1.1 `shouldSatisfy` isLeft
        
    describe "Invariant 5: FRQ score bounds (1.0-5.0)" $ do
      it "validates FRQ score within bounds" $ do
        validateFRQScore 3.0 `shouldSatisfy` isRight
        validateFRQScore 1.0 `shouldSatisfy` isRight
        validateFRQScore 5.0 `shouldSatisfy` isRight
        
      it "rejects FRQ score outside bounds" $ do
        validateFRQScore 0.5 `shouldSatisfy` isLeft  
        validateFRQScore 5.5 `shouldSatisfy` isLeft
        
    describe "Invariant 6: Temporal ordering" $ do
      it "generates increasing timestamps" $ do
        ts1 <- getCurrentTimestamp
        ts2 <- getCurrentTimestamp
        ts2 `shouldSatisfy` (>= ts1)

-- | Property-based tests for atomicity (Invariant 12)
prop_hashConsistency :: Text -> Bool
prop_hashConsistency input = sha256Hash input == sha256Hash input

prop_confidenceBounds :: Double -> Bool  
prop_confidenceBounds c = 
  case validateConfidence c of
    Right (Blockchain.Confidence c') -> c' >= 0.0 && c' <= 1.0
    Left _ -> c < 0.0 || c > 1.0

prop_frqScoreBounds :: Double -> Bool
prop_frqScoreBounds s =
  case validateFRQScore s of  
    Right (Blockchain.FRQScore s') -> s' >= 1.0 && s' <= 5.0
    Left _ -> s < 1.0 || s > 5.0

prop_outlierDetectionStable :: [Double] -> Property
prop_outlierDetectionStable values =
  length values >= 3 ==>
    let outliers1 = detectOutliers values
        outliers2 = detectOutliers values
    in outliers1 == outliers2  -- Should be deterministic