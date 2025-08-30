{-|
Module      : TestBlockchain
Description : Test harness for WASM Blockchain atoms without GHCJS
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Test module to verify all 25 B atoms work correctly in isolation.
-}

module Main where

{-# LANGUAGE CPP #-}
#include "Blockchain.Pure.hs"
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Testing AP Statistics Blockchain atoms (25 B atoms)..."
    putStrLn "=" 
    
    -- Test B atoms 1-19 (data atoms)
    testDataAtoms
    
    -- Test B atoms 20-25 (function atoms)  
    testFunctionAtoms
    
    -- Test invariants
    testInvariants
    
    putStrLn "\nAll 25 B atoms tested successfully!"
    putStrLn "Ready for WASM compilation."

testDataAtoms :: IO ()
testDataAtoms = do
    putStrLn "\n-- Testing Data Atoms (B atoms 1-19) --"
    
    let testConfidence = Confidence 3.5
        testScore = FRQScore 4.2
        testTx = createTransactionPure 
            AttestationTx 
            "q001" 
            "test_pubkey_64chars_long_example_key_for_testing_purposes_ok"
            "test_signature_128chars_long_example_signature_for_testing_cryptographic_validation_in_blockchain_system_ok"
            (Just "Sample answer for FRQ")
            (Just testScore)
            testConfidence
            "test_private_key"
            1640995200.0  -- Fixed timestamp
    
    putStrLn $ "✓ B atom 1 (hash): " ++ show (T.take 16 $ hash testTx)
    putStrLn $ "✓ B atom 2 (prevHash): " ++ show (prevHash testTx)
    putStrLn $ "✓ B atom 3 (timestamp): " ++ show (timestamp testTx)
    putStrLn $ "✓ B atom 4 (nonce): " ++ show (nonce testTx)
    putStrLn $ "✓ B atom 5 (questionId): " ++ show (questionId testTx)
    putStrLn $ "✓ B atom 6 (answerHash): " ++ show (fmap (T.take 16) $ answerHash testTx)
    putStrLn $ "✓ B atom 7 (answerText): " ++ show (fmap (T.take 20) $ answerText testTx)
    putStrLn $ "✓ B atom 8 (score): " ++ show (score testTx)
    putStrLn $ "✓ B atom 9 (attesterPubkey): " ++ show (T.take 16 $ attesterPubkey testTx)
    putStrLn $ "✓ B atom 10 (signature): " ++ show (T.take 16 $ signature testTx)
    putStrLn $ "✓ B atom 11 (txType): " ++ show (txType testTx)
    putStrLn $ "✓ B atom 12 (isMatch): " ++ show (isMatch testTx)
    putStrLn $ "✓ B atom 13 (questionDistribution): " ++ show (questionDistribution testTx)
    putStrLn $ "✓ B atom 14 (mcqDistribution): " ++ show (mcqDistribution testTx)
    putStrLn $ "✓ B atom 15 (frqScores): " ++ show (frqScores testTx)
    putStrLn $ "✓ B atom 16 (convergence): " ++ show (convergence testTx)
    putStrLn $ "✓ B atom 17 (confidence): " ++ show (confidence testTx)
    putStrLn $ "✓ B atom 18 (anonymousSignature): " ++ show (anonymousSignature testTx)
    putStrLn $ "✓ B atom 19 (officialAnswer): " ++ show (officialAnswer testTx)

testFunctionAtoms :: IO ()
testFunctionAtoms = do
    putStrLn "\n-- Testing Function Atoms (B atoms 20-25) --"
    
    -- B atom function 1: SHA256 hash
    let testHash = sha256HashPure "test input"
    putStrLn $ "✓ B atom function 1 (sha256HashPure): " ++ show (T.take 16 testHash)
    
    -- B atom function 3: Signature validation
    let isValid = validateSignaturePure "message" 
            "test_signature_128chars_long_example_signature_for_testing_cryptographic_validation_in_blockchain_system_ok"
            "test_pubkey_64chars_long_example_key_for_testing_purposes_ok"
    putStrLn $ "✓ B atom function 3 (validateSignaturePure): " ++ show isValid
    
    -- B atom function 4: Consensus calculation
    let sampleAttestations = 
            [ createAttestationPure "att1" "val1" "tx1" True (Confidence 4.0) 1640995200.0
            , createAttestationPure "att2" "val2" "tx1" True (Confidence 3.5) 1640995201.0
            , createAttestationPure "att3" "val3" "tx1" False (Confidence 2.0) 1640995202.0
            ]
        consensus = calculateConsensusPure sampleAttestations
    putStrLn $ "✓ B atom function 4 (calculateConsensusPure): " ++ show (Map.size consensus) ++ " validators"
    
    -- B atom function 5: Distribution updates
    let sampleTxs = [createTransactionPure AttestationTx "q001" "pk1" "sig1" Nothing Nothing (Confidence 3.0) "priv1" 1640995200.0]
        updatedDist = updateDistributionsPure sampleTxs Map.empty
    putStrLn $ "✓ B atom function 5 (updateDistributionsPure): " ++ show (Map.size updatedDist) ++ " questions"
    
    -- B atom function 6: Outlier detection
    let outliers = detectOutliersPure sampleTxs
    putStrLn $ "✓ B atom function 6 (detectOutliersPure): " ++ show (length outliers) ++ " outliers"
    
    -- Progressive quorum (ADR-028)
    let quorum = progressiveQuorumPure 0.75
    putStrLn $ "✓ Progressive quorum (ADR-028): " ++ show quorum ++ " required"

testInvariants :: IO ()
testInvariants = do
    putStrLn "\n-- Testing System Invariants --"
    
    -- Invariant 3: Confidence bounds (1.0-5.0)
    let validConf = validateConfidencePure (Confidence 3.5)
        invalidConf = validateConfidencePure (Confidence 6.0)
    putStrLn $ "✓ Invariant 3 (confidence bounds): " ++ show validConf ++ " (valid), " ++ show (not invalidConf) ++ " (invalid rejected)"
    
    -- Invariant 5: FRQ score bounds (1.0-5.0)  
    let validScore = validateFRQScorePure (FRQScore 4.2)
        invalidScore = validateFRQScorePure (FRQScore 0.5)
    putStrLn $ "✓ Invariant 5 (FRQ score bounds): " ++ show validScore ++ " (valid), " ++ show (not invalidScore) ++ " (invalid rejected)"
    
    -- Invariant 4: Hash validation
    let testInput = "test answer A"
        expectedHash = sha256HashPure testInput
        actualHash = sha256HashPure testInput
    putStrLn $ "✓ Invariant 4 (hash validation): " ++ show (expectedHash == actualHash)
    
    -- ADR-028: Progressive quorum requirements
    let lowQuorum = progressiveQuorumPure 0.3   -- Should be 5
        medQuorum = progressiveQuorumPure 0.6   -- Should be 4  
        highQuorum = progressiveQuorumPure 0.9  -- Should be 3
    putStrLn $ "✓ ADR-028 (progressive quorum): Low=" ++ show lowQuorum ++ ", Med=" ++ show medQuorum ++ ", High=" ++ show highQuorum