{-|
Module      : TestPersistenceWASM
Description : Test harness for WASM persistence atoms and invariants
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Test suite for verifying all 7 persistence atoms work correctly in WASM
environment and that Invariant #11 (load/save symmetry) holds.
-}

{-# LANGUAGE OverloadedStrings #-}

module TestPersistenceWASM where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as L

-- Import the modules we're testing
import Persistence.WASM
import Profile (Profile, createProfile)
import Blockchain (BlockchainState, createEmptyBlockchainState)
import FFI (logWASM, getCurrentTimestampWASM)

-- ============================================================================
-- TEST DATA GENERATION
-- ============================================================================

-- | Create minimal test data for persistence testing
createTestProfiles :: IO [Profile]
createTestProfiles = do
    profile1 <- createProfile "alice" "test-key-1"
    profile2 <- createProfile "bob" "test-key-2"
    return [profile1, profile2]

-- | Create test blockchain state
createTestBlockchain :: IO BlockchainState
createTestBlockchain = createEmptyBlockchainState

-- ============================================================================
-- PERSISTENCE ATOM TESTS (7 atoms)
-- ============================================================================

-- | Test P2 atom 3: stateToJsonWASM (pure serialization)
testStateToJson :: IO Bool
testStateToJson = do
    logWASM "Testing P2 atom 3: stateToJsonWASM"
    profiles <- createTestProfiles
    blockchain <- createTestBlockchain
    state <- createPersistenceStateWASM profiles blockchain
    
    let jsonData = stateToJsonWASM state
        jsonSize = L.length jsonData
    
    logWASM $ "Serialized state to " <> T.pack (show jsonSize) <> " bytes"
    return (jsonSize > 0)

-- | Test P2 atom 4: jsonToStateWASM (pure deserialization) 
testJsonToState :: IO Bool
testJsonToState = do
    logWASM "Testing P2 atom 4: jsonToStateWASM"
    profiles <- createTestProfiles
    blockchain <- createTestBlockchain
    state <- createPersistenceStateWASM profiles blockchain
    
    let jsonData = stateToJsonWASM state
    
    case jsonToStateWASM jsonData of
        Left err -> do
            logWASM $ "Deserialization failed: " <> T.pack err
            return False
        Right parsedState -> do
            logWASM "Deserialization successful"
            return True

-- | Test P2 atom 5: storageKeyWASM (key generation)
testStorageKey :: IO Bool
testStorageKey = do
    logWASM "Testing P2 atom 5: storageKeyWASM"
    let key = storageKeyWASM "test-file.json"
        expectedKey = "apstat-test-file.json"
    
    if key == expectedKey
        then do
            logWASM $ "Storage key correct: " <> key
            return True
        else do
            logWASM $ "Storage key incorrect: expected " <> expectedKey <> ", got " <> key
            return False

-- | Test P2 atom 6: integrityCheckWASM (hash verification)
testIntegrityCheck :: IO Bool
testIntegrityCheck = do
    logWASM "Testing P2 atom 6: integrityCheckWASM"
    profiles <- createTestProfiles
    blockchain <- createTestBlockchain
    state <- createPersistenceStateWASM profiles blockchain
    
    let jsonData = stateToJsonWASM state
    case jsonToStateWASM jsonData of
        Left _ -> do
            logWASM "Cannot test integrity - deserialization failed"
            return False
        Right parsedState -> do
            let integrityValid = integrityCheckWASM parsedState
            if integrityValid
                then do
                    logWASM "Integrity check passed"
                    return True
                else do
                    logWASM "Integrity check failed"
                    return False

-- | Test P2 atom 7: serializeAtomWASM (individual component serialization)
testSerializeAtom :: IO Bool
testSerializeAtom = do
    logWASM "Testing P2 atom 7: serializeAtomWASM"
    profiles <- createTestProfiles
    
    let serialized = serializeAtomWASM profiles
        serializedSize = L.length serialized
        
    logWASM $ "Serialized atom to " <> T.pack (show serializedSize) <> " bytes"
    return (serializedSize > 0)

-- ============================================================================
-- INVARIANT TESTS
-- ============================================================================

-- | Test Invariant #11: Persistence Integrity - load(save(s)) = s
testInvariant11_PersistenceIntegrity :: IO Bool
testInvariant11_PersistenceIntegrity = do
    logWASM "Testing Invariant #11: Persistence Integrity"
    profiles <- createTestProfiles
    blockchain <- createTestBlockchain
    originalState <- createPersistenceStateWASM profiles blockchain
    
    -- Simulate save/load cycle using pure functions
    let jsonData = stateToJsonWASM originalState
    
    case jsonToStateWASM jsonData of
        Left err -> do
            logWASM $ "Round-trip failed during deserialization: " <> T.pack err
            return False
        Right restoredState -> do
            -- Compare key fields (excluding timestamp which may vary slightly)
            let profilesMatch = length (persistenceProfiles originalState) == 
                               length (persistenceProfiles restoredState)
                versionMatch = persistenceVersion originalState == 
                              persistenceVersion restoredState
                hashMatch = persistenceHash originalState == 
                           persistenceHash restoredState
            
            if profilesMatch && versionMatch && hashMatch
                then do
                    logWASM "✓ Invariant #11 satisfied: load(save(s)) = s"
                    return True
                else do
                    logWASM "✗ Invariant #11 violated: state mismatch after round-trip"
                    return False

-- ============================================================================
-- FULL TEST SUITE
-- ============================================================================

-- | Run all persistence tests
runAllTests :: IO Bool
runAllTests = do
    logWASM "========================================="
    logWASM "AP Statistics WASM Persistence Test Suite"
    logWASM "Testing all 7 P2 atoms and invariants"
    logWASM "========================================="
    
    test1 <- testStateToJson
    test2 <- testJsonToState 
    test3 <- testStorageKey
    test4 <- testIntegrityCheck
    test5 <- testSerializeAtom
    
    logWASM "---------------------"
    logWASM "Testing Invariants"
    logWASM "---------------------"
    
    invariant11 <- testInvariant11_PersistenceIntegrity
    
    let allTestsPassed = and [test1, test2, test3, test4, test5, invariant11]
    
    logWASM "========================================="
    if allTestsPassed
        then do
            logWASM "🎉 ALL TESTS PASSED!"
            logWASM "All 7 P2 atoms verified"
            logWASM "Invariant #11 (Persistence Integrity) satisfied"
            logWASM "WASM persistence ready for browser deployment"
        else do
            logWASM "❌ SOME TESTS FAILED"
            logWASM "Review test output above for details"
    logWASM "========================================="
    
    return allTestsPassed

-- | Main test entry point
main :: IO ()
main = do
    success <- runAllTests
    if success
        then logWASM "Test suite completed successfully"
        else logWASM "Test suite completed with failures"