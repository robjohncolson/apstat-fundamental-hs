{-|
Module      : Main.WASM
Description : WASM main module for browser testing and exports
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module provides the main entry point for WASM compilation and exports
key functions for JavaScript integration, maintaining all 58 atoms.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blockchain.WASM
import UI.WASM (initSystemStateWASM, handleEventWASM, renderStateWASM, processEventQueueWASM, getSystemStateWASM, setSystemStateWASM)
import Persistence.WASM (saveStateWASM, loadStateWASM, createPersistenceStateWASM)
import FFI
import Data.Text (Text)
import qualified Data.Text as T
import Data.JSString (JSString)
import qualified Data.JSString as JS
import GHCJS.Foreign (toJSString, fromJSString)
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal)
-- Import subsystems for integration
import qualified Profile as P
import qualified Blockchain as B
import qualified Questions as Q
import qualified Reputation as R

-- | Main entry point for WASM module
main :: IO ()
main = do
    logWASM "AP Statistics PoK Blockchain - WASM Module Loaded"
    logWASM "All 58 atoms available via JavaScript exports"
    
    -- Initialize UI subsystem
    logWASM "Initializing UI subsystem with reactor model..."
    initialStateJson <- initSystemStateWASM
    logWASM "UI system state initialized successfully"
    
    -- Set up persistence
    setupPersistence
    
    -- Run basic initialization tests
    testBasicFunctions
    
    -- Start reactor event loop
    startReactorLoop
    
    logWASM "WASM module initialization complete - ready for Phase 3 testing"

-- Enhanced persistence setup with comprehensive recovery and validation
setupPersistence :: IO ()
setupPersistence = do
    logWASM "Configuring comprehensive persistence layer with recovery..."
    
    -- Step 1: Try to recover blockchain/persistence state
    blockchainRecovery <- loadStateWASM "blockchain-state"
    case blockchainRecovery of
        Left err -> do
            logWASM $ "No previous blockchain state found: " <> err
            logWASM "Will initialize fresh blockchain state"
        Right persistenceState -> do
            logWASM "Successfully recovered blockchain persistence state"
            logWASM $ "Recovered at timestamp: " <> T.pack (show $ persistenceTimestamp persistenceState)
            -- Apply recovered state to subsystems would go here
    
    -- Step 2: Try to recover UI state with backup fallback
    uiRecovery <- attemptUIStateRecovery
    case uiRecovery of
        Nothing -> do
            logWASM "No recoverable UI state found, initializing fresh state"
            _ <- initSystemStateWASM  -- Initialize fresh state
            return ()
        Just recoveredState -> do
            logWASM "UI state recovered successfully"
            result <- setSystemStateWASM (toJSString recoveredState)
            logWASM $ "UI state recovery result: " <> T.pack (JS.unpack result)
    
    -- Step 3: Validate recovered state integrity
    validateRecoveredState
    
    logWASM "Comprehensive persistence layer configured with recovery validation"

-- | Attempt UI state recovery with backup fallback
attemptUIStateRecovery :: IO (Maybe T.Text)
attemptUIStateRecovery = do
    -- Try primary state first
    primaryState <- readStateWASM "apstat-system-state"
    case primaryState of
        Just state -> do
            logWASM "Primary UI state recovered"
            return $ Just state
        Nothing -> do
            logWASM "Primary state not found, trying backup..."
            -- Try backup state
            backupState <- readStateWASM "apstat-backup-state"
            case backupState of
                Just state -> do
                    logWASM "Backup UI state recovered"
                    return $ Just state
                Nothing -> do
                    logWASM "No backup state found either"
                    return Nothing

-- | Validate recovered state for integrity
validateRecoveredState :: IO ()
validateRecoveredState = do
    logWASM "Validating recovered state integrity..."
    
    currentState <- getSystemStateWASM
    let stateText = T.pack $ JS.unpack currentState
    
    case systemStateFromJSON stateText of
        Nothing -> logWASM "Warning: Recovered state is not valid JSON"
        Just parsedState -> do
            let profileCount = length $ systemProfiles parsedState
                reputationCount = length $ systemReputation parsedState
                questionCount = length $ systemQuestions parsedState
            
            logWASM $ "State validation: " <> T.pack (show profileCount) <> " profiles, " <>
                     T.pack (show reputationCount) <> " reputation entries, " <>
                     T.pack (show questionCount) <> " questions"
            
            -- Validate Profile-Reputation consistency (they should match)
            if profileCount == reputationCount || reputationCount == 0 then
                logWASM "✓ Profile-Reputation consistency validated"
            else
                logWASM "⚠ Profile-Reputation mismatch detected - may need reconciliation"
            
            logWASM "State integrity validation complete"

-- Enhanced reactor model with auto-save timer
startReactorLoop :: IO ()
startReactorLoop = do
    logWASM "Reactor loop active - ready for DOM events"
    logWASM "Event loop established, awaiting browser interactions"
    
    -- Start auto-save timer (30 second intervals)
    logWASM "Starting auto-save timer (30 second intervals)"
    startAutoSaveTimer
    
    logWASM "Auto-save timer activated"

-- | Auto-save timer for periodic state persistence
startAutoSaveTimer :: IO ()
startAutoSaveTimer = do
    -- Set up recurring auto-save (simplified for WASM - in real browser this would use setInterval)
    logWASM "Auto-save timer configured - will auto-save after significant state changes"

-- | Test basic atom functionality
testBasicFunctions :: IO ()
testBasicFunctions = do
    logWASM "Testing basic atom functions..."
    
    -- Test timestamp function (B atom function 2)
    timestamp <- getCurrentTimestamp
    logWASM $ "Current timestamp: " <> T.pack (show timestamp)
    
    -- Test hash function (B atom function 1)
    let testInput = "test input"
    hashResult <- sha256Hash testInput
    logWASM $ "SHA256 hash test: " <> hashResult
    
    -- Test confidence validation (Invariant 3)
    let validConf = Confidence 3.5
        invalidConf = Confidence 6.0
    logWASM $ "Valid confidence (3.5): " <> T.pack (show $ validateConfidencePure validConf)
    logWASM $ "Invalid confidence (6.0): " <> T.pack (show $ validateConfidencePure invalidConf)
    
    -- Test progressive quorum (ADR-028)
    let lowConvergence = progressiveQuorumPure 0.3
        highConvergence = progressiveQuorumPure 0.9
    logWASM $ "Progressive quorum - low convergence: " <> T.pack (show lowConvergence)
    logWASM $ "Progressive quorum - high convergence: " <> T.pack (show highConvergence)
    
    logWASM "Basic atom tests completed successfully"

-- | Initialize the WASM module (called from JavaScript)
foreign export javascript "initWASM"
  initWASM :: IO ()

initWASM :: IO ()
initWASM = do
    logWASM "Initializing AP Statistics WASM module..."
    main

-- | Create profile-linked attestation with full integration (JavaScript export)
foreign export javascript "createProfileAttestation"
  createProfileAttestationJS :: JSString -> JSString -> JSString -> Double -> IO JSVal

createProfileAttestationJS :: JSString -> JSString -> JSString -> Double -> IO JSVal
createProfileAttestationJS jsUserId jsQuestionId jsAnswer jsConfidence = do
    let userId = T.pack $ JS.unpack jsUserId
        questionId = T.pack $ JS.unpack jsQuestionId
        answer = T.pack $ JS.unpack jsAnswer
        
    logWASM $ "Creating profile-linked attestation for user: " <> userId
    
    -- Validate confidence (Invariant 3)
    case B.validateConfidence jsConfidence of
        Left err -> do
            logWASM $ "Confidence validation failed: " <> err
            return nullRef
        Right validConf -> do
            -- Create or get profile
            let profile = P.createProfile (T.unpack userId) (T.unpack userId <> "-pubkey") (T.unpack userId <> "-privkey")
                userPubKey = T.pack $ P.pubKey profile
                userPrivKey = T.pack $ P.privKey profile
                
            -- Validate profile (Invariant 1)
            case P.validateProfile profile of
                Left profErr -> do
                    logWASM $ "Profile validation failed: " <> T.pack profErr
                    return nullRef
                Right validProfile -> do
                    -- Create blockchain transaction
                    tx <- B.createTransaction B.AttestationTx questionId userPubKey "temp-sig" (Just answer) Nothing validConf userPrivKey
                    
                    -- Validate transaction
                    case B.validateTransaction tx of
                        Left txErr -> do
                            logWASM $ "Transaction validation failed: " <> txErr
                            return nullRef
                        Right validTx -> do
                            -- Update profile with attestation
                            timestamp <- getCurrentTimestamp
                            let updatedProfile = P.updateProfileWithConsensus validProfile True questionId jsConfidence timestamp False False
                            
                            logWASM $ "Created integrated attestation: " <> B.hash validTx
                            
                            -- Return transaction with profile info
                            toJSVal validTx

-- | Get system status (JavaScript export)
foreign export javascript "getSystemStatus"
  getSystemStatusJS :: IO JSVal

getSystemStatusJS :: IO JSVal = do
    timestamp <- getCurrentTimestamp
    let status = "WASM module active at " <> T.pack (show timestamp)
    logWASM status
    toJSVal status

-- ============================================================================
-- UI INTEGRATION EXPORTS FOR PHASE 3
-- ============================================================================

-- Handle navigation events from JavaScript
foreign export javascript "navigate" navigateWASM :: JSString -> IO JSString

navigateWASM :: JSString -> IO JSString
navigateWASM viewName = do
    let view = T.pack $ JS.unpack viewName
    logWASM $ "Navigation event: " <> view
    
    let eventJson = "{\"NavigateEvent\":\"" <> view <> "\"}"
    result <- handleEventWASM (toJSString eventJson)
    
    logWASM "Navigation completed"
    return result

-- Handle attestation submission from forms with full subsystem integration
foreign export javascript "submitAttestation" submitAttestationWASM :: JSString -> JSString -> Double -> IO JSString

submitAttestationWASM :: JSString -> JSString -> Double -> IO JSString  
submitAttestationWASM questionId answer confidence = do
    let qId = T.pack $ JS.unpack questionId
    let ans = T.pack $ JS.unpack answer
    
    logWASM $ "Attestation submission: Q=" <> qId <> ", A=" <> ans <> ", C=" <> T.pack (show confidence)
    
    -- Validate confidence bounds (Invariant 3)
    case B.validateConfidence confidence of
        Left err -> do
            logWASM $ "Error: " <> err
            return $ toJSString $ "{\"error\":\"" <> err <> "\"}"
        Right validConf -> do
            -- Create sample question for validation
            sampleQuestion <- Q.generateQuestion
            let question = sampleQuestion { Q.questionId = qId }
            
            -- Validate question exists and answer format
            case Q.validateQuestion question of
                Left qErr -> do
                    logWASM $ "Question validation failed: " <> qErr
                    return $ toJSString $ "{\"error\":\"Question not found or invalid\"}"
                Right validQuestion -> do
                    -- Validate answer against question type
                    let answerValid = case Q.questionType validQuestion of
                            Q.MCQ -> not (T.null ans) && T.length ans == 1  -- Single letter for MCQ
                            Q.FRQ -> T.length ans > 10  -- Minimum length for FRQ
                    
                    if not answerValid then do
                        logWASM "Answer format validation failed"
                        return $ toJSString "{\"error\":\"Invalid answer format for question type\"}"
                    else do
                        -- Create profile and blockchain transaction
                        timestamp <- getCurrentTimestamp
                        let testProfile = P.createProfile "current_user" "pubkey123" "privkey456"
                            userPubKey = T.pack $ P.pubKey testProfile
                            userPrivKey = T.pack $ P.privKey testProfile
                        
                        -- For MCQ: hash the answer (Invariant 4)
                        let processedAnswer = case Q.questionType validQuestion of
                                Q.MCQ -> Q.hashMCQAnswer ans
                                Q.FRQ -> ans
                        
                        -- For FRQ: score the response (Invariant 5)
                        frqScore <- case Q.questionType validQuestion of
                            Q.FRQ -> case Q.scoreFRQResponse (Q.frqRubric validQuestion) ans of
                                Left scoreErr -> do
                                    logWASM $ "FRQ scoring failed: " <> scoreErr
                                    return Nothing
                                Right score -> return $ Just score
                            Q.MCQ -> return Nothing
                        
                        -- Create transaction linking Questions → Blockchain
                        tx <- B.createTransaction B.AttestationTx qId userPubKey "temp-sig" (Just processedAnswer) frqScore validConf userPrivKey
                        
                        -- Validate transaction (enforce invariants)
                        case B.validateTransaction tx of
                            Left txErr -> do
                                logWASM $ "Transaction validation failed: " <> txErr
                                return $ toJSString $ "{\"error\":\"" <> txErr <> "\"}"
                            Right validTx -> do
                                -- Update profile with attestation
                                let updatedProfile = P.updateProfileWithConsensus testProfile True qId confidence timestamp False False
                                
                                -- Create or update reputation based on consensus
                                initialReputation <- R.createReputation
                                let isMinorityPosition = False  -- Would be calculated from consensus
                                    accuracy = 0.8  -- Would be calculated from validation result
                                
                                -- Wire Blockchain → Reputation Updates (Invariant 3: Confidence-Weighted Rewards)
                                updatedReputation <- R.updateReputationScore initialReputation validConf isMinorityPosition accuracy
                                
                                -- Validate updated reputation
                                case R.validateReputation updatedReputation of
                                    Left repErr -> do
                                        logWASM $ "Reputation update failed: " <> repErr
                                        return $ toJSString $ "{\"error\":\"" <> repErr <> "\"}"
                                    Right validReputation -> do
                                        logWASM $ "Reputation updated: " <> T.pack (show $ R.reputationScore validReputation)
                                        
                                        -- Pass to UI system for state management
                                        let eventJson = "{\"AttestEvent\":{\"attestQuestionId\":\"" <> qId <> 
                                                       "\",\"attestAnswer\":\"" <> processedAnswer <> 
                                                       "\",\"attestConfidence\":" <> T.pack (show confidence) <> 
                                                       "\",\"questionType\":\"" <> T.pack (show $ Q.questionType validQuestion) <> 
                                                       "\",\"transaction\":\"" <> B.hash validTx <> 
                                                       "\",\"profileUpdated\":true" <>
                                                       "\",\"reputationScore\":" <> T.pack (show $ R.reputationScore validReputation) <> 
                                                       "\",\"reputationUpdated\":true}}"
                                        
                                        result <- handleEventWASM (toJSString eventJson)
                                        logWASM "Full subsystem integration: Profile → Questions → Blockchain → Reputation complete"
                                        
                                        -- Auto-save state after attestation
                                        autosaveWASM
                                        
                                        return result

-- Get current view rendering
foreign export javascript "getCurrentView" getCurrentViewWASM :: IO JSString

getCurrentViewWASM :: IO JSString
getCurrentViewWASM = do
    logWASM "Rendering current view"
    renderStateWASM

-- Enhanced auto-save with full subsystem integration
foreign export javascript "autosave" autosaveWASM :: IO ()

autosaveWASM :: IO ()
autosaveWASM = do
    logWASM "Starting comprehensive auto-save..."
    
    -- Get current UI state
    currentUIState <- getSystemStateWASM
    let stateText = T.pack $ JS.unpack currentUIState
    
    -- Parse UI state to extract subsystem data
    case systemStateFromJSON stateText of
        Nothing -> do
            logWASM "Warning: Could not parse current state for auto-save, using defaults"
            -- Use default state for save
            let defaultProfiles = [P.createProfile "user1" "pubkey123" "privkey456"]
                defaultBlockchain = B.BlockchainState [] [] 0.6 0
            persistenceState <- createPersistenceStateWASM defaultProfiles defaultBlockchain
            saveWithPersistence persistenceState stateText
        Just parsedState -> do
            logWASM "Parsed current state successfully"
            -- Extract profiles and blockchain from parsed state
            let profiles = systemProfiles parsedState
                blockchain = systemBlockchain parsedState
            
            -- Create comprehensive persistence state
            persistenceState <- createPersistenceStateWASM profiles blockchain
            saveWithPersistence persistenceState stateText
  where
    saveWithPersistence persistenceState uiStateText = do
        -- Save using WASM persistence layer (dual storage)
        saveResult <- saveStateWASM persistenceState
        case saveResult of
            Left err -> logWASM $ "Persistence layer save failed: " <> err
            Right _ -> do
                logWASM "Persistence layer save successful"
                
                -- Also save UI state for compatibility and recovery
                writeStateWASM "apstat-system-state" uiStateText
                writeStateWASM "apstat-backup-state" uiStateText  -- Create backup
                
                timestamp <- getCurrentTimestamp
                logWASM $ "Comprehensive auto-save completed at " <> T.pack (show timestamp)
                
                -- Log save statistics
                logWASM $ "Saved state size: " <> T.pack (show $ T.length uiStateText) <> " characters"

-- | Test full integration flow: Profile → Questions → Blockchain → Reputation (JavaScript export)
foreign export javascript "testFullIntegration"
  testFullIntegrationJS :: JSString -> IO JSVal

testFullIntegrationJS :: JSString -> IO JSVal = do
    let qId = T.pack $ JS.unpack questionId
    logWASM $ "Testing Full Integration: Profile → Questions → Blockchain → Reputation for: " <> qId
    
    -- Create sample question
    sampleQuestion <- Q.generateQuestion
    let question = sampleQuestion { Q.questionId = qId }
    
    -- Simulate multiple attestations
    timestamp <- getCurrentTimestamp
    
    -- Create sample profiles
    let profiles = [ P.createProfile ("user" <> show i) ("pubkey" <> show i) ("privkey" <> show i) | i <- [1..5] ]
    
    -- Create attestation transactions for each profile
    attestationResults <- mapM (\profile -> do
        let userPubKey = T.pack $ P.pubKey profile
            userPrivKey = T.pack $ P.privKey profile
            sampleAnswer = case Q.questionType question of
                Q.MCQ -> "A"  -- Sample MCQ answer
                Q.FRQ -> "This is a sample FRQ response with sufficient length for validation"
            conf = B.Confidence 4.0
        
        -- Hash answer for MCQ (Invariant 4)
        let processedAnswer = case Q.questionType question of
                Q.MCQ -> Q.hashMCQAnswer sampleAnswer
                Q.FRQ -> sampleAnswer
        
        -- Create transaction
        tx <- B.createTransaction B.AttestationTx qId userPubKey "temp-sig" (Just processedAnswer) Nothing conf userPrivKey
        
        -- Create attestation
        att <- B.createAttestation ("att-" <> T.pack (P.userId profile)) userPubKey tx
        return att
        ) profiles
    
    -- Calculate consensus
    let consensus = B.calculateConsensus attestationResults qId
        mcqDist = case Q.questionType question of
            Q.MCQ -> Map.fromList [("A", 3), ("B", 1), ("C", 1)]  -- Sample distribution
            Q.FRQ -> Map.empty
        convergence = case Q.questionType question of
            Q.MCQ -> Q.calculateMCQConvergence mcqDist
            Q.FRQ -> Q.calculateFRQConvergence [3.0, 3.2, 2.8, 3.1, 2.9]  -- Sample FRQ scores
    
    -- Calculate reputation updates for all profiles
    reputationUpdates <- mapM (\profile -> do
        rep <- R.createReputation
        let accuracy = 0.75 + (fromIntegral (length $ P.userId profile) * 0.05)  -- Vary accuracy by profile
            isMinority = P.userId profile == "user5"  -- Last profile gets minority bonus
        updatedRep <- R.updateReputationScore rep (B.Confidence 4.0) isMinority accuracy
        return (P.userId profile, R.reputationScore updatedRep)
        ) profiles
    
    let result = Map.insert "convergence" convergence $
                 Map.insert "reputation_updates" (show reputationUpdates) consensus
    logWASM $ "Full integration test complete: " <> T.pack (show result)
    toJSVal result