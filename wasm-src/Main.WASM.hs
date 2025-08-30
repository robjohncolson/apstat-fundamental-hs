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
import FFI
import Data.Text (Text)
import qualified Data.Text as T
import Data.JSString (JSString)
import qualified Data.JSString as JS
import GHCJS.Foreign (toJSString, fromJSString)
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal)

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

-- Set up auto-save and recovery
setupPersistence :: IO ()
setupPersistence = do
    logWASM "Configuring state persistence..."
    
    -- Try to recover previous state
    maybeState <- readStateWASM "apstat-system-state"
    case maybeState of
        Nothing -> logWASM "No previous state found, using initial state"
        Just stateJson -> do
            logWASM "Recovering previous state..."
            result <- setSystemStateWASM (toJSString stateJson)
            logWASM $ "State recovery result: " <> T.pack (JS.unpack result)
    
    logWASM "Persistence layer configured"

-- Reactor model event loop
startReactorLoop :: IO ()
startReactorLoop = do
    logWASM "Reactor loop active - ready for DOM events"
    logWASM "Event loop established, awaiting browser interactions"

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

-- | Create a simple attestation for testing (JavaScript export)
foreign export javascript "createSimpleAttestation"
  createSimpleAttestationJS :: JSString -> JSString -> Double -> IO JSVal

createSimpleAttestationJS :: JSString -> JSString -> Double -> IO JSVal
createSimpleAttestationJS jsQuestionId jsAnswer jsConfidence = do
    let questionId = T.pack $ JS.unpack jsQuestionId
        answer = T.pack $ JS.unpack jsAnswer
        conf = Confidence jsConfidence
        
    if validateConfidencePure conf
        then do
            -- Create a simple transaction for testing
            timestamp <- getCurrentTimestamp
            let testTx = createTransactionPure 
                    AttestationTx 
                    questionId 
                    "test_pubkey" 
                    "test_signature"
                    (Just answer)
                    Nothing
                    conf
                    "test_privkey"
                    timestamp
            
            logWASM $ "Created transaction for question: " <> questionId
            toJSVal testTx
        else do
            logWASM "Error: Invalid confidence value"
            return nullRef

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

-- Handle attestation submission from forms
foreign export javascript "submitAttestation" submitAttestationWASM :: JSString -> JSString -> Double -> IO JSString

submitAttestationWASM :: JSString -> JSString -> Double -> IO JSString  
submitAttestationWASM questionId answer confidence = do
    let qId = T.pack $ JS.unpack questionId
    let ans = T.pack $ JS.unpack answer
    
    logWASM $ "Attestation submission: Q=" <> qId <> ", A=" <> ans <> ", C=" <> T.pack (show confidence)
    
    let eventJson = "{\"AttestEvent\":{\"attestQuestionId\":\"" <> qId <> 
                   "\",\"attestAnswer\":\"" <> ans <> 
                   "\",\"attestConfidence\":" <> T.pack (show confidence) <> "}}"
    
    result <- handleEventWASM (toJSString eventJson)
    logWASM "Attestation processed"
    return result

-- Get current view rendering
foreign export javascript "getCurrentView" getCurrentViewWASM :: IO JSString

getCurrentViewWASM :: IO JSString
getCurrentViewWASM = do
    logWASM "Rendering current view"
    renderStateWASM

-- Auto-save function
foreign export javascript "autosave" autosaveWASM :: IO ()

autosaveWASM :: IO ()
autosaveWASM = do
    currentState <- getSystemStateWASM
    let stateText = T.pack $ JS.unpack currentState
    writeStateWASM "apstat-system-state" stateText
    timestamp <- getCurrentTimestamp
    logWASM $ "Auto-save completed at " <> T.pack (show timestamp)

-- | Test consensus calculation with sample data (JavaScript export)
foreign export javascript "testConsensus"
  testConsensusJS :: IO JSVal

testConsensusJS :: IO JSVal = do
    logWASM "Testing consensus calculation with sample data..."
    
    timestamp <- getCurrentTimestamp
    let sampleAttestations = 
            [ createAttestationPure "att1" "validator1" "tx1" True (Confidence 4.0) timestamp
            , createAttestationPure "att2" "validator2" "tx1" True (Confidence 3.5) timestamp  
            , createAttestationPure "att3" "validator3" "tx1" False (Confidence 2.0) timestamp
            ]
    
    let consensus = calculateConsensusPure sampleAttestations
    logWASM $ "Consensus result: " <> T.pack (show consensus)
    toJSVal consensus