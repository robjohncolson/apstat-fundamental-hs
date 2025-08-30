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
    
    -- Run basic initialization tests
    testBasicFunctions
    
    logWASM "WASM module initialization complete"

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