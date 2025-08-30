{-|
Module      : Blockchain.WASM
Description : WASM-compatible Blockchain module with JS FFI integration
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module provides WASM-compatible versions of Blockchain operations,
using JS FFI for IO while maintaining all 25 B atoms and invariants.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Blockchain.WASM
    ( -- Re-export pure types and functions
      module Blockchain.Pure
    -- WASM-compatible IO functions  
    , getCurrentTimestamp
    , sha256Hash
    , createTransaction
    , createAttestation
    -- Export functions for JavaScript
    , createAttestationJS
    , calculateConsensusJS
    , validateTransactionJS
    ) where

import Blockchain.Pure
import FFI
import Data.Text (Text)
import qualified Data.Text as T
import Data.JSString (JSString)
import qualified Data.JSString as JS
import GHCJS.Foreign (toJSString, fromJSString)
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal)

-- WASM-compatible IO functions using FFI

-- | WASM-compatible timestamp function
getCurrentTimestamp :: IO Double
getCurrentTimestamp = getCurrentTimestampWASM

-- | WASM-compatible SHA256 hash function
sha256Hash :: Text -> IO Text
sha256Hash input = do
    -- Use JS Web Crypto API for better performance
    result <- sha256WASM input
    return result

-- | WASM-compatible transaction creation
createTransaction :: TxType -> Text -> Text -> Text -> Maybe Text -> Maybe FRQScore -> 
                    Confidence -> Text -> IO Transaction
createTransaction txType' qid attesterKey sig answerTxt frqScore conf privKey = do
    currentTime <- getCurrentTimestamp
    return $ createTransactionPure txType' qid attesterKey sig answerTxt frqScore conf privKey currentTime

-- | WASM-compatible attestation creation
createAttestation :: Text -> Text -> Text -> Bool -> Confidence -> IO Attestation
createAttestation attId validator tx isValid conf = do
    currentTime <- getCurrentTimestamp
    return $ createAttestationPure attId validator tx isValid conf currentTime

-- JavaScript Export Functions for Browser Integration

-- | Export attestation creation to JavaScript
foreign export javascript "createAttestation" 
  createAttestationJS :: JSString -> JSString -> JSString -> Bool -> Double -> IO JSVal

createAttestationJS :: JSString -> JSString -> JSString -> Bool -> Double -> IO JSVal
createAttestationJS jsAttId jsValidator jsTx isValid confValue = do
    let attId = T.pack $ JS.unpack jsAttId
        validator = T.pack $ JS.unpack jsValidator
        tx = T.pack $ JS.unpack jsTx
        conf = Confidence confValue
    
    if validateConfidencePure conf
        then do
            attestation <- createAttestation attId validator tx isValid conf
            toJSVal attestation
        else do
            logWASM "Error: Invalid confidence value in createAttestationJS"
            return nullRef

-- | Export consensus calculation to JavaScript  
foreign export javascript "calculateConsensus"
  calculateConsensusJS :: JSVal -> IO JSVal

calculateConsensusJS :: JSVal -> IO JSVal
calculateConsensusJS jsAttestations = do
    maybeAttestations <- fromJSVal jsAttestations
    case maybeAttestations of
        Nothing -> do
            logWASM "Error: Invalid attestations array in calculateConsensusJS"
            return nullRef
        Just attestations -> do
            let consensus = calculateConsensusPure attestations
            toJSVal consensus

-- | Export transaction validation to JavaScript
foreign export javascript "validateTransaction"
  validateTransactionJS :: JSVal -> IO Bool

validateTransactionJS :: JSVal -> IO Bool
validateTransactionJS jsTx = do
    maybeTx <- fromJSVal jsTx
    case maybeTx of
        Nothing -> do
            logWASM "Error: Invalid transaction in validateTransactionJS"
            return False
        Just tx -> return $ validateTransactionPure tx

-- | Export hash function to JavaScript
foreign export javascript "sha256Hash"
  sha256HashJS :: JSString -> IO JSString

sha256HashJS :: JSString -> IO JSString
sha256HashJS jsInput = do
    let input = T.pack $ JS.unpack jsInput
    result <- sha256Hash input
    return $ toJSString result

-- | Export progressive quorum calculation to JavaScript
foreign export javascript "progressiveQuorum"
  progressiveQuorumJS :: Double -> IO Int

progressiveQuorumJS :: Double -> IO Int
progressiveQuorumJS convergence = return $ progressiveQuorumPure convergence