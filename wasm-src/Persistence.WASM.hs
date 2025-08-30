{-|
Module      : Persistence.WASM
Description : P2 atoms - WASM browser-based persistence with exactly 7 atoms
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module implements the P2 (Persistence) subsystem with exactly 7 atoms
adapted for WebAssembly browser storage, maintaining mathematical foundation
compatibility while replacing file IO with JS FFI localStorage/IndexedDB.
Enforces Invariant 11 (Persistence Integrity) and Invariant 12 (Atomicity).
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Persistence.WASM
    ( -- P2 Atoms (exactly 7) - WASM browser adaptations
      saveStateWASM         -- P2 atom 1: Save state to browser storage
    , loadStateWASM         -- P2 atom 2: Load state from browser storage
    , stateToJsonWASM       -- P2 atom 3: Convert state to JSON (pure)
    , jsonToStateWASM       -- P2 atom 4: Parse JSON to state (pure)
    , storageKeyWASM        -- P2 atom 5: Determine storage key
    , integrityCheckWASM    -- P2 atom 6: Verify data integrity
    , serializeAtomWASM     -- P2 atom 7: Atomic serialization
    -- Supporting types
    , PersistenceState(..)
    , IntegrityHash(..)
    , StorageStrategy(..)
    -- Helper functions
    , createPersistenceStateWASM
    , selectStorageStrategy
    ) where

import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Control.Exception (try, IOException)
import Crypto.Hash (SHA256(..), Digest)
import qualified Crypto.Hash as H

-- Import WASM FFI operations
import FFI (getStorageItemWASM, setStorageItemWASM, indexedDBGetWASM, 
           indexedDBPutWASM, getStorageQuotaWASM, getCurrentTimestampWASM,
           sha256WASM, logWASM)

-- Import original types for compatibility
import Profile (Profile)
import Blockchain (BlockchainState)

-- | Integrity hash wrapper for data verification
newtype IntegrityHash = IntegrityHash { unHash :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON IntegrityHash
instance FromJSON IntegrityHash

-- | Storage strategy selection for dual-storage approach
data StorageStrategy 
    = UseLocalStorage      -- For small states (<4MB)
    | UseIndexedDB        -- For large states (>=4MB) 
    deriving (Show, Eq, Generic)

instance ToJSON StorageStrategy
instance FromJSON StorageStrategy

-- | Complete application state for persistence (same as original)
data PersistenceState = PersistenceState
    { persistenceProfiles :: [Profile]           -- All user profiles
    , persistenceBlockchain :: BlockchainState   -- Complete blockchain state
    , persistenceVersion :: Text                 -- Version for compatibility
    , persistenceTimestamp :: Double            -- Save timestamp
    , persistenceHash :: IntegrityHash          -- Data integrity verification
    } deriving (Show, Eq, Generic)

instance ToJSON PersistenceState
instance FromJSON PersistenceState

-- ============================================================================
-- P2 ATOMS (7 functions per specification) - WASM ADAPTATIONS
-- ============================================================================

-- | P2 atom 1: Save state to browser storage with dual-strategy selection
-- Enforces Invariant 11: Persistence Integrity
saveStateWASM :: PersistenceState -> IO (Either Text ())
saveStateWASM state = do
    let jsonData = stateToJsonWASM state
        jsonSize = L.length jsonData
        jsonText = TE.decodeUtf8 $ L.toStrict jsonData
        key = storageKeyWASM "blockchain-state"
        
    strategy <- selectStorageStrategy (fromIntegral jsonSize)
    
    result <- try $ case strategy of
        UseLocalStorage -> do
            logWASM $ "Saving state to localStorage (" <> T.pack (show jsonSize) <> " bytes)"
            setStorageItemWASM key jsonText
            return ()
        UseIndexedDB -> do
            logWASM $ "Saving state to IndexedDB (" <> T.pack (show jsonSize) <> " bytes)"
            indexedDBPutWASM "apstat-blockchain" key jsonText
            return ()
            
    case result of
        Left (ex :: IOException) -> return $ Left $ T.pack $ "Failed to save state: " ++ show ex
        Right _ -> return $ Right ()

-- | P2 atom 2: Load state from browser storage with dual-strategy attempt
-- Enforces Invariant 11: loadState(saveState(s)) must equal s
loadStateWASM :: Text -> IO (Either Text PersistenceState)
loadStateWASM fileName = do
    let key = storageKeyWASM fileName
    
    -- Try localStorage first, then IndexedDB
    result <- try $ do
        maybeData <- getStorageItemWASM key
        case maybeData of
            Just jsonText -> return $ Just jsonText
            Nothing -> do
                -- Try IndexedDB as fallback
                logWASM "State not found in localStorage, trying IndexedDB"
                indexedDBGetWASM "apstat-blockchain" key
                
    case result of
        Left (ex :: IOException) -> return $ Left $ T.pack $ "Failed to read state: " ++ show ex
        Right Nothing -> return $ Left "State not found in browser storage"
        Right (Just jsonText) -> do
            let jsonData = L.fromStrict $ TE.encodeUtf8 jsonText
            case jsonToStateWASM jsonData of
                Left err -> return $ Left $ T.pack $ "Failed to parse state: " ++ err
                Right state -> do
                    let verification = integrityCheckWASM state
                    if verification
                        then return $ Right state
                        else return $ Left "Integrity check failed: data may be corrupted"

-- | P2 atom 3: Convert state to JSON ByteString (pure function, unchanged)
-- Pure function for serialization
stateToJsonWASM :: PersistenceState -> ByteString
stateToJsonWASM state = 
    let stateWithHash = state { persistenceHash = calculateHashWASM state }
    in encode stateWithHash

-- | P2 atom 4: Parse JSON ByteString to state (pure function, unchanged)
-- Pure function for deserialization
jsonToStateWASM :: ByteString -> Either String PersistenceState
jsonToStateWASM = eitherDecode

-- | P2 atom 5: Determine storage key for browser storage
-- Pure function returning storage key for persistence
storageKeyWASM :: Text -> Text
storageKeyWASM fileName = "apstat-" <> fileName

-- | P2 atom 6: Verify data integrity using SHA256 hash (enhanced)
-- Enforces data integrity per mathematical specification
integrityCheckWASM :: PersistenceState -> Bool
integrityCheckWASM state = 
    let storedHash = persistenceHash state
        calculatedHash = calculateHashWASM state
    in storedHash == calculatedHash

-- | P2 atom 7: Atomic serialization of individual components (unchanged)
-- Enables independent serialization of subsystem atoms
serializeAtomWASM :: (ToJSON a) => a -> ByteString
serializeAtomWASM = encode

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Calculate SHA256 hash of state for integrity verification
calculateHashWASM :: PersistenceState -> IntegrityHash
calculateHashWASM state = 
    let stateWithoutHash = state { persistenceHash = IntegrityHash "" }
        jsonData = encode stateWithoutHash
        bytes = L.toStrict jsonData
        digest = H.hash bytes :: Digest SHA256
    in IntegrityHash $ T.pack $ show digest

-- | Create a new persistence state with current timestamp and hash
createPersistenceStateWASM :: [Profile] -> BlockchainState -> IO PersistenceState
createPersistenceStateWASM profiles blockchain = do
    currentTime <- getCurrentTimestampWASM
    let state = PersistenceState
            { persistenceProfiles = profiles
            , persistenceBlockchain = blockchain
            , persistenceVersion = "1.0.0-wasm"
            , persistenceTimestamp = currentTime
            , persistenceHash = IntegrityHash ""  -- Will be calculated in stateToJsonWASM
            }
    return state

-- | Select storage strategy based on data size (dual-storage logic)
-- localStorage: <4MB, IndexedDB: >=4MB
selectStorageStrategy :: Int -> IO StorageStrategy
selectStorageStrategy dataSize = do
    (quota, usage) <- getStorageQuotaWASM
    let localStorageLimit = 4 * 1024 * 1024  -- 4MB threshold
        availableQuota = quota - usage
    
    if dataSize < localStorageLimit && availableQuota > dataSize
        then do
            logWASM $ "Selected localStorage strategy (size: " <> T.pack (show dataSize) <> ")"
            return UseLocalStorage
        else do
            logWASM $ "Selected IndexedDB strategy (size: " <> T.pack (show dataSize) <> ")"
            return UseIndexedDB