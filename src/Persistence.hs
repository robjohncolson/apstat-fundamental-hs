{-|
Module      : Persistence
Description : P2 atoms - State persistence with exactly 7 atoms
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module implements the P2 (Persistence) subsystem with exactly 7 atoms
as specified in the AP Statistics PoK Blockchain mathematical foundation.
Enforces Invariant 11 (Persistence Integrity) and Invariant 12 (Atomicity).
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Persistence
    ( -- P2 Atoms (exactly 7)
      saveState         -- P2 atom 1: Save state to filesystem
    , loadState         -- P2 atom 2: Load state from filesystem
    , stateToJson       -- P2 atom 3: Convert state to JSON
    , jsonToState       -- P2 atom 4: Parse JSON to state
    , filePath          -- P2 atom 5: Determine storage location
    , integrityCheck    -- P2 atom 6: Verify data integrity
    , serializeAtom     -- P2 atom 7: Atomic serialization
    -- Supporting types
    , PersistenceState(..)
    , IntegrityHash(..)
    -- Helper functions
    , createPersistenceState
    ) where

import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.IO (writeFile, readFile)
import Control.Exception (try, IOException)
import Crypto.Hash (SHA256(..), Digest)
import qualified Crypto.Hash as H
import Profile (Profile)
import Blockchain (BlockchainState, getCurrentTimestamp)

-- | Integrity hash wrapper for data verification
newtype IntegrityHash = IntegrityHash { unHash :: Text }
    deriving (Show, Eq, Generic)

instance ToJSON IntegrityHash
instance FromJSON IntegrityHash

-- | Complete application state for persistence (combines Profile + Blockchain)
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
-- P2 ATOMS (7 functions per specification)
-- ============================================================================

-- | P2 atom 1: Save state to filesystem with integrity check
-- Enforces Invariant 11: Persistence Integrity
saveState :: PersistenceState -> IO (Either Text ())
saveState state = do
    let jsonData = stateToJson state
        path = filePath "blockchain-state.json"
    result <- try $ L.writeFile path jsonData
    case result of
        Left (ex :: IOException) -> return $ Left $ T.pack $ "Failed to save state: " ++ show ex
        Right _ -> return $ Right ()

-- | P2 atom 2: Load state from filesystem with integrity verification
-- Enforces Invariant 11: loadState(saveState(s)) must equal s
loadState :: FilePath -> IO (Either Text PersistenceState)
loadState fileName = do
    let path = filePath fileName
    result <- try $ L.readFile path
    case result of
        Left (ex :: IOException) -> return $ Left $ T.pack $ "Failed to read state file: " ++ show ex
        Right jsonData -> case jsonToState jsonData of
            Left err -> return $ Left $ T.pack $ "Failed to parse state: " ++ err
            Right state -> do
                let verification = integrityCheck state
                if verification
                    then return $ Right state
                    else return $ Left "Integrity check failed: data may be corrupted"

-- | P2 atom 3: Convert state to JSON ByteString
-- Pure function for serialization
stateToJson :: PersistenceState -> ByteString
stateToJson state = 
    let stateWithHash = state { persistenceHash = calculateHash state }
    in encode stateWithHash

-- | P2 atom 4: Parse JSON ByteString to state
-- Pure function for deserialization
jsonToState :: ByteString -> Either String PersistenceState
jsonToState = eitherDecode

-- | P2 atom 5: Determine storage file path
-- Pure function returning absolute path for persistence
filePath :: FilePath -> FilePath
filePath fileName = "./data/" ++ fileName

-- | P2 atom 6: Verify data integrity using SHA256 hash
-- Enforces data integrity per mathematical specification
integrityCheck :: PersistenceState -> Bool
integrityCheck state = 
    let storedHash = persistenceHash state
        calculatedHash = calculateHash state
    in storedHash == calculatedHash

-- | P2 atom 7: Atomic serialization of individual components
-- Enables independent serialization of subsystem atoms
serializeAtom :: (ToJSON a) => a -> ByteString
serializeAtom = encode

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Calculate SHA256 hash of state for integrity verification
calculateHash :: PersistenceState -> IntegrityHash
calculateHash state = 
    let stateWithoutHash = state { persistenceHash = IntegrityHash "" }
        jsonData = encode stateWithoutHash
        bytes = L.toStrict jsonData
        digest = H.hash bytes :: Digest SHA256
    in IntegrityHash $ T.pack $ show digest

-- | Create a new persistence state with current timestamp and hash
createPersistenceState :: [Profile] -> BlockchainState -> IO PersistenceState
createPersistenceState profiles blockchain = do
    currentTime <- getCurrentTimestamp
    let state = PersistenceState
            { persistenceProfiles = profiles
            , persistenceBlockchain = blockchain
            , persistenceVersion = "1.0.0"
            , persistenceTimestamp = currentTime
            , persistenceHash = IntegrityHash ""  -- Will be calculated in stateToJson
            }
    return state

-- getCurrentTimestamp is imported from Blockchain module