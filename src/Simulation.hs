{-|
Module      : Main
Description : Entry point for the simulation flow
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module provides the main entry point for the AP Statistics fundamental concepts
simulation. It orchestrates the flow: attest -> consensus -> save.
-}

module Simulation
    ( main
    , runSimulation
    , simulationStep
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import System.Environment (getArgs)

import Profile (Profile, createProfile, validateProfile)
import Blockchain (BlockchainState(..), Attestation, Transaction, calculateConsensus, createAttestation, createTransaction, TxType(..), validateConfidence)
import Persistence (PersistenceState(..), saveState, loadState, stateToJson, jsonToState, createPersistenceState)

-- | Main entry point for the application
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "AP Statistics Fundamental Concepts - Haskell Implementation"
            putStrLn "Usage: apstat-fundamental-hs-exe [simulation-steps]"
            runSimulation 1
        [stepsStr] -> case reads stepsStr of
            [(steps, "")] -> runSimulation steps
            _ -> do
                putStrLn "Error: Invalid number of simulation steps"
                putStrLn "Usage: apstat-fundamental-hs-exe [simulation-steps]"
        _ -> do
            putStrLn "Error: Too many arguments"
            putStrLn "Usage: apstat-fundamental-hs-exe [simulation-steps]"

-- | Run the main simulation with the specified number of steps
runSimulation :: Int -> IO ()
runSimulation steps = do
    putStrLn $ "Running simulation with " ++ show steps ++ " steps"
    
    -- Initialize or load existing state
    stateResult <- loadState "simulation-state.json"
    initialState <- case stateResult of
        Left err -> do
            putStrLn $ "Could not load existing state: " ++ T.unpack err
            putStrLn "Creating new initial state"
            createInitialState
        Right state -> do
            putStrLn "Loaded existing simulation state"
            return state
    
    -- Run simulation steps
    finalState <- runSimulationSteps steps initialState
    
    -- Save final state
    saveResult <- saveState finalState
    case saveResult of
        Left err -> putStrLn $ "Error saving state: " ++ T.unpack err
        Right _ -> putStrLn "Simulation state saved successfully"
    
    putStrLn "Simulation completed"

-- | Create initial application state
createInitialState :: IO PersistenceState
createInitialState = do
    let initialBlockchain = BlockchainState
            { blockchainTransactions = []
            , blockchainAttestations = []
            , blockchainConsensusThreshold = 0.75
            , blockchainCurrentHeight = 0
            }
    
    createPersistenceState [] initialBlockchain

-- | Run multiple simulation steps
runSimulationSteps :: Int -> PersistenceState -> IO PersistenceState
runSimulationSteps 0 state = return state
runSimulationSteps n state = do
    putStrLn $ "Running simulation step " ++ show (n - (n - 1))
    newState <- simulationStep state
    runSimulationSteps (n - 1) newState

-- | Perform a single simulation step: attest -> consensus -> save
simulationStep :: PersistenceState -> IO PersistenceState
simulationStep state = do
    putStrLn "  - Creating attestations..."
    stateWithAttestations <- createAttestations state
    
    putStrLn "  - Calculating consensus..."
    stateWithConsensus <- calculateConsensusStep stateWithAttestations
    
    putStrLn "  - Updating state..."
    return stateWithConsensus

-- | Create sample attestations for the simulation
createAttestations :: PersistenceState -> IO PersistenceState
createAttestations state = do
    currentTime <- getCurrentTime
    let blockchain = persistenceBlockchain state
    
    -- First create a transaction for the attestation
    Right conf <- return $ validateConfidence 0.85
    transaction <- createTransaction 
            AttestationTx 
            (T.pack "statistical_concept_1") 
            (T.pack "pubkey_validator") 
            (T.pack "sig") 
            (Just (T.pack "A")) 
            Nothing 
            conf 
            (T.pack "privkey")
    
    newAttestation <- createAttestation
            (T.pack "att_001")
            (T.pack "validator_1")
            transaction
    
    let updatedBlockchain = blockchain
            { blockchainAttestations = newAttestation : blockchainAttestations blockchain
            }
    
    return state { persistenceBlockchain = updatedBlockchain }

-- | Calculate consensus from current attestations
calculateConsensusStep :: PersistenceState -> IO PersistenceState
calculateConsensusStep state = do
    let blockchain = persistenceBlockchain state
    let attestations = blockchainAttestations blockchain
    let consensusMap = calculateConsensus attestations (T.pack "statistical_concept_1")
    
    if null (show consensusMap)
        then putStrLn "    No consensus reached"
        else putStrLn $ "    Consensus results: " ++ show consensusMap
    
    return state
