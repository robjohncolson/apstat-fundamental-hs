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

module Main
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
import Blockchain (BlockchainState(..), Attestation, Transaction, calculateConsensus, createAttestation)
import Persistence (AppState(..), saveStateToFile, loadStateFromFile)

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
    stateResult <- loadStateFromFile "simulation-state.json"
    initialState <- case stateResult of
        Left err -> do
            putStrLn $ "Could not load existing state: " ++ err
            putStrLn "Creating new initial state"
            createInitialState
        Right state -> do
            putStrLn "Loaded existing simulation state"
            return state
    
    -- Run simulation steps
    finalState <- runSimulationSteps steps initialState
    
    -- Save final state
    saveResult <- saveStateToFile "simulation-state.json" finalState
    case saveResult of
        Left err -> putStrLn $ "Error saving state: " ++ err
        Right _ -> putStrLn "Simulation state saved successfully"
    
    putStrLn "Simulation completed"

-- | Create initial application state
createInitialState :: IO AppState
createInitialState = do
    currentTime <- getCurrentTime
    let initialBlockchain = BlockchainState
            { blockchainBlocks = []
            , blockchainTransactionPool = []
            , blockchainAttestationPool = []
            , blockchainConsensusThreshold = 0.75
            }
    
    return AppState
        { appStateProfiles = []
        , appStateBlockchain = initialBlockchain
        , appStateVersion = "0.1.0"
        , appStateMetadata = "Initial simulation state"
        }

-- | Run multiple simulation steps
runSimulationSteps :: Int -> AppState -> IO AppState
runSimulationSteps 0 state = return state
runSimulationSteps n state = do
    putStrLn $ "Running simulation step " ++ show (n - (n - 1))
    newState <- simulationStep state
    runSimulationSteps (n - 1) newState

-- | Perform a single simulation step: attest -> consensus -> save
simulationStep :: AppState -> IO AppState
simulationStep state = do
    putStrLn "  - Creating attestations..."
    stateWithAttestations <- createAttestations state
    
    putStrLn "  - Calculating consensus..."
    stateWithConsensus <- calculateConsensusStep stateWithAttestations
    
    putStrLn "  - Updating state..."
    return stateWithConsensus

-- | Create sample attestations for the simulation
createAttestations :: AppState -> IO AppState
createAttestations state = do
    currentTime <- getCurrentTime
    let blockchain = appStateBlockchain state
    let newAttestation = createAttestation
            "att_001"
            "validator_1"
            "statistical_concept_1"
            0.85
            currentTime
            "proof_of_understanding"
    
    let updatedBlockchain = blockchain
            { blockchainAttestationPool = newAttestation : blockchainAttestationPool blockchain
            }
    
    return state { appStateBlockchain = updatedBlockchain }

-- | Calculate consensus from current attestations
calculateConsensusStep :: AppState -> IO AppState
calculateConsensusStep state = do
    let blockchain = appStateBlockchain state
    let attestations = blockchainAttestationPool blockchain
    let consensus = calculateConsensus attestations "statistical_concept_1"
    
    case consensus of
        Nothing -> putStrLn "    No consensus reached"
        Just value -> putStrLn $ "    Consensus value: " ++ show value
    
    return state
