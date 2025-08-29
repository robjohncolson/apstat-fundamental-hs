{-|
Module      : Main
Description : Main application entry point integrating all 58 atoms
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This is the main executable entry point for the apstat-fundamental-hs application.
It implements the full system integration with UI loop, state management, and
all invariants enforced according to ADR-012 and ADR-028.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (catch, IOException)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import System.IO (hFlush, stdout)

import Profile (Profile, createProfile, validateProfile)
import Blockchain (BlockchainState(..), Transaction, Attestation, TxType(..), Confidence, validateConfidence, createTransaction, createAttestation, getCurrentTimestamp, calculateConsensus)
import Questions (Question, generateQuestion, validateAnswer, hashMCQAnswer)
import Reputation (Reputation, createReputation, updateReputationScore)
import Persistence (PersistenceState(..), saveState, loadState, createPersistenceState)
import UI (SystemState(..), CurrentView(..), Event(..), EventQueue, AttestationData(..), RevealData(..), ScoreData(..), renderState, handleEvent, processEventQueue, createSystemState)

-- | Main entry point - loads state, runs UI loop, saves on exit
main :: IO ()
main = do
    putStrLn "AP Statistics PoK Blockchain - Fundamental Architecture"
    putStrLn "Integrating all 58 atoms across 6 subsystems"
    putStrLn ""
    
    -- Phase 1: Initialize or load state
    initialState <- initializeOrLoadState
    
    -- Phase 2: Run main UI loop
    finalState <- mainUILoop initialState
    
    -- Phase 3: Save state on exit
    saveStateOnExit finalState
    
    putStrLn "Application terminated. State saved."

-- | Initialize new state or load existing state
initializeOrLoadState :: IO SystemState
initializeOrLoadState = do
    putStrLn "Loading application state..."
    result <- loadState "apstat-state.json" `catch` handleLoadError
    case result of
        Left err -> do
            putStrLn $ "Could not load existing state: " ++ T.unpack err
            putStrLn "Creating new initial state..."
            createInitialState
        Right persistenceState -> do
            putStrLn "Loaded existing application state"
            convertPersistenceToSystemState persistenceState
  where
    handleLoadError :: IOException -> IO (Either Text PersistenceState)
    handleLoadError _ = return $ Left "File not found"

-- | Create initial system state with sample data
createInitialState :: IO SystemState
createInitialState = do
    -- Generate initial questions (5 questions as per plan)
    questions <- sequence [generateQuestion | _ <- [1..5]]
    
    -- Create initial reputations
    reps <- sequence [createReputation | _ <- [1..2]]
    
    let initialBlockchain = BlockchainState
            { blockchainTransactions = []
            , blockchainAttestations = []
            , blockchainConsensusThreshold = 0.75
            , blockchainCurrentHeight = 0
            }
    
    let initialProfiles = []  -- Start with no profiles
    
    return $ createSystemState initialProfiles initialBlockchain questions reps

-- | Convert PersistenceState to SystemState (for loaded state)
convertPersistenceToSystemState :: PersistenceState -> IO SystemState
convertPersistenceToSystemState pState = do
    -- Generate some questions if none exist
    questions <- if null (persistenceProfiles pState)
                 then sequence [generateQuestion | _ <- [1..3]]
                 else return []
    
    -- Create reputations for profiles
    reps <- sequence [createReputation | _ <- persistenceProfiles pState]
    
    return $ createSystemState 
        (persistenceProfiles pState)
        (persistenceBlockchain pState)
        questions
        reps

-- | Main UI loop - render, read input, process events, repeat
mainUILoop :: SystemState -> IO SystemState
mainUILoop state = do
    -- Render current state
    putStrLn $ "\n" ++ replicate 50 '='
    putStrLn $ renderState state
    putStrLn $ replicate 50 '='
    
    -- Display command options based on current view
    displayCommands (systemCurrentView state)
    
    -- Read user input
    putStr "> "
    hFlush stdout
    input <- getLine
    
    -- Parse input to events
    case parseInput (systemCurrentView state) input of
        Left err -> do
            putStrLn $ "Error: " ++ err
            mainUILoop state
        Right Nothing -> 
            -- Exit command
            return state
        Right (Just event) -> do
            -- Process event and continue loop
            let stateWithEvent = state { systemEventQueue = [event] }
            newState <- processEventQueue [event] stateWithEvent
            mainUILoop newState

-- | Display available commands based on current view
displayCommands :: CurrentView -> IO ()
displayCommands view = do
    putStrLn "\nAvailable commands:"
    case view of
        MainMenu -> do
            putStrLn "1 - Profile View"
            putStrLn "2 - Blockchain View"
            putStrLn "3 - Questions View"
            putStrLn "4 - Reputation View"
            putStrLn "q - Exit"
        ProfileView -> do
            putStrLn "create <username> - Create new profile"
            putStrLn "list - Show all profiles"
            putStrLn "back - Return to Main Menu"
        BlockchainView -> do
            putStrLn "status - Show blockchain statistics"
            putStrLn "transactions - List recent transactions"
            putStrLn "consensus <questionId> - Show consensus for question"
            putStrLn "back - Return to Main Menu"
        QuestionsView -> do
            putStrLn "list - Show available questions"
            putStrLn "attest <questionId> <answer> <confidence> - Create attestation"
            putStrLn "reveal <questionId> <answer> - AP reveal (if convergence > 50%)"
            putStrLn "back - Return to Main Menu"
        ReputationView -> do
            putStrLn "leaderboard - Show reputation rankings"
            putStrLn "details <userId> - Show reputation details"
            putStrLn "back - Return to Main Menu"

-- | Parse user input into events
parseInput :: CurrentView -> String -> Either String (Maybe Event)
parseInput view input = case words input of
    [] -> Left "Empty command"
    ["q"] -> Right Nothing  -- Exit
    ["back"] -> Right $ Just $ NavigateEvent MainMenu
    
    -- Main menu navigation
    ["1"] | view == MainMenu -> Right $ Just $ NavigateEvent ProfileView
    ["2"] | view == MainMenu -> Right $ Just $ NavigateEvent BlockchainView
    ["3"] | view == MainMenu -> Right $ Just $ NavigateEvent QuestionsView
    ["4"] | view == MainMenu -> Right $ Just $ NavigateEvent ReputationView
    
    -- Profile commands
    ["create", username] | view == ProfileView -> 
        Right $ Just $ UpdateScoreEvent $ ScoreData (T.pack username) 0.0
    ["list"] | view == ProfileView -> 
        Right $ Just $ NavigateEvent ProfileView  -- Refresh view
    
    -- Questions commands
    ["list"] | view == QuestionsView -> 
        Right $ Just $ NavigateEvent QuestionsView  -- Refresh view
    ["attest", qId, answer, confStr] | view == QuestionsView -> 
        case reads confStr of
            [(conf, "")] -> 
                if conf >= 1.0 && conf <= 5.0
                then Right $ Just $ AttestEvent $ AttestationData (T.pack qId) (T.pack answer) conf
                else Left "Confidence must be between 1.0 and 5.0"
            _ -> Left "Invalid confidence value"
    ["reveal", qId, answer] | view == QuestionsView -> 
        Right $ Just $ RevealEvent $ RevealData (T.pack qId) (T.pack answer)
    
    -- Blockchain commands
    ["status"] | view == BlockchainView -> 
        Right $ Just $ NavigateEvent BlockchainView  -- Refresh view
    ["transactions"] | view == BlockchainView -> 
        Right $ Just $ NavigateEvent BlockchainView  -- Refresh view
    ["consensus", qId] | view == BlockchainView -> 
        Right $ Just $ NavigateEvent BlockchainView  -- Refresh view with consensus
    
    -- Reputation commands
    ["leaderboard"] | view == ReputationView -> 
        Right $ Just $ NavigateEvent ReputationView  -- Refresh view
    ["details", userId] | view == ReputationView -> 
        Right $ Just $ NavigateEvent ReputationView  -- Refresh view with details
    
    _ -> Left "Unknown command"

-- | Save system state to persistence on exit
saveStateOnExit :: SystemState -> IO ()
saveStateOnExit state = do
    putStrLn "Saving application state..."
    
    -- Convert SystemState to PersistenceState
    persistenceState <- convertSystemToPersistenceState state
    
    -- Save state
    result <- saveState persistenceState
    case result of
        Left err -> putStrLn $ "Error saving state: " ++ T.unpack err
        Right _ -> putStrLn "Application state saved successfully"

-- | Convert SystemState to PersistenceState for saving
convertSystemToPersistenceState :: SystemState -> IO PersistenceState
convertSystemToPersistenceState state = 
    createPersistenceState 
        (systemProfiles state) 
        (systemBlockchain state)
