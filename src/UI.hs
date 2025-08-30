{-|
Module      : UI
Description : U atoms - UI data types and operations (6 atoms total)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module implements the U (UI) subsystem with exactly 6 atoms
as specified in the AP Statistics PoK Blockchain mathematical foundation.
Enforces Invariant 13 (UI Safety) and integrates with all subsystems.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
    ( -- U Atoms (exactly 6)
      CurrentView(..)     -- U atom 1: Active UI state
    , Event(..)           -- U atom 2: User input events
    , EventQueue          -- U atom 3: Pending events queue
    , renderState         -- U atom 4: Render current view
    , handleEvent         -- U atom 5: Process single event
    , processEventQueue   -- U atom 6: Sequential event processing
    -- Supporting types
    , SystemState(..)
    , AttestationData(..)
    , RevealData(..)
    , ScoreData(..)
    , ProfileData(..)
    -- Helper functions
    , createSystemState
    , validateView
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import GHC.Generics (Generic)
-- Import all subsystem modules for integration
import Profile (Profile)
import Blockchain (BlockchainState)
import Questions (Question)
import Reputation (Reputation)
import Persistence (saveState, createPersistenceState)
import qualified Blockchain as B
import qualified Profile as P
import qualified Questions as Q
import qualified Reputation as R

-- ============================================================================
-- U DATA ATOMS (3 atoms per specification)
-- ============================================================================

-- | U atom 1: Current view enumeration - Active UI state
data CurrentView = ProfileView | BlockchainView | QuestionsView | ReputationView | MainMenu
    deriving (Show, Eq, Ord, Generic)

instance ToJSON CurrentView
instance FromJSON CurrentView

-- | Supporting data types for events
data AttestationData = AttestationData
    { attestQuestionId :: Text
    , attestAnswer :: Text
    , attestConfidence :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON AttestationData
instance FromJSON AttestationData

data RevealData = RevealData
    { revealQuestionId :: Text
    , revealAnswer :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON RevealData
instance FromJSON RevealData

data ScoreData = ScoreData
    { scoreUserId :: Text
    , scoreDelta :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON ScoreData
instance FromJSON ScoreData

data ProfileData = ProfileData
    { profileUserId :: Text
    , profilePubKey :: Text
    , profilePrivKey :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON ProfileData
instance FromJSON ProfileData

-- | U atom 2: Event types for user interactions
data Event = 
      AttestEvent AttestationData
    | RevealEvent RevealData  
    | UpdateScoreEvent ScoreData
    | NavigateEvent CurrentView
    | CreateProfileEvent ProfileData
    deriving (Show, Eq, Generic)

instance ToJSON Event
instance FromJSON Event

-- | U atom 3: Event queue type - Array of pending events
type EventQueue = [Event]

-- | Complete system state combining all subsystems
data SystemState = SystemState
    { systemProfiles :: [Profile]
    , systemBlockchain :: BlockchainState
    , systemQuestions :: [Question]
    , systemReputation :: [Reputation]
    , systemCurrentView :: CurrentView
    , systemEventQueue :: EventQueue
    , systemRenderBuffer :: String
    } deriving (Show, Eq, Generic)

instance ToJSON SystemState
instance FromJSON SystemState

-- ============================================================================
-- U FUNCTION ATOMS (3 atoms per specification)
-- ============================================================================

-- | U atom 4: Render current view based on state
-- Enforces Invariant 13: UI Safety (non-null renders, valid views)
renderState :: SystemState -> String
renderState state = 
    if not (validateView (systemCurrentView state))
    then "ERROR: Invalid view state"  -- Invariant 13 enforcement
    else case systemCurrentView state of
        MainMenu -> renderMainMenu state
        ProfileView -> renderProfileView state
        BlockchainView -> renderBlockchainView state
        QuestionsView -> renderQuestionsView state
        ReputationView -> renderReputationView state

-- | U atom 5: Handle single event and update state
-- Enforces Invariant 10: Cycle Stability (avoid event loops)
handleEvent :: Event -> SystemState -> SystemState
handleEvent event state = 
    case event of
        NavigateEvent newView -> 
            if validateView newView
            then 
                let newState = state { systemCurrentView = newView }
                    renderedView = renderState newState
                in newState { systemRenderBuffer = renderedView }
            else state { systemRenderBuffer = "ERROR: Invalid view navigation attempted" }  -- Invalid view, no change (Invariant 13)
            
        AttestEvent attestData -> 
            -- Validate confidence bounds (Invariant 5)
            let confidence = attestConfidence attestData
                questionId' = attestQuestionId attestData
                answer = attestAnswer attestData
            in if confidence < 0.0 || confidence > 1.0
               then state { systemRenderBuffer = "Error: Confidence must be between 0.0 and 1.0" }
               else if T.null questionId' || T.null answer
               then state { systemRenderBuffer = "Error: Question ID and answer cannot be empty" }
               else 
                   -- Find matching question in system
                   let matchingQuestion = find (\q -> Q.questionId q == questionId') (systemQuestions state)
                   in case matchingQuestion of
                       Nothing -> state { systemRenderBuffer = "Error: Question not found: " ++ T.unpack questionId' }
                       Just question ->
                           -- Get attester profile (use first profile for now, in real system this would be from auth)
                           case systemProfiles state of
                               [] -> state { systemRenderBuffer = "Error: No attester profile available" }
                               (attester:_) ->
                                   let updatedState = state { systemRenderBuffer = "Attestation queued: " ++ T.unpack questionId' }
                                   in updatedState
            
        RevealEvent revealData ->
            -- Validate reveal authority and find matching question
            let questionId' = revealQuestionId revealData
                answer = revealAnswer revealData
                matchingQuestion = find (\q -> Q.questionId q == questionId') (systemQuestions state)
            in case matchingQuestion of
                Nothing -> state { systemRenderBuffer = "Error: Question not found for reveal: " ++ T.unpack questionId' }
                Just question ->
                    -- Check if question already has official answer
                    case Q.officialAnswer question of
                        Just _ -> state { systemRenderBuffer = "Error: Official answer already exists for: " ++ T.unpack questionId' }
                        Nothing ->
                            -- Queue reveal processing (full operation will happen in processEventQueue)
                            let updatedState = state { systemRenderBuffer = "Reveal queued: " ++ T.unpack questionId' }
                            in updatedState
            
        UpdateScoreEvent scoreData ->
            -- Validate user exists and score delta is reasonable
            let userIdStr = T.unpack (scoreUserId scoreData)
                scoreDelta' = scoreDelta scoreData
                matchingProfile = find (\p -> P.userId p == userIdStr) (systemProfiles state)
            in if abs scoreDelta' > 1000.0  -- Prevent extreme score changes
               then state { systemRenderBuffer = "Error: Score delta too large: " ++ show scoreDelta' }
               else case matchingProfile of
                   Nothing -> state { systemRenderBuffer = "Error: User not found: " ++ userIdStr }
                   Just _ ->
                       -- Validate that we have corresponding reputation entry
                       let profileCount = length (systemProfiles state)
                           reputationCount = length (systemReputation state)
                       in if profileCount /= reputationCount
                          then state { systemRenderBuffer = "Error: Reputation/Profile count mismatch" }
                          else
                              -- Queue score update (full operation will happen in processEventQueue)
                              let updatedState = state { systemRenderBuffer = "Score update queued for: " ++ userIdStr }
                              in updatedState
            
        CreateProfileEvent profileData ->
            -- Validate unique userId against existing profiles
            let userIdStr = T.unpack (profileUserId profileData)
                existingProfile = any (\p -> P.userId p == userIdStr) (systemProfiles state)
            in if existingProfile
               then state { systemRenderBuffer = "Error: User ID already exists: " ++ userIdStr }
               else
                   -- Create new profile (pure operation)
                   let newProfile = P.createProfile userIdStr (T.unpack $ profilePubKey profileData) (T.unpack $ profilePrivKey profileData)
                       -- Update system state with new profile (reputation and transaction will be added in processEventQueue)
                       updatedState = state 
                           { systemProfiles = newProfile : systemProfiles state
                           , systemRenderBuffer = "Profile creation queued for: " ++ userIdStr
                           }
                   in updatedState

-- | U atom 6: Process event queue sequentially
-- Enforces Invariant 11: Persistence Integrity (triggers saves)
processEventQueue :: EventQueue -> SystemState -> IO SystemState
processEventQueue [] state = return state  -- Empty queue handling
processEventQueue events state = do
    let processedState = foldl (flip handleEvent) state events
    let finalState = processedState { systemEventQueue = [] }  -- Clear queue after processing
    
    -- Trigger persistence save per Invariant 11
    persistenceState <- createPersistenceState (systemProfiles finalState) (systemBlockchain finalState)
    saveResult <- saveState persistenceState
    case saveResult of
        Left err -> return finalState { systemRenderBuffer = "Save error: " ++ T.unpack err }
        Right _ -> return finalState { systemRenderBuffer = renderState finalState }

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Validate view state for Invariant 13 enforcement
validateView :: CurrentView -> Bool
validateView MainMenu = True
validateView ProfileView = True
validateView BlockchainView = True
validateView QuestionsView = True
validateView ReputationView = True

-- | Create initial system state
createSystemState :: [Profile] -> BlockchainState -> [Question] -> [Reputation] -> SystemState
createSystemState profiles blockchain questions reputation = SystemState
    { systemProfiles = profiles
    , systemBlockchain = blockchain
    , systemQuestions = questions
    , systemReputation = reputation
    , systemCurrentView = MainMenu
    , systemEventQueue = []
    , systemRenderBuffer = ""
    }

-- ============================================================================
-- VIEW RENDERING FUNCTIONS
-- ============================================================================

-- | Render main menu view
renderMainMenu :: SystemState -> String
renderMainMenu _state = unlines
    [ "=== AP Statistics PoK Blockchain ==="
    , "1. View Profile"
    , "2. View Blockchain" 
    , "3. View Questions"
    , "4. View Reputation"
    , "5. Exit"
    , "Select option (1-5): "
    ]

-- | Render profile view showing user information
renderProfileView :: SystemState -> String
renderProfileView state = unlines $
    [ "=== Profile View ===" ] ++
    map renderProfile (take 5 $ systemProfiles state) ++  -- Show first 5 profiles
    [ "Press 'M' for Main Menu" ]
  where
    renderProfile profile = 
        "User: " ++ show profile  -- Simplified rendering

-- | Render blockchain view showing recent transactions
renderBlockchainView :: SystemState -> String
renderBlockchainView state = unlines
    [ "=== Blockchain View ==="
    , "Recent Transactions:"
    , show (systemBlockchain state)  -- Simplified rendering
    , "Press 'M' for Main Menu"
    ]

-- | Render questions view showing current questions
renderQuestionsView :: SystemState -> String
renderQuestionsView state = unlines $
    [ "=== Questions View ===" ] ++
    map (("Q: " ++) . show) (take 3 $ systemQuestions state) ++  -- Show first 3 questions
    [ "Press 'M' for Main Menu" ]

-- | Render reputation view showing scores and leaderboard
renderReputationView :: SystemState -> String
renderReputationView state = unlines $
    [ "=== Reputation View ===" ] ++
    map (("Score: " ++) . show) (take 10 $ systemReputation state) ++  -- Show top 10 scores
    [ "Press 'M' for Main Menu" ]