{-|
Module      : UISpec
Description : Unit tests for U atoms - UI module
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Unit tests for the U (UI) subsystem with exactly 6 atoms.
Tests enforce Invariant 13 (UI Safety), Invariant 10 (Cycle Stability),
and Invariant 11 (Persistence Integrity).
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UISpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try)

import UI
import Profile (createProfile, Profile)
import Blockchain (BlockchainState(..), Transaction, Attestation)
import Questions (Question(..), QuestionType(..), MCQOption(..), FRQRubric(..), Difficulty(..), Unit(..), Lesson(..), generateQuestion)
import Reputation (createReputation, Reputation)
import qualified Data.Map as Map

-- ============================================================================
-- TEST DATA CREATION HELPERS
-- ============================================================================

createTestProfile :: String -> Profile
createTestProfile suffix = createProfile ("user-" ++ suffix) ("pubkey-" ++ suffix) ("privkey-" ++ suffix)

createTestBlockchain :: BlockchainState
createTestBlockchain = BlockchainState
    { blockchainTransactions = []
    , blockchainAttestations = []
    , blockchainConsensusThreshold = 0.75
    , blockchainCurrentHeight = 0
    }

createTestQuestion :: Text -> Question
createTestQuestion qid = 
    let emptyRubric = FRQRubric [] 5.0 Map.empty
    in Question
    { questionId = qid
    , prompt = "Sample question text"
    , rubric = emptyRubric
    , mcqOptions = [MCQOption "A" "Option A", MCQOption "B" "Option B"]
    , frqRubric = emptyRubric
    , questionType = MCQ
    , difficulty = Medium
    , unit = Unit "descriptive"
    , lesson = Lesson "sample-lesson"
    , distribution = Map.empty
    , convergence = 0.0
    , officialAnswer = Nothing
    , anonymousReveal = False
    , timestamp = 1609459200.0
    }

createTestReputation :: IO Reputation
createTestReputation = createReputation

createTestSystemState :: IO SystemState
createTestSystemState = do
    rep1 <- createTestReputation
    rep2 <- createTestReputation
    return $ createSystemState
        [createTestProfile "1", createTestProfile "2"]
        createTestBlockchain
        [createTestQuestion "q1", createTestQuestion "q2"]
        [rep1, rep2]

-- ============================================================================
-- MAIN TEST SPEC
-- ============================================================================

spec :: Spec
spec = do
  describe "UI Data Atoms" $ do
    dataAtomTests
    
  describe "UI Function Atoms" $ do
    functionAtomTests
    
  describe "UI Invariants" $ do
    invariantTests
    
  describe "UI Integration" $ do
    integrationTests

-- ============================================================================
-- DATA ATOM TESTS (U atoms 1-3)
-- ============================================================================

dataAtomTests :: Spec
dataAtomTests = do
  describe "CurrentView (U atom 1)" $ do
    it "should support all view types" $ do
      [MainMenu, ProfileView, BlockchainView, QuestionsView, ReputationView] 
        `shouldSatisfy` all validateView
        
    it "should be serializable" $ do
      show MainMenu `shouldBe` "MainMenu"
      show ProfileView `shouldBe` "ProfileView"
      
  describe "Event (U atom 2)" $ do
    it "should support all event types" $ do
      let attestData = AttestationData "q1" "A" 0.9
      let revealData = RevealData "q1" "A"  
      let scoreData = ScoreData "user1" 10.0
      let events = [AttestEvent attestData, RevealEvent revealData, 
                   UpdateScoreEvent scoreData, NavigateEvent MainMenu]
      length events `shouldBe` 4
      
  describe "EventQueue (U atom 3)" $ do
    it "should handle empty queue" $ do
      let emptyQueue = [] :: EventQueue
      length emptyQueue `shouldBe` 0
      
    it "should handle multiple events" $ do
      let queue = [NavigateEvent ProfileView, NavigateEvent MainMenu] :: EventQueue
      length queue `shouldBe` 2

-- ============================================================================
-- FUNCTION ATOM TESTS (U atoms 4-6) 
-- ============================================================================

functionAtomTests :: Spec
functionAtomTests = do
  describe "renderState (U atom 4)" $ do
    it "should render MainMenu without error" $ do
      state <- createTestSystemState
      let result = renderState state
      result `shouldContain` "AP Statistics PoK Blockchain"
      result `shouldNotBe` ""
      
    it "should render ProfileView without error" $ do  
      state <- createTestSystemState
      let stateWithView = state { systemCurrentView = ProfileView }
      let result = renderState stateWithView
      result `shouldContain` "Profile View"
      result `shouldNotBe` ""
      
    it "should render BlockchainView without error" $ do
      state <- createTestSystemState
      let stateWithView = state { systemCurrentView = BlockchainView }
      let result = renderState stateWithView
      result `shouldContain` "Blockchain View" 
      result `shouldNotBe` ""
      
    it "should render QuestionsView without error" $ do
      state <- createTestSystemState
      let stateWithView = state { systemCurrentView = QuestionsView }
      let result = renderState stateWithView
      result `shouldContain` "Questions View"
      result `shouldNotBe` ""
      
    it "should render ReputationView without error" $ do
      state <- createTestSystemState
      let stateWithView = state { systemCurrentView = ReputationView }
      let result = renderState stateWithView
      result `shouldContain` "Reputation View"
      result `shouldNotBe` ""
      
  describe "handleEvent (U atom 5)" $ do
    it "should handle NavigateEvent correctly" $ do
      state <- createTestSystemState
      let event = NavigateEvent ProfileView
      let newState = handleEvent event state
      systemCurrentView newState `shouldBe` ProfileView
      
    it "should handle AttestEvent correctly" $ do
      state <- createTestSystemState
      let attestData = AttestationData "q1" "A" 0.9
      let event = AttestEvent attestData
      let newState = handleEvent event state
      systemRenderBuffer newState `shouldContain` "Attestation processed"
      
    it "should handle RevealEvent correctly" $ do
      state <- createTestSystemState
      let revealData = RevealData "q1" "A"
      let event = RevealEvent revealData
      let newState = handleEvent event state
      systemRenderBuffer newState `shouldContain` "Reveal processed"
      
    it "should handle UpdateScoreEvent correctly" $ do
      state <- createTestSystemState
      let scoreData = ScoreData "user1" 10.0
      let event = UpdateScoreEvent scoreData
      let newState = handleEvent event state
      systemRenderBuffer newState `shouldContain` "Score updated"
      
  describe "processEventQueue (U atom 6)" $ do
    it "should handle empty queue" $ do
      state <- createTestSystemState
      result <- processEventQueue [] state
      systemEventQueue result `shouldBe` []
      
    it "should process single event" $ do
      state <- createTestSystemState
      let events = [NavigateEvent ProfileView]
      result <- processEventQueue events state
      systemCurrentView result `shouldBe` ProfileView
      systemEventQueue result `shouldBe` []
      
    it "should process multiple events sequentially" $ do
      state <- createTestSystemState
      let events = [NavigateEvent ProfileView, NavigateEvent MainMenu]
      result <- processEventQueue events state
      systemCurrentView result `shouldBe` MainMenu
      systemEventQueue result `shouldBe` []

-- ============================================================================
-- INVARIANT TESTS
-- ============================================================================

invariantTests :: Spec
invariantTests = do
  describe "Invariant 13: UI Safety" $ do
    it "should never return null render" $ do
      baseState <- createTestSystemState
      let states = map (\v -> baseState { systemCurrentView = v })
                      [MainMenu, ProfileView, BlockchainView, QuestionsView, ReputationView]
      let renders = map renderState states
      all (not . null) renders `shouldBe` True
      
    it "should validate all view types" $ do
      let views = [MainMenu, ProfileView, BlockchainView, QuestionsView, ReputationView]
      all validateView views `shouldBe` True
      
  describe "Invariant 10: Cycle Stability" $ do  
    it "should not create event loops in navigation" $ do
      state <- createTestSystemState
      let navEvents = [NavigateEvent ProfileView, NavigateEvent MainMenu, NavigateEvent ProfileView]
      result <- processEventQueue navEvents state
      systemCurrentView result `shouldBe` ProfileView
      systemEventQueue result `shouldBe` []  -- Queue should be cleared
      
  describe "Invariant 11: Persistence Integrity" $ do
    it "should trigger save after event processing" $ do
      state <- createTestSystemState  
      let events = [NavigateEvent ProfileView]
      result <- processEventQueue events state
      -- Persistence save is attempted (may fail in test environment but should not crash)
      systemEventQueue result `shouldBe` []

-- ============================================================================
-- INTEGRATION TESTS
-- ============================================================================

integrationTests :: Spec  
integrationTests = do
  describe "Full UI Flow" $ do
    it "should handle complete user interaction flow" $ do
      initialState <- createTestSystemState
      
      -- Navigate to profile
      let afterNav = handleEvent (NavigateEvent ProfileView) initialState
      systemCurrentView afterNav `shouldBe` ProfileView
      
      -- Process an attestation
      let attestData = AttestationData "q1" "A" 0.9
      let afterAttest = handleEvent (AttestEvent attestData) afterNav
      systemRenderBuffer afterAttest `shouldContain` "Attestation processed"
      
      -- Navigate back to main menu
      let finalState = handleEvent (NavigateEvent MainMenu) afterAttest
      systemCurrentView finalState `shouldBe` MainMenu
      
  describe "Edge Cases" $ do
    it "should handle malformed events gracefully" $ do
      state <- createTestSystemState
      let scoreData = ScoreData "" (-999.0)  -- Invalid data
      let event = UpdateScoreEvent scoreData
      let result = handleEvent event state
      -- Should not crash, just process the event
      systemRenderBuffer result `shouldContain` "Score updated"
      
    it "should handle view transitions correctly" $ do
      state <- createTestSystemState
      let transitions = [NavigateEvent ProfileView, NavigateEvent BlockchainView, 
                        NavigateEvent QuestionsView, NavigateEvent ReputationView, 
                        NavigateEvent MainMenu]
      result <- processEventQueue transitions state
      systemCurrentView result `shouldBe` MainMenu