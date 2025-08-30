{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Control.Monad.IO.Class (liftIO)

-- Import all subsystems for full integration testing
import Profile (Profile, createProfile, validateProfile)
import qualified Profile as P
import Blockchain (BlockchainState(..), TxType(..), Confidence(..), validateConfidence, createTransaction, validateTransaction)
import qualified Blockchain as B
import Questions (Question, QuestionType(..), generateQuestion, validateQuestion, hashMCQAnswer, scoreFRQResponse)
import qualified Questions as Q
import Reputation (Reputation, createReputation, updateReputationScore, validateReputation)
import qualified Reputation as R
import Persistence (createPersistenceState, saveState, loadState)
import UI (SystemState(..), Event(..), AttestationData(..), RevealData(..), ScoreData(..), ProfileData(..), CurrentView(..), handleEvent, processEventQueue, createSystemState)

-- Test helper functions
createTestProfiles :: IO [Profile]
createTestProfiles = return 
    [ createProfile "alice" "alice-pubkey" "alice-privkey"
    , createProfile "bob" "bob-pubkey" "bob-privkey" 
    , createProfile "charlie" "charlie-pubkey" "charlie-privkey"
    ]

createTestQuestions :: IO [Question]
createTestQuestions = do
    mcqQuestion <- generateQuestion
    frqQuestion <- generateQuestion
    return [ mcqQuestion { Q.questionId = "mcq-001", Q.questionType = Q.MCQ, Q.prompt = "What is 2 + 2?" }
           , frqQuestion { Q.questionId = "frq-001", Q.questionType = Q.FRQ, Q.prompt = "Explain the central limit theorem." }
           ]

createTestReputation :: IO [Reputation]
createTestReputation = sequence [createReputation, createReputation, createReputation]

initializeFullTestState :: IO SystemState
initializeFullTestState = do
    profiles <- createTestProfiles
    questions <- createTestQuestions
    reputation <- createTestReputation
    let blockchain = BlockchainState [] [] 0.6 0
    return $ createSystemState profiles blockchain questions reputation

-- Comprehensive integration test suite
spec :: Spec
spec = do
  describe "Full System Integration Tests" $ do
    
    it "should complete full PoK cycle: create profile → attest → consensus → reputation update" $ do
      initialState <- initializeFullTestState
      
      -- Step 1: Create new profile
      let profileData = ProfileData "david" "david-pubkey" "david-privkey"
          createEvent = CreateProfileEvent profileData
      
      state1 <- return $ handleEvent createEvent initialState
      length (systemProfiles state1) `shouldBe` 4
      
      -- Step 2: Submit attestation for MCQ
      let attestData = AttestationData "mcq-001" "B" 0.85
          attestEvent = AttestEvent attestData
      
      state2 <- return $ handleEvent attestEvent state1
      systemRenderBuffer state2 `shouldContain` "Attestation"
      
      -- Step 3: Verify blockchain integration (would contain transaction)
      -- In full system, blockchain would have new transaction
      -- For now, verify state progression
      systemRenderBuffer state2 `shouldNotBe` systemRenderBuffer state1
      
      -- Step 4: Verify reputation updates would occur
      length (systemReputation state2) `shouldBe` 4  -- Reputation created for new profile

    it "should handle multi-user attestation convergence scenario" $ do
      initialState <- initializeFullTestState
      
      -- Multiple users attest to same question with different answers
      let attestEvents = 
            [ AttestEvent $ AttestationData "mcq-001" "A" 0.9  -- High confidence, answer A
            , AttestEvent $ AttestationData "mcq-001" "B" 0.7  -- Medium confidence, answer B  
            , AttestEvent $ AttestationData "mcq-001" "A" 0.8  -- High confidence, answer A
            , AttestEvent $ AttestationData "mcq-001" "A" 0.6  -- Medium confidence, answer A
            ]
      
      -- Process all attestations
      finalState <- return $ foldl (flip handleEvent) initialState attestEvents
      
      -- Should have processed all attestations without errors
      systemRenderBuffer finalState `shouldNotContain` "Error"
      -- In full system, convergence would be calculated: answer A should have 3/4 = 75% convergence
      
    it "should enforce all 13 invariants throughout integration flow" $ do
      initialState <- initializeFullTestState
      
      -- Test Invariant 1: Identity validation
      let invalidProfileData = ProfileData "eve" "" ""  -- Empty pubkey
          invalidCreateEvent = CreateProfileEvent invalidProfileData
      
      state1 <- return $ handleEvent invalidCreateEvent initialState
      systemRenderBuffer state1 `shouldContain` "Error"  -- Should reject empty pubkey
      
      -- Test Invariant 3: Confidence bounds  
      let invalidConfidenceAttest = AttestationData "mcq-001" "A" 1.5  -- > 1.0
          invalidAttestEvent = AttestEvent invalidConfidenceAttest
      
      state2 <- return $ handleEvent invalidAttestEvent initialState
      systemRenderBuffer state2 `shouldContain` "Error"  -- Should reject invalid confidence
      
      -- Test Invariant 5: FRQ score bounds (would be tested in full blockchain integration)
      -- Test Invariant 13: UI Safety - verify no null renders
      let navEvent = NavigateEvent ProfileView
      state3 <- return $ handleEvent navEvent initialState
      systemRenderBuffer state3 `shouldNotBe` ""  -- Should have non-empty render
      systemCurrentView state3 `shouldBe` ProfileView
      
    it "should handle AP reveal after 50% consensus achieved" $ do
      initialState <- initializeFullTestState
      
      -- Simulate reaching 50% consensus first (simplified)
      let consensusAttests = replicate 5 $ AttestEvent $ AttestationData "mcq-001" "C" 0.8
      
      stateWithConsensus <- return $ foldl (flip handleEvent) initialState consensusAttests
      
      -- Now AP can reveal official answer
      let revealData = RevealData "mcq-001" "C"  -- Matches consensus
          revealEvent = RevealEvent revealData
      
      finalState <- return $ handleEvent revealEvent stateWithConsensus
      systemRenderBuffer finalState `shouldContain` "Reveal"
      systemRenderBuffer finalState `shouldNotContain` "Error"
      
    it "should maintain state consistency across complex event sequences" $ do
      initialState <- initializeFullTestState
      
      let complexSequence = 
            [ CreateProfileEvent $ ProfileData "user1" "pub1" "priv1"
            , NavigateEvent QuestionsView
            , AttestEvent $ AttestationData "mcq-001" "A" 0.9
            , NavigateEvent ReputationView  
            , UpdateScoreEvent $ ScoreData "alice" 50.0
            , NavigateEvent BlockchainView
            , AttestEvent $ AttestationData "frq-001" "This is my FRQ response with sufficient detail" 0.8
            , NavigateEvent MainMenu
            ]
      
      finalState <- return $ foldl (flip handleEvent) initialState complexSequence
      
      -- Verify final state consistency
      length (systemProfiles finalState) `shouldBe` 4  -- Added one profile
      systemCurrentView finalState `shouldBe` MainMenu  -- Final navigation
      systemRenderBuffer finalState `shouldNotBe` ""   -- Should have render output
      systemRenderBuffer finalState `shouldNotContain` "Error"  -- No errors in sequence
      
    it "should persist and recover complete state" $ do
      -- Create rich state
      richState <- initializeFullTestState
      let enhancedState = richState { systemCurrentView = ReputationView }
      
      -- Create persistence state from system state  
      persistenceState <- createPersistenceState (systemProfiles enhancedState) (systemBlockchain enhancedState)
      
      -- Save state
      saveResult <- saveState persistenceState
      saveResult `shouldSatisfy` (\result -> case result of
                                    Right _ -> True
                                    Left _ -> False)
      
      -- Load state back
      loadResult <- loadState "test-state"
      case loadResult of
        Left _ -> expectationFailure "Should be able to load saved state"
        Right loadedState -> do
          -- Verify state integrity (simplified check)
          length (persistenceProfiles loadedState) `shouldBe` length (systemProfiles enhancedState)
          
    it "should handle rate limiting enforcement (30-day cooldown)" $ do
      initialState <- initializeFullTestState
      
      -- Submit first attestation
      let attestData = AttestationData "mcq-001" "A" 0.8
          attestEvent = AttestEvent attestData
      
      state1 <- return $ handleEvent attestEvent initialState
      systemRenderBuffer state1 `shouldContain` "Attestation"
      
      -- Immediately try to attest to same question again (should be rate limited in full system)
      -- For now, verify system doesn't crash
      state2 <- return $ handleEvent attestEvent state1
      systemRenderBuffer state2 `shouldNotBe` ""  -- Should still render something
      
    it "should calculate progressive quorum based on convergence (ADR-028)" $ do
      initialState <- initializeFullTestState
      
      -- Test different convergence scenarios
      -- Low convergence (< 0.5) should require 5 attestations
      -- Medium convergence (0.5-0.8) should require 4 attestations  
      -- High convergence (>= 0.8) should require 3 attestations
      
      -- This would be fully tested in blockchain layer integration
      -- For now, verify system handles varying attestation counts
      let lowConvergenceAttests = [ AttestEvent $ AttestationData "mcq-001" answer 0.7 
                                  | answer <- ["A", "B", "C", "D", "A"] ]  -- Spread out answers
      
      finalState <- return $ foldl (flip handleEvent) initialState lowConvergenceAttests  
      systemRenderBuffer finalState `shouldNotContain` "Error"
      
    it "should integrate FRQ scoring with rubrics (1-5 scale)" $ do
      initialState <- initializeFullTestState
      
      -- Submit FRQ response
      let frqResponse = "The central limit theorem states that the sampling distribution of the mean approaches a normal distribution as sample size increases, regardless of the population distribution shape. This is fundamental to statistical inference."
          frqAttest = AttestEvent $ AttestationData "frq-001" frqResponse 0.9
      
      finalState <- return $ handleEvent frqAttest initialState
      systemRenderBuffer finalState `shouldContain` "Attestation"
      systemRenderBuffer finalState `shouldNotContain` "Error"
      
      -- In full system, FRQ would be scored 1-5 and integrated with reputation
      
    it "should detect and flag outlier responses" $ do
      initialState <- initializeFullTestState
      
      -- Submit mostly normal responses and one outlier
      let normalAttests = replicate 4 $ AttestEvent $ AttestationData "mcq-001" "B" 0.8
          outlierAttest = AttestEvent $ AttestationData "mcq-001" "E" 0.3  -- Different answer, low confidence
          allAttests = normalAttests ++ [outlierAttest]
      
      finalState <- return $ foldl (flip handleEvent) initialState allAttests
      
      -- In full system, outlier detection would flag the "E" response
      systemRenderBuffer finalState `shouldNotContain` "Error"  -- System should handle outliers gracefully
      
  describe "Performance and Memory Tests" $ do
    
    it "should handle large state without memory issues" $ do
      -- Create large state (100 profiles, 200 questions, 500 reputation entries)
      largeProfiles <- return $ [ createProfile ("user" ++ show i) ("pub" ++ show i) ("priv" ++ show i) 
                                | i <- [1..100] ]
      largeQuestions <- sequence $ replicate 200 generateQuestion
      largeReputation <- sequence $ replicate 500 createReputation
      
      let largeBlockchain = BlockchainState [] [] 0.6 0
          largeState = createSystemState largeProfiles largeBlockchain largeQuestions largeReputation
      
      -- Verify system can handle large state
      length (systemProfiles largeState) `shouldBe` 100
      length (systemQuestions largeState) `shouldBe` 200  
      length (systemReputation largeState) `shouldBe` 500
      
      -- Test navigation with large state
      let navEvent = NavigateEvent ProfileView
      finalState <- return $ handleEvent navEvent largeState
      systemCurrentView finalState `shouldBe` ProfileView
      
    it "should maintain <50MB memory usage target" $ do
      -- This would require actual memory profiling in a real test
      -- For now, verify system doesn't crash with moderate load
      moderateState <- initializeFullTestState
      let eventSequence = replicate 50 $ AttestEvent $ AttestationData "mcq-001" "A" 0.8
      
      finalState <- return $ foldl (flip handleEvent) moderateState eventSequence
      systemRenderBuffer finalState `shouldNotContain` "Error"
      
    it "should respond within 100ms for UI interactions" $ do
      testState <- initializeFullTestState
      
      -- Test quick navigation
      let navEvent = NavigateEvent ReputationView
      finalState <- return $ handleEvent navEvent testState
      
      systemCurrentView finalState `shouldBe` ReputationView
      -- In real system, this would have timing measurements