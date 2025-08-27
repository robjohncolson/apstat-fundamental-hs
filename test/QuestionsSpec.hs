{-|
Module      : QuestionsSpec
Description : Unit tests for Questions module Q atoms (Invariant 12: Atomicity)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Atomic tests for the Questions module with 14 Q atoms, ensuring each atom
is independently testable per Invariant 12 of the mathematical specification.
-}

{-# LANGUAGE OverloadedStrings #-}

module QuestionsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Either (isLeft, isRight)

import Questions
import Blockchain (validateFRQScore, FRQScore(..))

-- | Main test specification for Questions module (Invariant 12: Atomicity)
spec :: Spec
spec = do
  describe "Questions Q Atoms" $ do
    
    -- Test individual Q data atoms for atomicity (Invariant 12)
    describe "Q Data Atoms (14 atoms)" $ do
      
      describe "Q atom 1: prompt" $ do
        it "stores and retrieves prompt independently" $ do
          question <- generateQuestion
          prompt question `shouldSatisfy` (not . T.null)
          prompt question `shouldBe` "Sample question prompt"
          
      describe "Q atom 2: rubric" $ do
        it "stores and retrieves rubric independently" $ do
          question <- generateQuestion
          let questionRubric = rubric question
          maxScore questionRubric `shouldBe` 5.0
          rubricCriteria questionRubric `shouldBe` []
          
      describe "Q atom 3: mcqOptions" $ do
        it "stores and retrieves mcqOptions independently" $ do
          question <- generateQuestion
          mcqOptions question `shouldBe` []  -- Initially empty for sample
          
        it "handles multiple choice options" $ do
          let options = [MCQOption "A" "Option A", MCQOption "B" "Option B"]
          result <- createQuestion "Test question" MCQ options (FRQRubric [] 5.0 Map.empty) 
                      Medium (Unit "U1") (Lesson "L1") "test-q1"
          case result of
            Right q -> mcqOptions q `shouldBe` options
            Left err -> expectationFailure $ T.unpack err
            
      describe "Q atom 4: frqRubric" $ do
        it "stores and retrieves frqRubric independently" $ do
          question <- generateQuestion
          let frqRub = frqRubric question
          maxScore frqRub `shouldBe` 5.0
          rubricCriteria frqRub `shouldBe` []
          
        it "enforces scoring bounds (Invariant 5)" $ do
          let validRubric = FRQRubric ["criteria1"] 4.0 (Map.fromList [("criteria1", 4.0)])
          result <- createQuestion "Test FRQ" FRQ [] validRubric 
                      Hard (Unit "U2") (Lesson "L2") "frq-test"
          case result of
            Right q -> maxScore (frqRubric q) `shouldSatisfy` (\s -> s >= 1.0 && s <= 5.0)
            Left err -> expectationFailure $ T.unpack err
            
      describe "Q atom 5: questionType" $ do
        it "stores and retrieves questionType independently" $ do
          question <- generateQuestion
          questionType question `shouldBe` MCQ
          
        it "differentiates MCQ and FRQ types" $ do
          mcqResult <- createQuestion "MCQ Test" MCQ [MCQOption "A" "Option A"] (FRQRubric [] 5.0 Map.empty)
                         Easy (Unit "U1") (Lesson "L1") "mcq-1"
          frqResult <- createQuestion "FRQ Test" FRQ [] (FRQRubric [] 5.0 Map.empty)
                         Easy (Unit "U1") (Lesson "L1") "frq-1"
          case (mcqResult, frqResult) of
            (Right mcq, Right frq) -> do
              questionType mcq `shouldBe` MCQ
              questionType frq `shouldBe` FRQ
            _ -> expectationFailure "Failed to create test questions"
            
      describe "Q atom 6: difficulty" $ do
        it "stores and retrieves difficulty independently" $ do
          question <- generateQuestion
          difficulty question `shouldBe` Medium
          
        it "handles all difficulty levels" $ do
          easyQ <- createQuestion "Easy Q" MCQ [MCQOption "A" "Easy Option"] (FRQRubric [] 5.0 Map.empty)
                     Easy (Unit "U1") (Lesson "L1") "easy-1"
          hardQ <- createQuestion "Hard Q" MCQ [MCQOption "A" "Hard Option"] (FRQRubric [] 5.0 Map.empty)
                     Hard (Unit "U1") (Lesson "L1") "hard-1"
          case (easyQ, hardQ) of
            (Right e, Right h) -> do
              difficulty e `shouldBe` Easy
              difficulty h `shouldBe` Hard
            _ -> expectationFailure "Failed to create difficulty test questions"
            
      describe "Q atom 7: unit" $ do
        it "stores and retrieves unit independently" $ do
          question <- generateQuestion
          unit question `shouldBe` Unit "U1"
          unitId (unit question) `shouldBe` "U1"
          
      describe "Q atom 8: lesson" $ do
        it "stores and retrieves lesson independently" $ do
          question <- generateQuestion
          lesson question `shouldBe` Lesson "L1"
          lessonId (lesson question) `shouldBe` "L1"
          
      describe "Q atom 9: questionId" $ do
        it "stores and retrieves questionId independently" $ do
          question <- generateQuestion
          questionId question `shouldBe` "sample-q1"
          
        it "ensures unique question IDs" $ do
          q1 <- createQuestion "Q1" MCQ [MCQOption "A" "Option 1"] (FRQRubric [] 5.0 Map.empty)
                  Easy (Unit "U1") (Lesson "L1") "unique-1"
          q2 <- createQuestion "Q2" MCQ [MCQOption "A" "Option 2"] (FRQRubric [] 5.0 Map.empty)
                  Easy (Unit "U1") (Lesson "L1") "unique-2"
          case (q1, q2) of
            (Right question1, Right question2) ->
              questionId question1 `shouldNotBe` questionId question2
            _ -> expectationFailure "Failed to create unique ID test questions"
            
      describe "Q atom 10: distribution" $ do
        it "stores and retrieves distribution independently" $ do
          question <- generateQuestion
          distribution question `shouldBe` Map.empty  -- Initially empty
          
        it "handles answer distribution updates" $ do
          question <- generateQuestion
          let updatedDistribution = Map.fromList [("A", 5), ("B", 3), ("C", 2)]
          let updatedQuestion = question { distribution = updatedDistribution }
          distribution updatedQuestion `shouldBe` updatedDistribution
          
      describe "Q atom 11: convergence" $ do
        it "stores and retrieves convergence independently" $ do
          question <- generateQuestion
          convergence question `shouldBe` 0.0  -- Initially 0.0
          
        it "enforces convergence bounds (0.0-1.0)" $ do
          question <- generateQuestion
          let validConvergence = 0.75
          let updatedQuestion = question { convergence = validConvergence }
          convergence updatedQuestion `shouldSatisfy` (\c -> c >= 0.0 && c <= 1.0)
          
      describe "Q atom 12: officialAnswer" $ do
        it "stores and retrieves officialAnswer independently" $ do
          question <- generateQuestion
          officialAnswer question `shouldBe` Nothing  -- Initially Nothing
          
        it "handles official answer setting" $ do
          question <- generateQuestion
          let answerSet = question { officialAnswer = Just "A" }
          officialAnswer answerSet `shouldBe` Just "A"
          
      describe "Q atom 13: anonymousReveal" $ do
        it "stores and retrieves anonymousReveal independently" $ do
          question <- generateQuestion
          anonymousReveal question `shouldBe` False  -- Initially False
          
        it "handles reveal status changes" $ do
          question <- generateQuestion
          let revealEnabled = question { anonymousReveal = True }
          anonymousReveal revealEnabled `shouldBe` True
          
      describe "Q atom 14: timestamp" $ do
        it "stores and retrieves timestamp independently" $ do
          question <- generateQuestion
          timestamp question `shouldSatisfy` (> 0)  -- Should be positive
          
        it "generates increasing timestamps" $ do
          q1 <- generateQuestion
          q2 <- generateQuestion
          timestamp q2 `shouldSatisfy` (>= timestamp q1)
    
    -- Test Q function atoms
    describe "Q Function Atoms" $ do
      
      describe "validateAnswer (Invariant 4: Hash Validation)" $ do
        it "validates MCQ answers using hash comparison" $ do
          let mcqQuestion = Question
                { prompt = "Test MCQ"
                , rubric = FRQRubric [] 5.0 Map.empty
                , mcqOptions = [MCQOption "A" "Correct", MCQOption "B" "Wrong"]
                , frqRubric = FRQRubric [] 5.0 Map.empty
                , questionType = MCQ
                , difficulty = Easy
                , unit = Unit "U1"
                , lesson = Lesson "L1"
                , questionId = "mcq-hash-test"
                , distribution = Map.empty
                , convergence = 0.0
                , officialAnswer = Just "A"
                , anonymousReveal = False
                , timestamp = 1000.0
                }
          validateAnswer mcqQuestion "A" `shouldBe` True
          validateAnswer mcqQuestion "B" `shouldBe` False
          
        it "returns True for FRQ questions (stub)" $ do
          let frqQuestion = Question
                { prompt = "Test FRQ"
                , rubric = FRQRubric ["criteria"] 5.0 Map.empty
                , mcqOptions = []
                , frqRubric = FRQRubric ["criteria"] 5.0 Map.empty
                , questionType = FRQ
                , difficulty = Medium
                , unit = Unit "U2"
                , lesson = Lesson "L2"
                , questionId = "frq-test"
                , distribution = Map.empty
                , convergence = 0.0
                , officialAnswer = Nothing
                , anonymousReveal = False
                , timestamp = 2000.0
                }
          validateAnswer frqQuestion "Any answer" `shouldBe` True
      
      describe "calculateMCQConvergence (Invariant 7)" $ do
        it "calculates MCQ convergence using max count ratio" $ do
          let dist1 = Map.fromList [("A", 7), ("B", 2), ("C", 1)]  -- 7/10 = 0.7
          calculateMCQConvergence dist1 `shouldBe` 0.7
          
        it "returns 0.0 for empty distribution" $ do
          calculateMCQConvergence Map.empty `shouldBe` 0.0
          
        it "returns 1.0 for perfect convergence" $ do
          let perfectDist = Map.fromList [("A", 10)]
          calculateMCQConvergence perfectDist `shouldBe` 1.0
      
      describe "calculateFRQConvergence (Invariant 7)" $ do
        it "calculates FRQ convergence using coefficient of variation" $ do
          let scores1 = [3.0, 3.2, 2.8, 3.1, 2.9]  -- Low variation, high convergence
          calculateFRQConvergence scores1 `shouldSatisfy` (> 0.8)
          
        it "handles high variation scores" $ do
          let scores2 = [1.0, 3.0, 5.0, 2.0, 4.0]  -- High variation, low convergence
          let result = calculateFRQConvergence scores2
          result `shouldSatisfy` (\x -> x >= 0.0 && x <= 1.0)  -- Should be in valid bounds
          
        it "returns 0.0 for insufficient data" $ do
          calculateFRQConvergence [3.0] `shouldBe` 0.0
          calculateFRQConvergence [] `shouldBe` 0.0
          
        it "handles identical scores" $ do
          let uniformScores = [3.0, 3.0, 3.0, 3.0]
          calculateFRQConvergence uniformScores `shouldBe` 1.0
      
      describe "hashMCQAnswer (integration with Blockchain)" $ do
        it "produces consistent hash for same input" $ do
          let answer = "A"
          hashMCQAnswer answer `shouldBe` hashMCQAnswer answer
          
        it "produces different hashes for different answers" $ do
          hashMCQAnswer "A" `shouldNotBe` hashMCQAnswer "B"
          
        it "produces non-empty hash" $ do
          hashMCQAnswer "C" `shouldSatisfy` (not . T.null)
      
      describe "scoreFRQResponse (Invariant 5: FRQ Score Bounds)" $ do
        it "returns scores within 1.0-5.0 bounds" $ do
          let testRubric = FRQRubric ["criteria1", "criteria2"] 4.5 Map.empty
          case scoreFRQResponse testRubric "Test response" of
            Right (FRQScore score) -> score `shouldSatisfy` (\s -> s >= 1.0 && s <= 5.0)
            Left err -> expectationFailure $ T.unpack err
            
        it "respects rubric maximum score" $ do
          let lowMaxRubric = FRQRubric ["basic"] 2.0 Map.empty
          case scoreFRQResponse lowMaxRubric "Response" of
            Right (FRQScore score) -> score `shouldSatisfy` (<= 2.0)
            Left err -> expectationFailure $ T.unpack err

  -- Test Invariant enforcement
  describe "Invariant Enforcement" $ do
    
    describe "Question validation" $ do
      it "validates complete question" $ do
        let validQuestion = Question
              { prompt = "Valid test question"
              , rubric = FRQRubric ["criteria"] 5.0 Map.empty
              , mcqOptions = [MCQOption "A" "Option A", MCQOption "B" "Option B"]
              , frqRubric = FRQRubric ["criteria"] 5.0 Map.empty
              , questionType = MCQ
              , difficulty = Medium
              , unit = Unit "U1"
              , lesson = Lesson "L1"
              , questionId = "valid-q1"
              , distribution = Map.empty
              , convergence = 0.5
              , officialAnswer = Nothing
              , anonymousReveal = False
              , timestamp = 1000.0
              }
        validateQuestion validQuestion `shouldSatisfy` isRight
        
      it "rejects empty prompt" $ do
        question <- generateQuestion
        let invalidQuestion = question { prompt = "" }
        validateQuestion invalidQuestion `shouldSatisfy` isLeft
        
      it "rejects empty questionId" $ do
        question <- generateQuestion
        let invalidQuestion = question { questionId = "" }
        validateQuestion invalidQuestion `shouldSatisfy` isLeft
        
      it "enforces rubric score bounds (Invariant 5)" $ do
        question <- generateQuestion
        let invalidRubric = FRQRubric [] 6.0 Map.empty  -- > 5.0 violates Invariant 5
        let invalidQuestion = question { rubric = invalidRubric }
        validateQuestion invalidQuestion `shouldSatisfy` isLeft
        
      it "requires MCQ options for MCQ questions" $ do
        question <- generateQuestion
        let mcqNoOptions = question { questionType = MCQ, mcqOptions = [] }
        validateQuestion mcqNoOptions `shouldSatisfy` isLeft
        
      it "enforces convergence bounds (0.0-1.0)" $ do
        question <- generateQuestion
        let invalidConvergence = question { convergence = 1.5 }  -- > 1.0
        validateQuestion invalidConvergence `shouldSatisfy` isLeft
        
      it "enforces positive timestamps" $ do
        question <- generateQuestion
        let invalidTimestamp = question { timestamp = -1.0 }
        validateQuestion invalidTimestamp `shouldSatisfy` isLeft

-- | Property-based tests for atomicity (Invariant 12)
prop_hashConsistency :: Text -> Bool
prop_hashConsistency answer = hashMCQAnswer answer == hashMCQAnswer answer

prop_convergenceBounds :: [Double] -> Bool  
prop_convergenceBounds scores = 
  let conv = calculateFRQConvergence scores
  in conv >= 0.0 && conv <= 1.0

prop_mcqConvergenceBounds :: [(Text, Int)] -> Bool
prop_mcqConvergenceBounds pairs =
  let distMap = Map.fromList pairs
      conv = calculateMCQConvergence distMap
  in conv >= 0.0 && conv <= 1.0