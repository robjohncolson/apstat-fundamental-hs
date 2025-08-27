{-|
Module      : Questions
Description : Q atoms - Questions data types and operations (14 atoms total)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines the Questions subsystem with exactly 14 Q atoms as specified
in the AP Statistics PoK Blockchain mathematical foundation.
Aligned with ADR-012 and ADR-028 for emergent attestation consensus.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Questions
    ( -- Data types (14 Q data atoms)
      Question(..)
    , QuestionType(..)
    , MCQOption(..)
    , FRQRubric(..)
    , Difficulty(..)
    , Unit(..)
    , Lesson(..)
    -- Function atoms
    , generateQuestion
    , validateAnswer
    , calculateMCQConvergence
    , calculateFRQConvergence
    , hashMCQAnswer
    , scoreFRQResponse
    -- Helper functions
    , createQuestion
    , loadQuestions
    , validateQuestion
    ) where

import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Blockchain (sha256Hash, getCurrentTimestamp, FRQScore(..), validateFRQScore)

-- | Question type enumeration (Q atom 5)
data QuestionType = MCQ | FRQ
    deriving (Show, Eq, Ord, Generic)

instance ToJSON QuestionType
instance FromJSON QuestionType

-- | Multiple choice option (part of Q atom 3)
data MCQOption = MCQOption
    { optionKey :: Text      -- A, B, C, D, E
    , optionValue :: Text    -- The choice text
    } deriving (Show, Eq, Generic)

instance ToJSON MCQOption
instance FromJSON MCQOption

-- | FRQ rubric for scoring (Q atom 4)
data FRQRubric = FRQRubric
    { rubricCriteria :: [Text]  -- List of scoring criteria
    , maxScore :: Double        -- Maximum possible score (1.0-5.0 per Invariant 5)
    , scoringGuide :: Map Text Double  -- Criteria to point mapping
    } deriving (Show, Eq, Generic)

instance ToJSON FRQRubric
instance FromJSON FRQRubric

-- | Difficulty level enumeration (Q atom 6)
data Difficulty = Easy | Medium | Hard
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Difficulty
instance FromJSON Difficulty

-- | Unit identifier (Q atom 7)
newtype Unit = Unit { unitId :: Text }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Unit
instance FromJSON Unit

-- | Lesson identifier (Q atom 8)
newtype Lesson = Lesson { lessonId :: Text }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Lesson
instance FromJSON Lesson

-- | Question data structure with exactly 14 Q atoms per specification
data Question = Question
    { prompt :: Text                        -- Q atom 1: Question text
    , rubric :: FRQRubric                   -- Q atom 2: Scoring criteria (applicable to both MCQ/FRQ)
    , mcqOptions :: [MCQOption]             -- Q atom 3: Multiple choice options
    , frqRubric :: FRQRubric                -- Q atom 4: FRQ specific rubric
    , questionType :: QuestionType          -- Q atom 5: MCQ or FRQ
    , difficulty :: Difficulty              -- Q atom 6: Easy/Medium/Hard
    , unit :: Unit                          -- Q atom 7: Curriculum unit
    , lesson :: Lesson                      -- Q atom 8: Curriculum lesson
    , questionId :: Text                    -- Q atom 9: Unique identifier
    , distribution :: Map Text Int          -- Q atom 10: Answer distribution
    , convergence :: Double                 -- Q atom 11: Consensus metric (0.0-1.0)
    , officialAnswer :: Maybe Text          -- Q atom 12: Canonical answer for AP reveal
    , anonymousReveal :: Bool               -- Q atom 13: Whether AP can reveal anonymously
    , timestamp :: Double                   -- Q atom 14: Creation timestamp
    } deriving (Show, Eq, Generic)

instance ToJSON Question
instance FromJSON Question

-- ============================================================================
-- Q FUNCTION ATOMS
-- ============================================================================

-- | Generate a new question (stub for MVP)
-- IO function for question creation with current timestamp
generateQuestion :: IO Question
generateQuestion = do
    currentTime <- getCurrentTimestamp
    let emptyRubric = FRQRubric [] 5.0 Map.empty
    return Question
        { prompt = "Sample question prompt"
        , rubric = emptyRubric
        , mcqOptions = []
        , frqRubric = emptyRubric
        , questionType = MCQ
        , difficulty = Medium
        , unit = Unit "U1"
        , lesson = Lesson "L1"
        , questionId = "sample-q1"
        , distribution = Map.empty
        , convergence = 0.0
        , officialAnswer = Nothing
        , anonymousReveal = False
        , timestamp = currentTime
        }

-- | Validate MCQ answer using hash comparison (Invariant 4)
-- Implements hash validation per mathematical specification
validateAnswer :: Question -> Text -> Bool
validateAnswer question answer =
    case questionType question of
        MCQ -> case officialAnswer question of
            Just correctAnswer -> hashMCQAnswer answer == hashMCQAnswer correctAnswer
            Nothing -> False  -- No official answer to validate against
        FRQ -> True  -- FRQ validation requires human scoring, stub returns True

-- | Calculate MCQ convergence using max count ratio (Invariant 7)
-- Convergence = max_count / total_attestations
calculateMCQConvergence :: Map Text Int -> Double
calculateMCQConvergence distMap
    | Map.null distMap = 0.0
    | otherwise = 
        let totalCount = sum $ Map.elems distMap
            maxCount = maximum $ Map.elems distMap
        in if totalCount > 0
           then fromIntegral maxCount / fromIntegral totalCount
           else 0.0

-- | Calculate FRQ convergence using coefficient of variation (Invariant 7)
-- Convergence = 1 / coefficient_of_variation (higher consistency = higher convergence)
calculateFRQConvergence :: [Double] -> Double
calculateFRQConvergence scores
    | length scores < 2 = 0.0
    | otherwise =
        let mean' = sum scores / fromIntegral (length scores)
            variance = sum [(x - mean')^(2::Int) | x <- scores] / fromIntegral (length scores)
            stdDev = sqrt variance
            cv = if mean' /= 0 then stdDev / mean' else 1.0
        in if cv > 0 then min 1.0 (1.0 / cv) else 1.0

-- | Hash MCQ answer using SHA256 (integrates with Blockchain module)
-- Pure function for MCQ answer hashing per Invariant 4
hashMCQAnswer :: Text -> Text
hashMCQAnswer = sha256Hash

-- | Score FRQ response against rubric (Invariant 5: 1.0-5.0 bounds)
-- Returns FRQScore with bounds enforcement
scoreFRQResponse :: FRQRubric -> Text -> Either Text FRQScore
scoreFRQResponse rubric _response = 
    -- Stub implementation - would analyze response against rubric criteria
    let baseScore = 3.0  -- Default middle score
        boundedScore = max 1.0 (min (maxScore rubric) baseScore)
    in validateFRQScore boundedScore

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Create a question with validation
createQuestion :: Text -> QuestionType -> [MCQOption] -> FRQRubric -> 
                 Difficulty -> Unit -> Lesson -> Text -> IO (Either Text Question)
createQuestion promptText qType options rubric' diff unit' lesson' qid = do
    currentTime <- getCurrentTimestamp
    let question = Question
            { prompt = promptText
            , rubric = rubric'
            , mcqOptions = options
            , frqRubric = rubric'
            , questionType = qType
            , difficulty = diff
            , unit = unit'
            , lesson = lesson'
            , questionId = qid
            , distribution = Map.empty
            , convergence = 0.0
            , officialAnswer = Nothing
            , anonymousReveal = False
            , timestamp = currentTime
            }
    case validateQuestion question of
        Left err -> return $ Left err
        Right validQuestion -> return $ Right validQuestion

-- | Load questions from JSON file (integrates with curriculum.json)
loadQuestions :: FilePath -> IO (Either Text [Question])
loadQuestions filePath = do
    result <- L.readFile filePath
    case eitherDecode result of
        Left err -> return $ Left $ T.pack $ "Failed to parse questions: " ++ err
        Right questions -> return $ Right questions

-- | Validate question per system invariants
validateQuestion :: Question -> Either Text Question
validateQuestion question
    | T.null (prompt question) = Left "Invalid prompt: empty prompt text"
    | T.null (questionId question) = Left "Invalid question ID: empty questionId"
    | maxScore (rubric question) < 1.0 || maxScore (rubric question) > 5.0 = 
        Left "Invalid rubric: maxScore must be between 1.0 and 5.0 (Invariant 5)"
    | questionType question == MCQ && null (mcqOptions question) = 
        Left "Invalid MCQ: must have at least one option"
    | convergence question < 0.0 || convergence question > 1.0 = 
        Left "Invalid convergence: must be between 0.0 and 1.0"
    | timestamp question <= 0 = Left "Invalid timestamp: must be positive"
    | otherwise = Right question