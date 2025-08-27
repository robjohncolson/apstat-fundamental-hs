{-|
Module      : Reputation
Description : R atoms - Reputation data types and operations (10 atoms total)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines the Reputation subsystem with exactly 10 R atoms as specified
in the AP Statistics PoK Blockchain mathematical foundation.
Aligned with ADR-012 and ADR-028 for emergent attestation consensus.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Reputation
    ( -- Data types (10 R data atoms)
      Reputation(..)
    -- Function atoms
    , updateReputationScore
    , calculateMinorityBonus
    , applyTimeDecay
    , updateStreak
    , calculateLeaderboardRank
    -- Validation functions
    , validateReputationScore
    , validateDecayRate
    -- Helper functions
    , createReputation
    , validateReputation
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Blockchain (getCurrentTimestamp, Confidence(..), sha256Hash)

-- ============================================================================
-- R DATA ATOMS (10 atoms per specification)
-- ============================================================================

-- | Reputation data structure with exactly 10 R atoms per specification
data Reputation = Reputation
    { reputationScore :: Double          -- R atom 1: Core reputation score (0-1000 bounds)
    , minorityBonus :: Double            -- R atom 2: Multiplier for minority positions
    , decayRate :: Double                -- R atom 3: Time-based decay factor (0-1)
    , timeWindow :: Int                  -- R atom 4: Seconds for reputation window
    , consensusThreshold :: Double       -- R atom 5: Minimum threshold for rewards
    , baseAccuracyScore :: Double        -- R atom 6: Base accuracy component
    , streakBonus :: Int                 -- R atom 7: Consecutive correct predictions
    , socialScore :: Double              -- R atom 8: Peer interactions component
    , leaderboardRank :: Maybe Int       -- R atom 9: Optional ranking position
    , lastUpdated :: Double              -- R atom 10: Timestamp of last update
    } deriving (Show, Eq, Generic)

instance ToJSON Reputation
instance FromJSON Reputation

-- ============================================================================
-- R FUNCTION ATOMS
-- ============================================================================

-- | Primary reputation update function implementing Invariant 3
-- Applies confidence weighting, minority bonuses, and time decay
updateReputationScore :: Reputation -> Confidence -> Bool -> Double -> IO Reputation
updateReputationScore rep confidence isMinorityPosition accuracy = do
    currentTime <- getCurrentTimestamp
    let confidenceValue = unConfidence confidence
        timeDiff = currentTime - lastUpdated rep
        
        -- Apply time decay per R atom 3
        decayedScore = applyTimeDecay (reputationScore rep) (decayRate rep) timeDiff
        
        -- Calculate confidence-weighted accuracy per Invariant 3
        weightedAccuracy = accuracy * confidenceValue
        
        -- Apply minority bonus if applicable per Invariant 3
        bonusMultiplier = calculateMinorityBonus (minorityBonus rep) isMinorityPosition
        finalAccuracy = weightedAccuracy * bonusMultiplier
        
        -- Update streak
        newStreak = updateStreak (streakBonus rep) (accuracy > consensusThreshold rep)
        streakMultiplier = 1.0 + (fromIntegral newStreak * 0.1)  -- 10% bonus per streak
        
        -- Calculate new reputation score with bounds enforcement
        scoreChange = finalAccuracy * streakMultiplier * 10.0  -- Scale factor
        newScore = max 0.0 (min 1000.0 (decayedScore + scoreChange))
        
        -- Update base accuracy as running average
        newBaseAccuracy = (baseAccuracyScore rep * 0.9) + (accuracy * 0.1)
    
    return rep
        { reputationScore = newScore
        , baseAccuracyScore = newBaseAccuracy
        , streakBonus = newStreak
        , lastUpdated = currentTime
        }

-- | Calculate minority bonus multiplier per Invariant 3
calculateMinorityBonus :: Double -> Bool -> Double
calculateMinorityBonus bonusRate isMinority =
    if isMinority then 1.0 + bonusRate else 1.0

-- | Apply exponential time decay to reputation score
applyTimeDecay :: Double -> Double -> Double -> Double
applyTimeDecay currentScore decayRate' timeDiffSeconds =
    let decayFactor = exp ((-decayRate') * (timeDiffSeconds / 86400.0))  -- Decay per day
    in currentScore * decayFactor

-- | Update streak counter based on correctness
updateStreak :: Int -> Bool -> Int
updateStreak currentStreak isCorrect =
    if isCorrect then currentStreak + 1 else 0

-- | Calculate leaderboard rank among peers
calculateLeaderboardRank :: [Reputation] -> Reputation -> Maybe Int
calculateLeaderboardRank allReputation target =
    let allScores = map reputationScore allReputation
        targetScore = reputationScore target
        sortedScores = reverse $ map reputationScore allReputation
        position = length (filter (> targetScore) allScores) + 1
    in if not (null allReputation) then Just position else Nothing

-- ============================================================================
-- VALIDATION FUNCTIONS
-- ============================================================================

-- | Validate reputation score bounds (0-1000)
validateReputationScore :: Double -> Either Text Double
validateReputationScore score
    | score < 0.0 || score > 1000.0 = Left "Reputation score must be between 0.0 and 1000.0"
    | otherwise = Right score

-- | Validate decay rate bounds (0-1)
validateDecayRate :: Double -> Either Text Double
validateDecayRate rate
    | rate < 0.0 || rate > 1.0 = Left "Decay rate must be between 0.0 and 1.0"
    | otherwise = Right rate

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Create a new reputation with default values
createReputation :: IO Reputation
createReputation = do
    currentTime <- getCurrentTimestamp
    return Reputation
        { reputationScore = 100.0        -- Starting score
        , minorityBonus = 0.5            -- 50% bonus for minority positions
        , decayRate = 0.1                -- 10% decay per day
        , timeWindow = 86400             -- 24 hours in seconds
        , consensusThreshold = 0.6       -- 60% threshold for rewards
        , baseAccuracyScore = 0.5        -- Starting at 50% accuracy
        , streakBonus = 0                -- No initial streak
        , socialScore = 0.0              -- No initial social score
        , leaderboardRank = Nothing      -- Unranked initially
        , lastUpdated = currentTime
        }

-- | Validate reputation per system invariants
validateReputation :: Reputation -> Either Text Reputation
validateReputation rep
    | reputationScore rep < 0.0 || reputationScore rep > 1000.0 = 
        Left "Invalid reputation score: must be between 0.0 and 1000.0"
    | minorityBonus rep < 0.0 = Left "Invalid minority bonus: must be non-negative"
    | decayRate rep < 0.0 || decayRate rep > 1.0 = 
        Left "Invalid decay rate: must be between 0.0 and 1.0"
    | timeWindow rep <= 0 = Left "Invalid time window: must be positive"
    | consensusThreshold rep < 0.0 || consensusThreshold rep > 1.0 = 
        Left "Invalid consensus threshold: must be between 0.0 and 1.0"
    | baseAccuracyScore rep < 0.0 || baseAccuracyScore rep > 1.0 = 
        Left "Invalid base accuracy score: must be between 0.0 and 1.0"
    | streakBonus rep < 0 = Left "Invalid streak bonus: must be non-negative"
    | socialScore rep < 0.0 = Left "Invalid social score: must be non-negative"
    | lastUpdated rep <= 0 = Left "Invalid timestamp: must be positive"
    | otherwise = Right rep