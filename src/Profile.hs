{-|
Module      : Profile
Description : P atoms - Profile data types and operations (11 atoms total)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines the Profile subsystem with exactly 11 P atoms as specified
in the AP Statistics PoK Blockchain mathematical foundation.
Aligned with ADR-012 and ADR-028 for emergent attestation consensus.
-}

{-# LANGUAGE DeriveGeneric #-}

module Profile
    ( Profile(..)
    , Archetype(..)
    , updateProfile
    , updateProfileWithConsensus
    , createProfile
    , validateProfile
    , calculateArchetype
    , updateReputationScore
    , validateConfidence
    ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- | Archetype enumeration for user classification (P atom 11)
data Archetype = Aces | Strategists | Explorers | Learners | Socials
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Archetype
instance FromJSON Archetype

-- | Profile data structure with exactly 11 P atoms per specification
data Profile = Profile
    { userId :: String                    -- P atom 1: User identifier
    , pubKey :: String                    -- P atom 2: Public cryptographic key
    , privKey :: String                   -- P atom 3: Private cryptographic key  
    , reputationScore :: Float            -- P atom 4: Current reputation (0-1000)
    , attestationHistory :: [String]      -- P atom 5: List of question IDs
    , confidenceLevel :: Float            -- P atom 6: Average confidence level
    , lastAttestationTimestamp :: Float   -- P atom 7: Last attestation time
    , outlierFlags :: Int                 -- P atom 8: Outlier detection count
    , minorityBonusCount :: Int           -- P atom 9: Minority correct answers
    , streak :: Int                       -- P atom 10: Consecutive correct
    , archetype :: Archetype              -- P atom 11: User classification
    } deriving (Show, Eq, Generic)

instance ToJSON Profile
instance FromJSON Profile

-- | Create a new profile with initial values (P function atom)
createProfile :: String -> String -> String -> Profile
createProfile userId' pubKey' privKey' = Profile
    { userId = userId'
    , pubKey = pubKey'
    , privKey = privKey'
    , reputationScore = 100.0  -- Initial reputation
    , attestationHistory = []
    , confidenceLevel = 0.5
    , lastAttestationTimestamp = 0.0
    , outlierFlags = 0
    , minorityBonusCount = 0
    , streak = 0
    , archetype = Explorers  -- Default archetype
    }

-- | Update profile with attestation result (P function atom)
-- Takes (isMatch, questionId) and updates history and streak
updateProfile :: Profile -> (Bool, String) -> Profile
updateProfile profile (isMatch, questionId) = profile
    { attestationHistory = questionId : attestationHistory profile
    , streak = if isMatch then streak profile + 1 else 0
    }

-- | Enhanced update profile with consensus data (P function atom)
-- Updates P atoms 5,6,7,8,9,10 based on attestation results
updateProfileWithConsensus :: Profile -> Bool -> String -> Float -> Float -> Bool -> Bool -> Profile
updateProfileWithConsensus profile isMatch questionId confidence timestamp isOutlier isMinority = profile
    { attestationHistory = questionId : attestationHistory profile  -- P atom 5
    , confidenceLevel = updateConfidenceLevel (confidenceLevel profile) confidence (length $ attestationHistory profile)  -- P atom 6
    , lastAttestationTimestamp = timestamp  -- P atom 7
    , outlierFlags = if isOutlier then outlierFlags profile + 1 else outlierFlags profile  -- P atom 8
    , minorityBonusCount = if isMinority && isMatch then minorityBonusCount profile + 1 else minorityBonusCount profile  -- P atom 9
    , streak = if isMatch then streak profile + 1 else 0  -- P atom 10
    }

-- | Calculate weighted average confidence level
updateConfidenceLevel :: Float -> Float -> Int -> Float
updateConfidenceLevel currentAvg newConfidence historyLength
    | historyLength == 0 = newConfidence
    | otherwise = (currentAvg * fromIntegral historyLength + newConfidence) / fromIntegral (historyLength + 1)

-- | Calculate archetype based on performance metrics (P function atom)
-- Port from Racket reference lines 263-276, enforces mathematical classification
calculateArchetype :: Float -> Float -> Int -> Float -> Archetype
calculateArchetype accuracy responseTime questionsAnswered socialScore
    | accuracy >= 0.9 && responseTime < 3000 && questionsAnswered >= 50 = Aces
    | accuracy >= 0.85 && responseTime >= 5000 && responseTime <= 8000 && questionsAnswered >= 30 = Strategists
    | socialScore >= 0.8 = Socials
    | accuracy >= 0.6 && accuracy <= 0.8 && questionsAnswered >= 20 = Learners
    | otherwise = Explorers

-- | Update reputation score with minority bonus and decay (P function atom)
-- Port from Racket reference lines 742-758, enforces Invariant bounds
updateReputationScore :: Profile -> Float -> Bool -> Float -> Profile
updateReputationScore profile accuracy isMinorityCorrect streakCount = 
    let currentRep = reputationScore profile
        baseAccuracyScore = if accuracy > 0.5 
                           then accuracy * 100  -- Positive for correct
                           else (-50) * (1 - accuracy)  -- Negative for incorrect
        minorityBonus = if isMinorityCorrect then 1.5 else 1.0  -- MINORITY_BONUS_MULTIPLIER
        streakScore = streakCount * 2  -- Simple streak bonus
        delta = (baseAccuracyScore * minorityBonus) + streakScore
        totalScore = currentRep + delta
        boundedScore = max 0 (min totalScore 1000)  -- MAX_REPUTATION_SCORE bounds
    in profile { reputationScore = boundedScore }

-- | Validate confidence bounds per mathematical specification
validateConfidence :: Float -> Either String Float
validateConfidence conf
    | conf < 0.0 || conf > 1.0 = Left "Confidence must be between 0.0 and 1.0"
    | otherwise = Right conf

-- | Validate profile per Invariant 1 (valid attester public key)
validateProfile :: Profile -> Either String Profile
validateProfile profile
    | null (pubKey profile) = Left "Invalid attester public key: empty pubkey"
    | null (userId profile) = Left "Invalid user ID: empty userId"
    | reputationScore profile < 0 || reputationScore profile > 1000 = Left "Reputation score out of bounds (0-1000)"
    | confidenceLevel profile < 0 || confidenceLevel profile > 1.0 = Left "Confidence level out of bounds (0.0-1.0)"
    | otherwise = Right profile