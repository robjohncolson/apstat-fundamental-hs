{-|
Module      : Blockchain
Description : B atoms - Blockchain data types and operations (25 atoms total)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines the Blockchain subsystem with exactly 25 B atoms as specified
in the AP Statistics PoK Blockchain mathematical foundation.
Aligned with ADR-012 and ADR-028 for emergent attestation consensus.
-}

{-# LANGUAGE DeriveGeneric #-}

module Blockchain
    ( -- Data types (19 B data atoms)
      Transaction(..)
    , Attestation(..) 
    , BlockchainState(..)
    , Confidence(..)
    , FRQScore(..)
    , TxType(..)
    , QuestionDistribution
    , MCQDistribution
    , FRQScores
    -- Function atoms (6 B function atoms)
    , sha256Hash
    , getCurrentTimestamp
    , validateSignature
    , calculateConsensus
    , updateDistributions
    , detectOutliers
    -- Helper functions
    , createTransaction
    , createAttestation
    , validateTransaction
    , validateConfidence
    , validateFRQScore
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Crypto.Hash (SHA256(..), Digest)
import qualified Crypto.Hash as H

-- | Constrained confidence type enforcing Invariant 3 (0.0-1.0 bounds)
newtype Confidence = Confidence { unConfidence :: Double }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Confidence
instance FromJSON Confidence

-- | Constrained FRQ score type enforcing Invariant 5 (1.0-5.0 bounds)
newtype FRQScore = FRQScore { unFRQScore :: Double }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON FRQScore
instance FromJSON FRQScore

-- | Transaction type enumeration (B atom 11)
data TxType = AttestationTx | CreateUserTx
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TxType
instance FromJSON TxType

-- | Type aliases for distribution data structures
type QuestionDistribution = Map Text Int       -- B atom 13: question answer counts
type MCQDistribution = Map Text Int           -- B atom 14: MCQ choice frequencies  
type FRQScores = [FRQScore]                   -- B atom 15: list of FRQ scores

-- | Transaction data structure with exactly 19 B data atoms per specification
data Transaction = Transaction
    { hash :: Text                            -- B atom 1: SHA256 hash
    , prevHash :: Text                        -- B atom 2: Previous block hash
    , timestamp :: Double                     -- B atom 3: Unix timestamp
    , nonce :: Int                            -- B atom 4: Proof-of-work nonce
    , questionId :: Text                      -- B atom 5: Question identifier
    , answerHash :: Maybe Text                -- B atom 6: MCQ answer hash
    , answerText :: Maybe Text                -- B atom 7: FRQ answer text
    , score :: Maybe FRQScore                 -- B atom 8: FRQ score (1.0-5.0)
    , attesterPubkey :: Text                  -- B atom 9: Attester public key
    , signature :: Text                       -- B atom 10: Digital signature
    , txType :: TxType                        -- B atom 11: Transaction type
    , isMatch :: Maybe Bool                   -- B atom 12: Answer match result
    , questionDistribution :: QuestionDistribution  -- B atom 13: Answer distribution
    , mcqDistribution :: MCQDistribution      -- B atom 14: MCQ choice counts
    , frqScores :: FRQScores                  -- B atom 15: FRQ score list
    , convergence :: Double                   -- B atom 16: Convergence metric
    , confidence :: Confidence                -- B atom 17: Confidence level
    , anonymousSignature :: Maybe Text        -- B atom 18: Anonymous signature
    , officialAnswer :: Maybe Text            -- B atom 19: Canonical answer
    } deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- | Attestation data structure for consensus operations
data Attestation = Attestation
    { attestationId :: Text
    , attestationValidator :: Text
    , attestationTransaction :: Transaction
    , attestationTimestamp :: Double
    , attestationConfidence :: Confidence
    } deriving (Show, Eq, Generic)

instance ToJSON Attestation
instance FromJSON Attestation

-- | Overall blockchain state for persistence
data BlockchainState = BlockchainState
    { blockchainTransactions :: [Transaction]
    , blockchainAttestations :: [Attestation]
    , blockchainConsensusThreshold :: Double
    , blockchainCurrentHeight :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON BlockchainState
instance FromJSON BlockchainState

-- ============================================================================
-- B FUNCTION ATOMS (6 functions per specification)
-- ============================================================================

-- | B function atom 1: SHA256 hash calculation
-- Pure function for data integrity per Invariant 4
sha256Hash :: Text -> Text
sha256Hash input = 
    let bytes = TE.encodeUtf8 input
        digest = H.hash bytes :: Digest SHA256
    in T.pack $ show digest

-- | B function atom 2: Get current timestamp
-- IO function returning Unix timestamp per Invariant 6
getCurrentTimestamp :: IO Double
getCurrentTimestamp = do
    utc <- getCurrentTime
    return $ realToFrac $ utcTimeToPOSIXSeconds utc

-- | B function atom 3: Validate digital signature
-- Validates signature against public key (simplified for MVP)
validateSignature :: Text -> Text -> Text -> Bool
validateSignature _pubkey _message signature = 
    not (T.null signature) && T.isSuffixOf (T.pack "-sig") signature

-- | B function atom 4: Calculate consensus with progressive quorum
-- Implements Invariant 2 (Progressive Quorum) and Invariant 3 (Confidence Weights)
calculateConsensus :: [Attestation] -> Text -> Map Text Double
calculateConsensus attestations questionId' =
    let relevantAttestations = filter (\att -> questionId (attestationTransaction att) == questionId') attestations
        convergenceLevel = if null relevantAttestations 
                          then 0.0 
                          else convergence (attestationTransaction $ head relevantAttestations)
        quorumSize = progressiveQuorumSize convergenceLevel  -- Invariant 2
        validAttestations = take quorumSize relevantAttestations
        confidenceWeights = map (unConfidence . attestationConfidence) validAttestations
        totalWeight = sum confidenceWeights
    in if totalWeight > 0 && length validAttestations >= 3  -- Minimum quorum
       then Map.fromList [(T.pack "consensus", totalWeight / fromIntegral (length validAttestations)),
                         (T.pack "quorum_size", fromIntegral $ length validAttestations)]
       else Map.empty

-- | Helper function implementing Invariant 2: Progressive Quorum (3-5 based on convergence)
progressiveQuorumSize :: Double -> Int
progressiveQuorumSize conv
    | conv >= 0.8 = 3  -- High convergence, smaller quorum
    | conv >= 0.6 = 4  -- Medium convergence
    | otherwise = 5    -- Low convergence, larger quorum

-- | B function atom 5: Update distributions and calculate convergence
-- Implements Invariant 7 (Convergence Calculation)
updateDistributions :: [Transaction] -> Text -> (QuestionDistribution, MCQDistribution, FRQScores, Double)
updateDistributions transactions questionId' =
    let relevantTxs = filter (\tx -> questionId tx == questionId') transactions
        mcqTxs = filter (\tx -> case answerHash tx of Just _ -> True; Nothing -> False) relevantTxs
        frqTxs = filter (\tx -> case score tx of Just _ -> True; Nothing -> False) relevantTxs
        
        -- Build MCQ distribution
        mcqAnswers = [maybe T.empty id (answerHash tx) | tx <- mcqTxs]
        mcqDist = foldr (\answer acc -> Map.insertWith (+) answer 1 acc) Map.empty mcqAnswers
        
        -- Build FRQ scores list
        frqScoresList = [s | tx <- frqTxs, Just s <- [score tx]]
        
        -- Calculate convergence per Invariant 7
        conv = if not (null mcqAnswers)
               then fromIntegral (maximum $ Map.elems mcqDist) / fromIntegral (length mcqAnswers)  -- MCQ: max count ratio
               else if length frqScoresList >= 2
                    then 1.0 / coefficientOfVariation frqScoresList  -- FRQ: inverse CV
                    else 0.0
        
        questionDist = Map.fromList [(T.pack "mcq_count", length mcqTxs), (T.pack "frq_count", length frqTxs)]
    in (questionDist, mcqDist, frqScoresList, conv)

-- | Helper function to calculate coefficient of variation for FRQ convergence
coefficientOfVariation :: [FRQScore] -> Double
coefficientOfVariation scores = 
    let values = map unFRQScore scores
        mean' = sum values / fromIntegral (length values)
        variance = sum [(x - mean')^(2::Int) | x <- values] / fromIntegral (length values)
        stdDev = sqrt variance
    in if mean' /= 0 then stdDev / mean' else 1.0

-- | B function atom 6: Detect outliers using Z-score
-- Implements Invariant 11 (Outlier Detection)
detectOutliers :: [Double] -> [Double]
detectOutliers values
    | length values < 3 = []  -- Need at least 3 values for meaningful detection
    | otherwise = 
        let mean' = sum values / fromIntegral (length values)
            variance = sum [(x - mean')^(2::Int) | x <- values] / fromIntegral (length values)
            stdDev = sqrt variance
            zThreshold = 2.0  -- Standard Z-score threshold
        in [v | v <- values, stdDev > 0 && abs ((v - mean') / stdDev) > zThreshold]

-- ============================================================================
-- HELPER FUNCTIONS AND VALIDATION
-- ============================================================================

-- | Create a new transaction with proper validation
createTransaction :: TxType -> Text -> Text -> Text -> Maybe Text -> Maybe FRQScore -> 
                    Confidence -> Text -> IO Transaction
createTransaction txType' qid attesterKey sig answerTxt frqScore conf privKey = do
    currentTime <- getCurrentTimestamp
    let baseData = T.concat [qid, maybe T.empty id answerTxt, T.pack $ show currentTime]
        txHash = sha256Hash baseData
        answerHash' = case txType' of
            AttestationTx -> case answerTxt of
                Just txt -> Just $ sha256Hash txt
                Nothing -> Nothing
            CreateUserTx -> Nothing
        signature' = T.append privKey (T.pack "-sig")  -- Simplified signature
        
        -- Initialize empty distributions 
        emptyQuestionDist = Map.empty
        emptyMcqDist = Map.empty
        emptyFrqScores = []
        initialConvergence = 0.0
        
    return Transaction
        { hash = txHash
        , prevHash = T.empty  -- Will be set when adding to blockchain
        , timestamp = currentTime
        , nonce = 0
        , questionId = qid
        , answerHash = answerHash'
        , answerText = answerTxt
        , score = frqScore
        , attesterPubkey = attesterKey
        , signature = signature'
        , txType = txType'
        , isMatch = Nothing  -- Will be determined during consensus
        , questionDistribution = emptyQuestionDist
        , mcqDistribution = emptyMcqDist
        , frqScores = emptyFrqScores
        , convergence = initialConvergence
        , confidence = conf
        , anonymousSignature = Nothing
        , officialAnswer = Nothing
        }

-- | Create attestation from transaction
createAttestation :: Text -> Text -> Transaction -> IO Attestation
createAttestation attId validator tx = do
    currentTime <- getCurrentTimestamp
    return Attestation
        { attestationId = attId
        , attestationValidator = validator
        , attestationTransaction = tx
        , attestationTimestamp = currentTime
        , attestationConfidence = confidence tx
        }

-- | Validate transaction per multiple invariants
validateTransaction :: Transaction -> Either Text Transaction
validateTransaction tx
    | T.null (attesterPubkey tx) = Left (T.pack "Invalid attester public key: empty pubkey (Invariant 1)")
    | T.null (questionId tx) = Left (T.pack "Invalid question ID: empty questionId")
    | T.null (signature tx) = Left (T.pack "Invalid signature: empty signature")
    | not (validateSignature (attesterPubkey tx) (questionId tx) (signature tx)) = Left (T.pack "Invalid signature verification")
    | case score tx of Just s -> not (isValidFRQScore s); Nothing -> False = Left (T.pack "FRQ score out of bounds 1.0-5.0 (Invariant 5)")
    | not (isValidConfidence (confidence tx)) = Left (T.pack "Confidence out of bounds 0.0-1.0 (Invariant 3)")
    | timestamp tx <= 0 = Left (T.pack "Invalid timestamp: must be positive (Invariant 6)")
    | otherwise = Right tx

-- | Validate confidence bounds per Invariant 3
validateConfidence :: Double -> Either Text Confidence
validateConfidence conf
    | conf < 0.0 || conf > 1.0 = Left (T.pack "Confidence must be between 0.0 and 1.0 (Invariant 3)")
    | otherwise = Right (Confidence conf)

-- | Validate FRQ score bounds per Invariant 5  
validateFRQScore :: Double -> Either Text FRQScore
validateFRQScore s
    | s < 1.0 || s > 5.0 = Left (T.pack "FRQ score must be between 1.0 and 5.0 (Invariant 5)")
    | otherwise = Right (FRQScore s)

-- | Helper to check valid confidence
isValidConfidence :: Confidence -> Bool
isValidConfidence (Confidence c) = c >= 0.0 && c <= 1.0

-- | Helper to check valid FRQ score
isValidFRQScore :: FRQScore -> Bool
isValidFRQScore (FRQScore s) = s >= 1.0 && s <= 5.0