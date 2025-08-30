{-|
Module      : Blockchain.Pure
Description : Pure Blockchain operations for WASM compilation (25 atoms)
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module extracts all pure functions from the Blockchain subsystem,
maintaining the exact 25 B atoms while removing IO dependencies for WASM.
-}

{-# LANGUAGE DeriveGeneric #-}

module Blockchain.Pure
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
    -- Pure function atoms (6 B function atoms - IO removed)
    , sha256HashPure
    , validateSignaturePure
    , calculateConsensusPure
    , updateDistributionsPure
    , detectOutliersPure
    , progressiveQuorumPure
    -- Helper functions
    , createTransactionPure
    , createAttestationPure
    , validateTransactionPure
    , validateConfidencePure
    , validateFRQScorePure
    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, group)
import GHC.Generics (Generic)
import Crypto.Hash (SHA256(..), Digest)
import qualified Crypto.Hash as H

-- | Constrained confidence type enforcing Invariant 3 (1.0-5.0 bounds)
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
data TxType = AttestationTx | CreateUserTx | APRevealTx
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TxType
instance FromJSON TxType

-- | Type aliases for distribution data structures
type QuestionDistribution = Map Text Int       -- B atom 13
type MCQDistribution = Map Text Int           -- B atom 14  
type FRQScores = [FRQScore]                   -- B atom 15

-- | Transaction data structure with exactly 19 B data atoms
data Transaction = Transaction
    { hash :: Text                            -- B atom 1
    , prevHash :: Text                        -- B atom 2
    , timestamp :: Double                     -- B atom 3
    , nonce :: Int                            -- B atom 4
    , questionId :: Text                      -- B atom 5
    , answerHash :: Maybe Text                -- B atom 6
    , answerText :: Maybe Text                -- B atom 7
    , score :: Maybe FRQScore                 -- B atom 8
    , attesterPubkey :: Text                  -- B atom 9
    , signature :: Text                       -- B atom 10
    , txType :: TxType                        -- B atom 11
    , isMatch :: Maybe Bool                   -- B atom 12
    , questionDistribution :: QuestionDistribution  -- B atom 13
    , mcqDistribution :: MCQDistribution      -- B atom 14
    , frqScores :: FRQScores                  -- B atom 15
    , convergence :: Double                   -- B atom 16
    , confidence :: Confidence                -- B atom 17
    , anonymousSignature :: Maybe Text        -- B atom 18
    , officialAnswer :: Maybe Text            -- B atom 19
    } deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- | Attestation structure for social consensus (ADR-012)
data Attestation = Attestation
    { attestationId :: Text
    , validatorPubkey :: Text
    , targetTransaction :: Text
    , isValidAttestation :: Bool
    , attestationConfidence :: Confidence
    , attestationTimestamp :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON Attestation
instance FromJSON Attestation

-- | Blockchain state containing all transactions and attestations
data BlockchainState = BlockchainState
    { blockchainTransactions :: [Transaction]
    , blockchainAttestations :: [Attestation]
    , blockchainConsensusThreshold :: Double
    , blockchainCurrentHeight :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON BlockchainState
instance FromJSON BlockchainState

-- Pure Function Atoms (6 B atoms)

-- | B atom function 1: Pure SHA256 hash (no IO)
sha256HashPure :: Text -> Text
sha256HashPure input = 
    let bytes = TE.encodeUtf8 input
        digest = H.hash bytes :: Digest SHA256
    in T.pack $ show digest

-- | B atom function 3: Pure signature validation (simplified)
validateSignaturePure :: Text -> Text -> Text -> Bool
validateSignaturePure message signature pubkey =
    -- Simplified validation - in real WASM, use Web Crypto API
    T.length signature == 128 && T.length pubkey == 64

-- | B atom function 4: Pure consensus calculation per ADR-028
calculateConsensusPure :: [Attestation] -> Map Text Double
calculateConsensusPure attestations =
    let grouped = Map.fromListWith (+) [(validatorPubkey att, 1) | att <- attestations, isValidAttestation att]
        total = fromIntegral $ length attestations
    in Map.map (/ total) grouped

-- | B atom function 5: Pure distribution updates per ADR-028
updateDistributionsPure :: [Transaction] -> QuestionDistribution -> QuestionDistribution
updateDistributionsPure transactions currentDist =
    let newCounts = Map.fromListWith (+) [(questionId tx, 1) | tx <- transactions]
    in Map.unionWith (+) currentDist newCounts

-- | B atom function 6: Pure outlier detection per ADR-028
detectOutliersPure :: [Transaction] -> [Text]
detectOutliersPure transactions =
    let confValues = [unConfidence (confidence tx) | tx <- transactions]
        sorted = sort confValues
        len = length sorted
    in if len < 3
       then []
       else let q1 = sorted !! (len `div` 4)
                q3 = sorted !! (3 * len `div` 4)
                iqr = q3 - q1
                lowerBound = q1 - 1.5 * iqr
                upperBound = q3 + 1.5 * iqr
            in [hash tx | tx <- transactions, 
                let conf = unConfidence (confidence tx),
                conf < lowerBound || conf > upperBound]

-- | Progressive quorum calculation per ADR-028
progressiveQuorumPure :: Double -> Int
progressiveQuorumPure convergenceScore
    | convergenceScore >= 0.8 = 3  -- High convergence
    | convergenceScore >= 0.5 = 4  -- Medium convergence
    | otherwise = 5                -- Low convergence

-- Helper Functions

-- | Pure transaction creation (timestamp passed as parameter)
createTransactionPure :: TxType -> Text -> Text -> Text -> Maybe Text -> Maybe FRQScore -> 
                         Confidence -> Text -> Double -> Transaction
createTransactionPure txType' qid attesterKey sig answerTxt frqScore conf privKey currentTime =
    let baseData = T.concat [qid, maybe T.empty id answerTxt, T.pack $ show currentTime]
        txHash = sha256HashPure baseData
        defaultDist = Map.empty
    in Transaction
        { hash = txHash
        , prevHash = T.empty  -- Will be set by blockchain
        , timestamp = currentTime
        , nonce = 0
        , questionId = qid
        , answerHash = fmap sha256HashPure answerTxt  -- MCQ hash
        , answerText = answerTxt
        , score = frqScore
        , attesterPubkey = attesterKey
        , signature = sig
        , txType = txType'
        , isMatch = Nothing  -- Will be determined by consensus
        , questionDistribution = defaultDist
        , mcqDistribution = defaultDist
        , frqScores = []
        , convergence = 0.0
        , confidence = conf
        , anonymousSignature = Nothing
        , officialAnswer = Nothing
        }

-- | Pure attestation creation (timestamp passed as parameter)
createAttestationPure :: Text -> Text -> Text -> Bool -> Confidence -> Double -> Attestation
createAttestationPure attId validator tx isValid conf currentTime = 
    Attestation
        { attestationId = attId
        , validatorPubkey = validator
        , targetTransaction = tx
        , isValidAttestation = isValid
        , attestationConfidence = conf
        , attestationTimestamp = currentTime
        }

-- | Pure transaction validation
validateTransactionPure :: Transaction -> Bool
validateTransactionPure tx = 
    validateConfidencePure (confidence tx) &&
    maybe True validateFRQScorePure (score tx) &&
    T.length (attesterPubkey tx) == 64 &&
    T.length (hash tx) == 64

-- | Confidence validation per Invariant 3
validateConfidencePure :: Confidence -> Bool
validateConfidencePure (Confidence c) = c >= 1.0 && c <= 5.0

-- | FRQ score validation per Invariant 5  
validateFRQScorePure :: FRQScore -> Bool
validateFRQScorePure (FRQScore s) = s >= 1.0 && s <= 5.0