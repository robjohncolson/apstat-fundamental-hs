{-|
Module      : Blockchain
Description : B atoms - Blockchain transactions, attestations, and consensus
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines blockchain-related data types and operations.
B atoms represent blockchain transactions, attestations, and consensus mechanisms
in the AP Statistics fundamental concepts system.
-}

module Blockchain
    ( Transaction(..)
    , Attestation(..)
    , Block(..)
    , BlockchainState(..)
    , TransactionId
    , AttestationId
    , calculateConsensus
    , createTransaction
    , createAttestation
    , validateTransaction
    , addTransactionToBlock
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Unique identifier for transactions
type TransactionId = Text

-- | Unique identifier for attestations
type AttestationId = Text

-- | Transaction data structure
data Transaction = Transaction
    { transactionId :: TransactionId
    , transactionFrom :: Text
    , transactionTo :: Text
    , transactionData :: Text
    , transactionTimestamp :: UTCTime
    } deriving (Show, Eq)

-- | Attestation data structure for consensus
data Attestation = Attestation
    { attestationId :: AttestationId
    , attestationValidator :: Text
    , attestationSubject :: Text
    , attestationValue :: Double
    , attestationTimestamp :: UTCTime
    , attestationProof :: Text
    } deriving (Show, Eq)

-- | Block containing transactions and attestations
data Block = Block
    { blockIndex :: Int
    , blockTransactions :: [Transaction]
    , blockAttestations :: [Attestation]
    , blockTimestamp :: UTCTime
    , blockPreviousHash :: Text
    , blockHash :: Text
    } deriving (Show, Eq)

-- | Overall blockchain state
data BlockchainState = BlockchainState
    { blockchainBlocks :: [Block]
    , blockchainTransactionPool :: [Transaction]
    , blockchainAttestationPool :: [Attestation]
    , blockchainConsensusThreshold :: Double
    } deriving (Show, Eq)

-- | Create a new transaction
createTransaction :: TransactionId -> Text -> Text -> Text -> UTCTime -> Transaction
createTransaction txId from to txData timestamp = Transaction
    { transactionId = txId
    , transactionFrom = from
    , transactionTo = to
    , transactionData = txData
    , transactionTimestamp = timestamp
    }

-- | Create a new attestation
createAttestation :: AttestationId -> Text -> Text -> Double -> UTCTime -> Text -> Attestation
createAttestation attId validator subject value timestamp proof = Attestation
    { attestationId = attId
    , attestationValidator = validator
    , attestationSubject = subject
    , attestationValue = value
    , attestationTimestamp = timestamp
    , attestationProof = proof
    }

-- | Validate a transaction
validateTransaction :: Transaction -> Either Text Transaction
validateTransaction tx
    | transactionFrom tx == "" = Left "Transaction must have a sender"
    | transactionTo tx == "" = Left "Transaction must have a recipient"
    | transactionData tx == "" = Left "Transaction must have data"
    | otherwise = Right tx

-- | Add a transaction to a block
addTransactionToBlock :: Block -> Transaction -> Block
addTransactionToBlock block tx = block
    { blockTransactions = tx : blockTransactions block
    }

-- | Calculate consensus based on attestations
calculateConsensus :: [Attestation] -> Text -> Maybe Double
calculateConsensus attestations subject =
    let subjectAttestations = filter (\att -> attestationSubject att == subject) attestations
        values = map attestationValue subjectAttestations
    in if null values
       then Nothing
       else Just (sum values / fromIntegral (length values))
