{-|
Module      : BlockchainSpec
Description : Unit tests for Blockchain module using HSpec and QuickCheck
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

Unit tests for the Blockchain module, testing B atoms functionality
including transactions, attestations, and consensus mechanisms.
-}

{-# LANGUAGE OverloadedStrings #-}

module BlockchainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Blockchain

-- | Main test specification for Blockchain module
spec :: Spec
spec = do
  describe "Blockchain" $ do
    describe "createTransaction" $ do
      it "creates a transaction with given parameters" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "alice" "bob" "transfer 100" currentTime
        transactionId tx `shouldBe` "tx-001"
        transactionFrom tx `shouldBe` "alice"
        transactionTo tx `shouldBe` "bob"
        transactionData tx `shouldBe` "transfer 100"
        transactionTimestamp tx `shouldBe` currentTime

    describe "createAttestation" $ do
      it "creates an attestation with given parameters" $ do
        currentTime <- getCurrentTime
        let att = createAttestation "att-001" "validator1" "subject1" 0.95 currentTime "proof123"
        attestationId att `shouldBe` "att-001"
        attestationValidator att `shouldBe` "validator1"
        attestationSubject att `shouldBe` "subject1"
        attestationValue att `shouldBe` 0.95
        attestationTimestamp att `shouldBe` currentTime
        attestationProof att `shouldBe` "proof123"

    describe "validateTransaction" $ do
      it "validates a correct transaction" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "alice" "bob" "transfer 100" currentTime
        validateTransaction tx `shouldBe` Right tx

      it "rejects transaction with empty sender" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "" "bob" "transfer 100" currentTime
        validateTransaction tx `shouldBe` Left "Transaction must have a sender"

      it "rejects transaction with empty recipient" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "alice" "" "transfer 100" currentTime
        validateTransaction tx `shouldBe` Left "Transaction must have a recipient"

      it "rejects transaction with empty data" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "alice" "bob" "" currentTime
        validateTransaction tx `shouldBe` Left "Transaction must have data"

    describe "calculateConsensus" $ do
      it "calculates average consensus value for matching subject" $ do
        currentTime <- getCurrentTime
        let att1 = createAttestation "att-001" "v1" "math" 0.8 currentTime "proof1"
        let att2 = createAttestation "att-002" "v2" "math" 0.9 currentTime "proof2"
        let att3 = createAttestation "att-003" "v3" "science" 0.7 currentTime "proof3"
        let attestations = [att1, att2, att3]
        
        calculateConsensus attestations "math" `shouldBe` Just 0.85
        calculateConsensus attestations "science" `shouldBe` Just 0.7
        calculateConsensus attestations "history" `shouldBe` Nothing

    describe "addTransactionToBlock" $ do
      it "adds transaction to block's transaction list" $ do
        currentTime <- getCurrentTime
        let tx = createTransaction "tx-001" "alice" "bob" "transfer 100" currentTime
        let block = Block 1 [] [] currentTime "prev-hash" "block-hash"
        let updatedBlock = addTransactionToBlock block tx
        
        blockTransactions updatedBlock `shouldBe` [tx]
        blockIndex updatedBlock `shouldBe` 1

-- | QuickCheck property tests
prop_consensusIsAverage :: [Double] -> Property
prop_consensusIsAverage values = 
  not (null values) ==> \currentTime ->
    let attestations = zipWith (\i v -> createAttestation (T.pack $ show i) "validator" "subject" v currentTime "proof") [1..] values
        result = calculateConsensus attestations "subject"
        expected = sum values / fromIntegral (length values)
    in result == Just expected

prop_transactionValidation :: Text -> Text -> Text -> Text -> Property
prop_transactionValidation txId from to txData = 
  not (T.null from) && not (T.null to) && not (T.null txData) ==> \currentTime ->
    let tx = createTransaction txId from to txData currentTime
    in case validateTransaction tx of
         Right _ -> True
         Left _ -> False
