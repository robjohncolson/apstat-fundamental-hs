{-|
Module      : Persistence
Description : P2 atoms - State persistence with JSON serialization
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module handles persistence operations for saving and loading application state.
P2 atoms represent persistence operations for the AP Statistics fundamental concepts system,
including JSON serialization and file I/O operations.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Persistence
    ( AppState(..)
    , saveState
    , loadState
    , saveStateToFile
    , loadStateFromFile
    , encodeState
    , decodeState
    ) where

import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import GHC.Generics (Generic)
import Profile (Profile)
import Blockchain (BlockchainState)

-- | Application state that can be persisted
data AppState = AppState
    { appStateProfiles :: [Profile]
    , appStateBlockchain :: BlockchainState
    , appStateVersion :: Text
    , appStateMetadata :: Text
    } deriving (Show, Eq, Generic)

-- Automatically derive JSON instances
instance ToJSON AppState
instance FromJSON AppState

-- | Save application state to a JSON ByteString
saveState :: AppState -> ByteString
saveState = encode

-- | Load application state from a JSON ByteString
loadState :: ByteString -> Either String AppState
loadState = eitherDecode

-- | Save application state to a file
saveStateToFile :: FilePath -> AppState -> IO (Either String ())
saveStateToFile filepath appState = do
    let jsonData = saveState appState
    result <- try $ L.writeFile filepath jsonData
    case result of
        Left ex -> return $ Left $ show ex
        Right _ -> return $ Right ()
  where
    try :: IO a -> IO (Either IOError a)
    try action = do
        result <- tryIO action
        return result
    
    tryIO :: IO a -> IO (Either IOError a)
    tryIO action = (Right <$> action) `catch` (return . Left)
    
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch = flip $ \handler action -> do
        result <- action
        return result  -- Simplified for template

-- | Load application state from a file
loadStateFromFile :: FilePath -> IO (Either String AppState)
loadStateFromFile filepath = do
    result <- try $ L.readFile filepath
    case result of
        Left ex -> return $ Left $ show ex
        Right jsonData -> return $ loadState jsonData
  where
    try :: IO a -> IO (Either IOError a)
    try action = do
        result <- tryIO action
        return result
    
    tryIO :: IO a -> IO (Either IOError a)
    tryIO action = (Right <$> action) `catch` (return . Left)
    
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch = flip $ \handler action -> do
        result <- action
        return result  -- Simplified for template

-- | Encode state to JSON (alias for saveState)
encodeState :: AppState -> ByteString
encodeState = saveState

-- | Decode state from JSON (alias for loadState)
decodeState :: ByteString -> Either String AppState
decodeState = loadState
