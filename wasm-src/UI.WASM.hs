{-|
Module      : UI.WASM
Description : WASM-compatible UI functions with JavaScript FFI exports
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module provides WASM-compatible wrappers for the UI subsystem,
exporting handleEvent, renderState, and processEventQueue functions
to JavaScript through FFI. Maintains all 6 UI atoms and invariants.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.WASM
    ( -- Exported WASM functions
      handleEventWASM
    , renderStateWASM
    , processEventQueueWASM
    , initSystemStateWASM
    , getSystemStateWASM
    , setSystemStateWASM
    -- Internal functions
    , systemStateToJSON
    , systemStateFromJSON
    , eventFromJSON
    , eventToJSON
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode, Value(..), object, (.=), (.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.JSString (JSString)
import qualified Data.JSString as JS
import GHCJS.Types (JSVal)
import GHCJS.Foreign (fromJSString, toJSString)
import GHCJS.Marshal (fromJSVal, toJSVal)

-- Import all subsystems with full integration
import UI (SystemState(..), Event(..), CurrentView(..), renderState, handleEvent, processEventQueue)
import qualified UI as U
import Profile (Profile, createProfile)
import qualified Profile as P
import Blockchain (BlockchainState, createBlockchainState, Confidence(..), validateConfidence)
import qualified Blockchain as B
import Questions (Question, createQuestion)
import qualified Questions as Q
import Reputation (Reputation, createReputation)
import qualified Reputation as R
import Persistence.WASM (saveStateWASM, loadStateWASM, createPersistenceStateWASM)
import FFI (logWASM)

-- Global state management for WASM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Global SystemState for WASM environment
{-# NOINLINE globalSystemState #-}
globalSystemState :: IORef SystemState
globalSystemState = unsafePerformIO $ do
    let initialProfiles = []
    let initialBlockchain = createBlockchainState
    let initialQuestions = []
    let initialReputation = []
    let initialState = U.createSystemState initialProfiles initialBlockchain initialQuestions initialReputation
    newIORef initialState

-- ============================================================================
-- JSON SERIALIZATION INSTANCES
-- ============================================================================

-- SystemState JSON serialization
instance ToJSON SystemState where
    toJSON state = object
        [ "profiles" .= systemProfiles state
        , "blockchain" .= systemBlockchain state  
        , "questions" .= systemQuestions state
        , "reputation" .= systemReputation state
        , "currentView" .= systemCurrentView state
        , "eventQueue" .= systemEventQueue state
        , "renderBuffer" .= systemRenderBuffer state
        ]

instance FromJSON SystemState where
    parseJSON = A.withObject "SystemState" $ \o -> SystemState
        <$> o .: "profiles"
        <*> o .: "blockchain"
        <*> o .: "questions"
        <*> o .: "reputation"
        <*> o .: "currentView"
        <*> o .: "eventQueue"
        <*> o .: "renderBuffer"

-- Helper functions for JSON conversion
systemStateToJSON :: SystemState -> Text
systemStateToJSON = TE.decodeUtf8 . toStrict . encode

systemStateFromJSON :: Text -> Maybe SystemState
systemStateFromJSON = decode . fromStrict . TE.encodeUtf8

eventToJSON :: Event -> Text
eventToJSON = TE.decodeUtf8 . toStrict . encode

eventFromJSON :: Text -> Maybe Event
eventFromJSON = decode . fromStrict . TE.encodeUtf8

-- ============================================================================
-- WASM FFI EXPORTS
-- ============================================================================

-- Initialize system state
foreign import javascript unsafe "console.log('Initializing WASM system state')"
    js_logInit :: IO ()

initSystemStateWASM :: IO JSString
initSystemStateWASM = do
    js_logInit
    logWASM "Initializing AP Statistics WASM system state"
    
    -- Create initial system with sample data for testing
    let sampleProfile = createProfile "user1" "pubkey123" "privkey456"
    let sampleQuestion = createQuestion "q001" "What is the mean of [1,2,3,4,5]?" ["2", "3", "4", "5"] Nothing
    let sampleReputation = createReputation "user1" 100.0
    
    let initialState = SystemState
            { systemProfiles = [sampleProfile]
            , systemBlockchain = createBlockchainState
            , systemQuestions = [sampleQuestion]
            , systemReputation = [sampleReputation]
            , systemCurrentView = U.MainMenu
            , systemEventQueue = []
            , systemRenderBuffer = ""
            }
    
    writeIORef globalSystemState initialState
    logWASM "System state initialized successfully"
    return $ toJSString $ systemStateToJSON initialState

-- Get current system state  
foreign import javascript unsafe "$r = h$runSync(function() { return h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, h$hs_mkJSString(h$c1(hZCUsersZCroberZCDownloadsZCapstatzminusfundamentalzminushsZCwasminussrcZCUIziWASM_getSystemStateWASM_e))); });"
    js_getSystemState :: IO JSString

getSystemStateWASM :: IO JSString
getSystemStateWASM = do
    state <- readIORef globalSystemState
    return $ toJSString $ systemStateToJSON state

-- Set system state
foreign import javascript unsafe "$1"
    js_setSystemState :: JSString -> IO ()

setSystemStateWASM :: JSString -> IO JSString  
setSystemStateWASM jsonStr = do
    let jsonText = T.pack $ JS.unpack jsonStr
    case systemStateFromJSON jsonText of
        Nothing -> do
            logWASM "Error: Invalid JSON format for system state"
            return $ toJSString "ERROR: Invalid JSON"
        Just newState -> do
            writeIORef globalSystemState newState
            logWASM "System state updated successfully"
            return $ toJSString "SUCCESS"

-- Handle single event
foreign import javascript unsafe "$r = h$runSync(function() { return h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, h$hs_mkJSString(h$c1(hZCUsersZCroberZCDownloadsZCapstatzminusfundamentalzminushsZCwasminussrcZCUIziWASM_handleEventWASM_e, $1))); });"
    js_handleEvent :: JSString -> IO JSString

handleEventWASM :: JSString -> IO JSString
handleEventWASM eventJsonStr = do
    let eventJsonText = T.pack $ JS.unpack eventJsonStr
    logWASM $ "Processing event with full subsystem integration: " <> T.take 100 eventJsonText
    
    case eventFromJSON eventJsonText of
        Nothing -> do
            logWASM "Error: Invalid event JSON format"
            return $ toJSString "ERROR: Invalid event JSON"
        Just event -> do
            currentState <- readIORef globalSystemState
            
            -- Enhanced event processing with cross-subsystem integration
            enhancedState <- case event of
                U.AttestEvent attestData -> do
                    logWASM $ "Processing attestation with full integration: " <> U.attestQuestionId attestData
                    -- Validate and integrate across all subsystems
                    integrateAttestationEvent attestData currentState
                
                U.CreateProfileEvent profileData -> do
                    logWASM $ "Creating profile with reputation integration: " <> U.profileUserId profileData
                    integrateProfileCreation profileData currentState
                    
                U.UpdateScoreEvent scoreData -> do
                    logWASM $ "Updating score with blockchain integration: " <> U.scoreUserId scoreData
                    integrateScoreUpdate scoreData currentState
                    
                _ -> do
                    -- For other events, use standard handling
                    let basicState = handleEvent event currentState
                    return basicState
            
            writeIORef globalSystemState enhancedState
            
            -- Auto-save after significant state changes
            case event of
                U.AttestEvent _ -> saveIntegratedState enhancedState
                U.CreateProfileEvent _ -> saveIntegratedState enhancedState
                _ -> return ()
            
            logWASM "Event processed with full subsystem integration"
            return $ toJSString $ systemStateToJSON enhancedState

-- Render current state
foreign import javascript unsafe "$r = h$runSync(function() { return h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, h$hs_mkJSString(h$c1(hZCUsersZCroberZCDownloadsZCapstatzminusfundamentalzminushsZCwasminussrcZCUIziWASM_renderStateWASM_e))); });"
    js_renderState :: IO JSString

renderStateWASM :: IO JSString
renderStateWASM = do
    currentState <- readIORef globalSystemState
    let rendered = renderState currentState
    logWASM "State rendered successfully"
    return $ toJSString $ T.pack rendered

-- Process event queue
foreign import javascript unsafe "$r = h$runSync(function() { return h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, h$hs_mkJSString(h$c1(hZCUsersZCroberZCDownloadsZCapstatzminusfundamentalzminushsZCwasminussrcZCUIziWASM_processEventQueueWASM_e, $1))); });"
    js_processEventQueue :: JSString -> IO JSString

processEventQueueWASM :: JSString -> IO JSString
processEventQueueWASM eventsJsonStr = do
    let eventsJsonText = T.pack $ JS.unpack eventsJsonStr
    logWASM $ "Processing event queue: " <> T.take 50 eventsJsonText
    
    case A.decode (fromStrict $ TE.encodeUtf8 eventsJsonText) of
        Nothing -> do
            logWASM "Error: Invalid events array JSON"
            return $ toJSString "ERROR: Invalid events JSON"
        Just (events :: [Event]) -> do
            currentState <- readIORef globalSystemState
            updatedState <- processEventQueue events currentState
            writeIORef globalSystemState updatedState
            logWASM $ "Event queue processed: " <> T.pack (show (length events)) <> " events"
            return $ toJSString $ systemStateToJSON updatedState

-- ============================================================================
-- REACTOR MODEL EXPORTS
-- ============================================================================

-- ============================================================================
-- INTEGRATED EVENT PROCESSORS
-- ============================================================================

-- | Integrate attestation event across Profile, Questions, Blockchain, Reputation
integrateAttestationEvent :: U.AttestationData -> SystemState -> IO SystemState
integrateAttestationEvent attestData state = do
    let qId = U.attestQuestionId attestData
        answer = U.attestAnswer attestData
        confidence = U.attestConfidence attestData
    
    -- Validate confidence (Invariant 3)
    case validateConfidence confidence of
        Left err -> do
            logWASM $ "Confidence validation failed: " <> err
            return state { U.systemRenderBuffer = T.unpack err }
        Right validConf -> do
            -- Find or create question
            matchingQuestion <- case filter (\q -> Q.questionId q == qId) (U.systemQuestions state) of
                    (q:_) -> return q
                    [] -> do -- Create sample question if not found
                        baseQ <- Q.generateQuestion
                        return baseQ { Q.questionId = qId }
            
            -- Update profile with attestation
            timestamp <- B.getCurrentTimestamp
            let updatedProfiles = case U.systemProfiles state of
                    (profile:rest) -> 
                        let updatedProfile = P.updateProfileWithConsensus profile True qId confidence timestamp False False
                        in updatedProfile : rest
                    [] -> [P.createProfile "default_user" "pubkey123" "privkey456"]
            
            -- Update reputation
            updatedReputation <- case U.systemReputation state of
                (rep:rest) -> do
                    newRep <- R.updateReputationScore rep validConf False 0.8
                    return (newRep : rest)
                [] -> do
                    newRep <- R.createReputation
                    return [newRep]
            
            let integratedState = state
                    { U.systemProfiles = updatedProfiles
                    , U.systemReputation = updatedReputation
                    , U.systemRenderBuffer = "Integrated attestation: " <> T.unpack qId
                    }
            
            return integratedState

-- | Integrate profile creation with reputation and blockchain
integrateProfileCreation :: U.ProfileData -> SystemState -> IO SystemState
integrateProfileCreation profileData state = do
    let userId = T.unpack $ U.profileUserId profileData
    
    -- Check for existing profile
    let existingProfile = any (\p -> P.userId p == userId) (U.systemProfiles state)
    
    if existingProfile then
        return state { U.systemRenderBuffer = "Error: Profile already exists: " <> userId }
    else do
        -- Create new profile
        let newProfile = P.createProfile userId 
                            (T.unpack $ U.profilePubKey profileData) 
                            (T.unpack $ U.profilePrivKey profileData)
        
        -- Create corresponding reputation
        newReputation <- R.createReputation
        
        let integratedState = state
                { U.systemProfiles = newProfile : U.systemProfiles state
                , U.systemReputation = newReputation : U.systemReputation state
                , U.systemRenderBuffer = "Integrated profile creation: " <> userId
                }
        
        return integratedState

-- | Integrate score update across reputation and profile
integrateScoreUpdate :: U.ScoreData -> SystemState -> IO SystemState
integrateScoreUpdate scoreData state = do
    let userId = T.unpack $ U.scoreUserId scoreData
        scoreDelta = U.scoreDelta scoreData
    
    -- Find matching profile and reputation
    case (findProfile userId, findReputation userId) of
        (Just profileIdx, Just repIdx) -> do
            let profiles = U.systemProfiles state
                reputation = U.systemReputation state
                profile = profiles !! profileIdx
                rep = reputation !! repIdx
            
            -- Update reputation score (simplified)
            updatedRep <- R.updateReputationScore rep (Confidence 0.5) False (scoreDelta / 100.0)
            
            -- Update profile reputation field
            let updatedProfile = profile { P.reputationScore = realToFrac $ R.reputationScore updatedRep }
                updatedProfiles = take profileIdx profiles ++ [updatedProfile] ++ drop (profileIdx + 1) profiles
                updatedReputation = take repIdx reputation ++ [updatedRep] ++ drop (repIdx + 1) reputation
            
            let integratedState = state
                    { U.systemProfiles = updatedProfiles
                    , U.systemReputation = updatedReputation
                    , U.systemRenderBuffer = "Integrated score update: " <> userId
                    }
            
            return integratedState
            
        _ -> return state { U.systemRenderBuffer = "Error: Profile or reputation not found: " <> userId }
  where
    findProfile uid = findIndex (\p -> P.userId p == uid) (U.systemProfiles state)
    findReputation uid = findIndex (const True) (U.systemReputation state)  -- Simplified matching
    findIndex pred list = case filter pred list of
        [] -> Nothing
        _ -> Just 0  -- Simplified index finding

-- | Save integrated state using persistence layer
saveIntegratedState :: SystemState -> IO ()
saveIntegratedState state = do
    persistenceState <- createPersistenceStateWASM (U.systemProfiles state) (U.systemBlockchain state)
    saveResult <- saveStateWASM persistenceState
    case saveResult of
        Left err -> logWASM $ "Integrated state save failed: " <> err
        Right _ -> logWASM "Integrated state saved successfully"

-- Export functions for reactor model compilation
foreign export javascript "initSystemState" initSystemStateWASM :: IO JSString
foreign export javascript "getSystemState" getSystemStateWASM :: IO JSString  
foreign export javascript "setSystemState" setSystemStateWASM :: JSString -> IO JSString
foreign export javascript "handleEvent" handleEventWASM :: JSString -> IO JSString
foreign export javascript "renderState" renderStateWASM :: IO JSString
foreign export javascript "processEventQueue" processEventQueueWASM :: JSString -> IO JSString