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

-- Import all subsystems
import UI (SystemState(..), Event(..), CurrentView(..), renderState, handleEvent, processEventQueue)
import qualified UI as U
import Profile (Profile, createProfile)
import qualified Profile as P
import Blockchain (BlockchainState, createBlockchainState)
import qualified Blockchain as B
import Questions (Question, createQuestion)
import qualified Questions as Q
import Reputation (Reputation, createReputation)
import qualified Reputation as R
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
    logWASM $ "Processing event: " <> T.take 100 eventJsonText
    
    case eventFromJSON eventJsonText of
        Nothing -> do
            logWASM "Error: Invalid event JSON format"
            return $ toJSString "ERROR: Invalid event JSON"
        Just event -> do
            currentState <- readIORef globalSystemState
            let updatedState = handleEvent event currentState
            writeIORef globalSystemState updatedState
            logWASM "Event processed successfully"
            return $ toJSString $ systemStateToJSON updatedState

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

-- Export functions for reactor model compilation
foreign export javascript "initSystemState" initSystemStateWASM :: IO JSString
foreign export javascript "getSystemState" getSystemStateWASM :: IO JSString  
foreign export javascript "setSystemState" setSystemStateWASM :: JSString -> IO JSString
foreign export javascript "handleEvent" handleEventWASM :: JSString -> IO JSString
foreign export javascript "renderState" renderStateWASM :: IO JSString
foreign export javascript "processEventQueue" processEventQueueWASM :: JSString -> IO JSString