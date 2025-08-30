{-|
Module      : FFI
Description : JavaScript FFI definitions for WASM compilation
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines all JavaScript FFI operations required for WASM compilation,
replacing native IO operations while maintaining the 58-atom mathematical model.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module FFI
    ( -- Time operations
      js_getCurrentTimestamp
    , getCurrentTimestampWASM
    -- Storage operations  
    , js_readState
    , js_writeState
    , readStateWASM
    , writeStateWASM
    -- Enhanced storage operations for WASM persistence
    , js_getStorageItem
    , js_setStorageItem
    , js_indexedDBOpen
    , js_indexedDBGet
    , js_indexedDBPut
    , js_storageQuota
    , getStorageItemWASM
    , setStorageItemWASM
    , indexedDBGetWASM
    , indexedDBPutWASM
    , getStorageQuotaWASM
    -- Console operations
    , js_log
    , logWASM
    -- Crypto operations
    , js_sha256
    , sha256WASM
    -- Random operations
    , js_random
    , randomWASM
    -- DOM operations
    , js_getElementById
    , js_setInnerHTML
    , js_addEventListener
    , js_getValue
    , js_setValue
    , js_createElement
    , js_appendChild
    , js_removeClass
    , js_addClass
    , getElementByIdWASM
    , setInnerHTMLWASM
    , addEventListenerWASM
    , getValueWASM
    , setValueWASM
    , createElementWASM
    , appendChildWASM
    , removeClassWASM
    , addClassWASM
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.JSString (JSString)
import qualified Data.JSString as JS
import GHCJS.Types (JSVal, nullRef)
import GHCJS.Foreign (fromJSString, toJSString)
import GHCJS.Marshal (fromJSVal, toJSVal)

-- Time Operations FFI
foreign import javascript unsafe "Date.now()"
  js_getCurrentTimestamp :: IO Double

getCurrentTimestampWASM :: IO Double
getCurrentTimestampWASM = (/ 1000.0) <$> js_getCurrentTimestamp  -- Convert ms to seconds

-- Storage Operations FFI
foreign import javascript unsafe "localStorage.getItem($1)"
  js_readState :: JSString -> IO (JSVal)

foreign import javascript unsafe "localStorage.setItem($1, $2)"
  js_writeState :: JSString -> JSString -> IO ()

readStateWASM :: Text -> IO (Maybe Text)
readStateWASM key = do
    result <- js_readState (toJSString key)
    if result == nullRef
        then return Nothing
        else do
            jsStr <- fromJSVal result
            case jsStr of
                Nothing -> return Nothing
                Just s -> return $ Just $ T.pack $ JS.unpack s

writeStateWASM :: Text -> Text -> IO ()
writeStateWASM key value = 
    js_writeState (toJSString key) (toJSString value)

-- Enhanced Storage Operations FFI for WASM persistence
foreign import javascript unsafe "localStorage.getItem($1)"
  js_getStorageItem :: JSString -> IO JSVal

foreign import javascript unsafe "localStorage.setItem($1, $2)"
  js_setStorageItem :: JSString -> JSString -> IO ()

-- IndexedDB operations for large state storage
foreign import javascript unsafe "window.indexedDBOpen($1, $2)"
  js_indexedDBOpen :: JSString -> Int -> IO JSVal

foreign import javascript unsafe "window.indexedDBGet($1, $2)"
  js_indexedDBGet :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "window.indexedDBPut($1, $2, $3)"
  js_indexedDBPut :: JSString -> JSString -> JSString -> IO ()

-- Storage quota checking
foreign import javascript unsafe "navigator.storage && navigator.storage.estimate ? navigator.storage.estimate().then(function(estimate) { return { quota: estimate.quota, usage: estimate.usage }; }) : Promise.resolve({ quota: 5242880, usage: 0 })"
  js_storageQuota :: IO JSVal

-- WASM wrapper functions
getStorageItemWASM :: Text -> IO (Maybe Text)
getStorageItemWASM key = do
    result <- js_getStorageItem (toJSString key)
    if result == nullRef
        then return Nothing
        else do
            jsStr <- fromJSVal result
            case jsStr of
                Nothing -> return Nothing
                Just s -> return $ Just $ T.pack $ JS.unpack s

setStorageItemWASM :: Text -> Text -> IO ()
setStorageItemWASM key value = 
    js_setStorageItem (toJSString key) (toJSString value)

indexedDBGetWASM :: Text -> Text -> IO (Maybe Text)
indexedDBGetWASM dbName key = do
    result <- js_indexedDBGet (toJSString dbName) (toJSString key)
    if result == nullRef
        then return Nothing
        else do
            jsStr <- fromJSVal result
            case jsStr of
                Nothing -> return Nothing
                Just s -> return $ Just $ T.pack $ JS.unpack s

indexedDBPutWASM :: Text -> Text -> Text -> IO ()
indexedDBPutWASM dbName key value = 
    js_indexedDBPut (toJSString dbName) (toJSString key) (toJSString value)

getStorageQuotaWASM :: IO (Int, Int)  -- (quota, usage) in bytes
getStorageQuotaWASM = do
    result <- js_storageQuota
    -- Default to 5MB quota, 0 usage if estimation fails
    return (5242880, 0)

-- Console Operations FFI
foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

logWASM :: Text -> IO ()
logWASM msg = js_log (toJSString msg)

-- Crypto Operations FFI
foreign import javascript unsafe "window.crypto.subtle.digest('SHA-256', new TextEncoder().encode($1)).then(function(hash) { return Array.from(new Uint8Array(hash), function(b) { return ('00' + b.toString(16)).slice(-2) }).join(''); })"
  js_sha256 :: JSString -> IO JSString

sha256WASM :: Text -> IO Text
sha256WASM input = do
    result <- js_sha256 (toJSString input)
    return $ T.pack $ JS.unpack result

-- Random Operations FFI  
foreign import javascript unsafe "Math.random()"
  js_random :: IO Double

randomWASM :: IO Double
randomWASM = js_random

-- ============================================================================
-- DOM OPERATIONS FFI (for UI integration)
-- ============================================================================

-- DOM element access
foreign import javascript unsafe "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.innerHTML = $2"
  js_setInnerHTML :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript unsafe "$1.value"
  js_getValue :: JSVal -> IO JSString

foreign import javascript unsafe "$1.value = $2"
  js_setValue :: JSVal -> JSString -> IO ()

-- DOM manipulation
foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1.appendChild($2)"
  js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.classList.remove($2)"
  js_removeClass :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.classList.add($2)"
  js_addClass :: JSVal -> JSString -> IO ()

-- WASM wrapper functions for DOM operations
getElementByIdWASM :: Text -> IO (Maybe JSVal)
getElementByIdWASM elementId = do
    result <- js_getElementById (toJSString elementId)
    if result == nullRef
        then return Nothing
        else return $ Just result

setInnerHTMLWASM :: JSVal -> Text -> IO ()
setInnerHTMLWASM element content = 
    js_setInnerHTML element (toJSString content)

addEventListenerWASM :: JSVal -> Text -> JSVal -> IO ()
addEventListenerWASM element eventType callback =
    js_addEventListener element (toJSString eventType) callback

getValueWASM :: JSVal -> IO Text
getValueWASM element = do
    result <- js_getValue element
    return $ T.pack $ JS.unpack result

setValueWASM :: JSVal -> Text -> IO ()
setValueWASM element value =
    js_setValue element (toJSString value)

createElementWASM :: Text -> IO JSVal
createElementWASM tagName = 
    js_createElement (toJSString tagName)

appendChildWASM :: JSVal -> JSVal -> IO ()
appendChildWASM parent child = 
    js_appendChild parent child

removeClassWASM :: JSVal -> Text -> IO ()
removeClassWASM element className =
    js_removeClass element (toJSString className)

addClassWASM :: JSVal -> Text -> IO ()
addClassWASM element className =
    js_addClass element (toJSString className)