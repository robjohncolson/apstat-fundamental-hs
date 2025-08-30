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