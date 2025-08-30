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