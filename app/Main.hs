{-|
Module      : Main
Description : CLI runner for the simulation
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This is the main executable entry point for the apstat-fundamental-hs CLI application.
It imports and runs the simulation from the Main module in src/.
-}

module Main where

import qualified Main as SimMain

-- | Main executable entry point
-- Delegates to the Main module in src/ for the actual simulation logic
main :: IO ()
main = SimMain.main
