{-|
Module      : Spec
Description : Main test runner using HSpec
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This is the main test runner that discovers and runs all tests using HSpec.
It automatically discovers test modules and runs them.
-}

{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- This file is automatically processed by hspec-discover
-- It will find all test modules matching the pattern *Spec.hs
-- and run them as part of the test suite
