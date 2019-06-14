{-# LANGUAGE TypeApplications #-}

module Ledger.Update.Properties (upiregTracesAreClassified) where

import Hedgehog (Property, forAll, property, success)

import Control.State.Transition.Generator (trace, classifyTraceLength)

import Ledger.Update (UPIREG)

-- TODO: factor out duplication. Put this in Transition.Generator module!
upiregTracesAreClassified :: Property
upiregTracesAreClassified = property $ do
  let (tl, step) = (1000, 100)
  tr <- forAll (trace @UPIREG tl)
  classifyTraceLength tr tl step
  success
