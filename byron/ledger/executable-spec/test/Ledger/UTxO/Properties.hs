{-# LANGUAGE TypeApplications #-}

module Ledger.UTxO.Properties where

import Hedgehog
  ( MonadTest
  , Property
  , (===)
  , assert
  , forAll
  , property
  , withTests
  )

import Control.State.Transition.Generator
  ( HasSizeInfo
  , HasTrace
  , initEnvGen
  , isTrivial
  , nonTrivialTrace
  , sigGen
  , suchThatLastState
  , trace
  )
import Control.State.Transition.Trace (firstAndLastState)
import Ledger.UTxO (UTXOW, reserves, balance, utxo, TxId)

-- | Check that the money is constant in the system.
moneyIsConstant :: Property
moneyIsConstant = property $ do
  (st0, st) <- firstAndLastState <$> forAll (trace @(UTXOW TxId) 100)
  reserves st0 + balance (utxo st0) === reserves st + balance (utxo st)
