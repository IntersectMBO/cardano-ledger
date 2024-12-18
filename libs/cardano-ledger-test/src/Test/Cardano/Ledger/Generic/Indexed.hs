{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tools for building unique Key (and related types) by just supplying an Int
--   Each Int returns a different Key (or related type)
--   Useful when writing unit tests
module Test.Cardano.Ledger.Generic.Indexed where

import Cardano.Crypto.DSIGN.Class (SignKeyDSIGN)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (DSIGN, VKey, WitVKey (..))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Proof (Proof (..))
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)

-- =======================================================
-- Keys and KeyHashes

-- | A signing key
newtype SKey (kr :: KeyRole) = SKey (SignKeyDSIGN DSIGN)

-- By changing the parameter 'n', we get a different keyPair
theKeyPair :: Int -> KeyPair kr
theKeyPair n = KeyPair a b
  where
    (b, a) = mkKeyPair (RawSeed 0 0 0 0 (fromIntegral n))

theVKey :: Int -> VKey kr
theVKey n = vKey (theKeyPair n)

theSKey :: forall kr. Int -> SKey kr
theSKey n = SKey (sKey (theKeyPair n))

theKeyHash :: Int -> KeyHash kr
theKeyHash n = hashKey (theVKey n)

theWitVKey ::
  Int ->
  SafeHash EraIndependentTxBody ->
  WitVKey 'Witness
theWitVKey n hash = mkWitnessVKey hash (theKeyPair n)

theKeyHashObj :: Int -> Credential kr
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj ::
  forall era kr. EraScript era => Proof era -> Script era -> Credential kr
aScriptHashObj _wit = ScriptHashObj . hashScript @era

theStakeReference :: Int -> StakeReference
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)
