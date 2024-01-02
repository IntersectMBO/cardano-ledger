{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tools for building unique Key (and related types) by just supplying an Int
--   Each Int returns a different Key (or related type)
--   Useful when writing unit tests
module Test.Cardano.Ledger.Generic.Indexed where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness), SignKeyDSIGN, VKey, WitVKey (..), hashKey)
import Cardano.Ledger.SafeHash (SafeHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Proof (
  GoodCrypto,
  Proof (..),
 )
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)

-- =======================================================
-- Keys and KeyHashes

-- | A signing key
newtype SKey (kr :: KeyRole) c = SKey (SignKeyDSIGN c)

-- By changing the parameter 'n', we get a different keyPair
theKeyPair :: Crypto c => Int -> KeyPair kr c
theKeyPair n = KeyPair a b
  where
    (b, a) = mkKeyPair (RawSeed 0 0 0 0 (fromIntegral n))

theVKey :: CC.Crypto c => Int -> VKey kr c
theVKey n = vKey (theKeyPair n)

theSKey :: forall c kr. CC.Crypto c => Int -> SKey kr c
theSKey n = SKey (sKey (theKeyPair @c n))

theKeyHash :: CC.Crypto c => Int -> KeyHash kr c
theKeyHash n = hashKey (theVKey n)

theWitVKey :: GoodCrypto c => Int -> SafeHash c EraIndependentTxBody -> WitVKey 'Witness c
theWitVKey n hash = mkWitnessVKey hash (theKeyPair n)

theKeyHashObj :: CC.Crypto c => Int -> Credential kr c
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj :: forall era kr. EraScript era => Proof era -> Script era -> Credential kr (EraCrypto era)
aScriptHashObj _wit s = ScriptHashObj . hashScript @era $ s

theStakeReference :: CC.Crypto c => Int -> StakeReference c
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)
