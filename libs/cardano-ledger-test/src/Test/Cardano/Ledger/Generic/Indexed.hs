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

-- | Tools for building unique Key (and other Core types) by just supplying an Int
--   Each Int returns a different Key (or related type)
--   Useful when writing unit tests, when you need a number of distinct, keys, hashes, credentials, etc.
module Test.Cardano.Ledger.Generic.Indexed where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole(..), SignKeyDSIGN, VKey, hashKey)
import Cardano.Ledger.SafeHash (SafeHash,unsafeMakeSafeHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkKeyPairWithRawSeed)
import Test.Cardano.Ledger.Core.Arbitrary(RawSeed (..),)
import Data.Binary(encode)
import Data.ByteString.Short(toShort)
import qualified Data.ByteString.Lazy as Lazy(toStrict,replicate,reverse)
import Cardano.Ledger.Crypto(StandardCrypto)
import Cardano.Ledger.Address(Addr(..))
import Cardano.Ledger.DRep (DRep(DRepCredential))
import Cardano.Ledger.Coin(Coin(..))
import Cardano.Crypto.Hash(Hash(UnsafeHash),Blake2b_256)
import Cardano.Ledger.TxIn(TxIn(..),TxId(..),mkTxInPartial)
import Cardano.Ledger.BaseTypes(Network(..))
import Cardano.Ledger.Val(Val(..))

-- =======================================================
-- Keys and KeyHashes

-- | A signing key
newtype SKey (kr :: KeyRole) c = SKey (SignKeyDSIGN c)

-- By changing the parameter 'n', we get a different keyPair
theKeyPair :: Crypto c => Int -> KeyPair kr c
theKeyPair n = KeyPair a b
  where
    (b, a) = mkKeyPairWithRawSeed (RawSeed 0 0 0 0 (fromIntegral n))

theVKey :: CC.Crypto c => Int -> VKey kr c
theVKey n = vKey (theKeyPair n)

theSKey :: forall c kr. CC.Crypto c => Int -> SKey kr c
theSKey n = SKey (sKey (theKeyPair @c n))

theKeyHash :: CC.Crypto c => Int -> KeyHash kr c
theKeyHash n = hashKey (theVKey n)

theKeyHashObj :: CC.Crypto c => Int -> Credential kr c
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj ::
  forall era kr. EraScript era => Script era -> Credential kr (EraCrypto era)
aScriptHashObj s = ScriptHashObj . hashScript @era $ s


theStakeReference :: CC.Crypto c => Int -> StakeReference c
theStakeReference 0 = StakeRefNull -- A 0 means no StakeReference
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)

-- =============================================================

stakeCred:: Int -> Credential 'Staking StandardCrypto
stakeCred n = theKeyHashObj n

poolHash :: Int -> KeyHash 'StakePool StandardCrypto
poolHash n = hashKey . vKey $ theKeyPair n

drep :: Int -> DRep StandardCrypto
drep n = DRepCredential (theKeyHashObj n)

addr :: Int -> Int -> Addr StandardCrypto
addr i j = Addr Testnet (theKeyHashObj i) (theStakeReference j)

txOut :: EraTxOut era => Addr (EraCrypto era) -> Coin -> TxOut era
txOut add c = mkBasicTxOut add (modifyCoin (const c) zero)

safeHash :: Int -> SafeHash StandardCrypto a
safeHash n = unsafeMakeSafeHash $ UnsafeHash $ toShort $ Lazy.toStrict (Lazy.reverse(encode @Int (maxBound - n)) <> Lazy.replicate 248 35)

hash :: Int -> Hash Blake2b_256 a
hash n = UnsafeHash $ toShort $ Lazy.toStrict (Lazy.reverse(encode @Int (maxBound - n)) <> Lazy.replicate 248 35)

txIn :: Int -> Int -> TxIn StandardCrypto
txIn i j = mkTxInPartial (TxId (safeHash i)) (fromIntegral j)
