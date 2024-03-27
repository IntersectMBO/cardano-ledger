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
module Test.Cardano.Ledger.UnitTestTools (
  -- * Operations
  theKeyPair,
  stakeReference,
  keyHash,
  keyWitFor,
  keyWit,
  credential,
  stakeCred,
  rewardAccount,
  poolHash,
  drep,
  addr,
  txOut,
  safeHash,
  hash,
  txIn,
  ccoin,
  individualPoolStake,
  poolParams,
  aScriptHashObj,

  -- * Types supported
  Hash,
  Addr,
  RewardAccount,
  Coin,
  CompactForm (CompactCoin),
  Credential,
  StakeReference,
  StandardCrypto,
  DRep,
  KeyHash,
  KeyRole (..),
  SafeHash,
  TxId,
  TxIn,
  IndividualPoolStake (IndividualPoolStake),
  WitVKey,
  RDPair (..),
  Word64,
  PoolParams,
) where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Crypto.Hash (Blake2b_256, Hash, castHash)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..), boundRational)
import Cardano.Ledger.Binary (hashEncCBOR, shelleyProtVer)
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.DRep (DRep (DRepCredential))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), WitVKey (..), hashKey)
import Cardano.Ledger.PoolDistr (IndividualPoolStake (IndividualPoolStake))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.UMap (RDPair (..))
import Cardano.Ledger.Val (Val (..))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.Arbitrary (RawSeed (..))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkKeyPairWithRawSeed, mkWitnessVKey)

-- =======================================================

-- By changing the parameter 'n', we get a different keyPair
theKeyPair :: Crypto c => Int -> KeyPair kr c
theKeyPair n = KeyPair a b
  where
    (b, a) = mkKeyPairWithRawSeed (RawSeed 0 0 0 0 (fromIntegral n))

-- =============================================================

-- | Creates a StakeReference for use in an Addr.
--   Note that 0 has special meaning, returning StakeRefNull,
--   Meaning this Addr does not stake it Coin to anything.
stakeReference :: CC.Crypto c => Int -> StakeReference c
stakeReference 0 = StakeRefNull -- A 0 means no StakeReference
stakeReference n = (StakeRefBase . KeyHashObj . hashKey) (vKey (theKeyPair n))

keyHash :: CC.Crypto c => Int -> KeyHash kr c
keyHash n = hashKey (vKey (theKeyPair n))

-- | (keyWitFor txbodyhash n) is the Witness for (keyHash n) in the TxBody with hash 'txbodyhash'
keyWitFor :: SafeHash StandardCrypto EraIndependentTxBody -> Int -> WitVKey 'Witness StandardCrypto
keyWitFor txbodyHash n = mkWitnessVKey txbodyHash (theKeyPair n)

-- | (keyWit txb n) is the Witness for (keyHash n) in the TxBody with hash (safeHash txb)
--   Use this when the actual TxBody is abstract, and we just need to pick one.
keyWit :: Int -> Int -> WitVKey 'Witness StandardCrypto
keyWit txb n = keyWitFor (safeHash txb) n

credential :: CC.Crypto c => Int -> Credential kr c
credential n = KeyHashObj . hashKey . vKey $ theKeyPair n

stakeCred :: Int -> Credential 'Staking StandardCrypto
stakeCred n = credential n

rewardAccount :: Int -> RewardAccount StandardCrypto
rewardAccount n = RewardAccount Testnet (stakeCred n)

poolHash :: Int -> KeyHash 'StakePool StandardCrypto
poolHash n = hashKey . vKey $ theKeyPair n

drep :: Int -> DRep StandardCrypto
drep n = DRepCredential (credential n)

addr :: CC.Crypto c => Int -> Int -> Addr c
addr i j = Addr Testnet (credential i) (stakeReference j)

txOut :: EraTxOut era => Addr (EraCrypto era) -> Coin -> TxOut era
txOut add c = mkBasicTxOut add (modifyCoin (const c) zero)

safeHash :: Int -> SafeHash StandardCrypto a
safeHash n = unsafeMakeSafeHash $ hash n

hash :: Int -> Hash Blake2b_256 a
hash n = castHash $ hashEncCBOR shelleyProtVer n

txIn :: Int -> Int -> TxIn StandardCrypto
txIn i j = mkTxInPartial (TxId (safeHash i)) (fromIntegral j)

ccoin :: Int -> CompactForm Coin
ccoin n = CompactCoin (fromIntegral n)

-- | Please NOTE that 'n' denotes the Hash of the VRF key,
--   NOT the poolHash (KeyHash 'StakePool StandardCrypto)
--   They are two completely different things and use different HashAlgorithms
individualPoolStake :: Rational -> Int -> IndividualPoolStake StandardCrypto
individualPoolStake r n = IndividualPoolStake r (hash n)

-- | Create a Script Credential for an actual Script.
--   I don't know of any good way to create a unique Script from an Int
aScriptHashObj :: forall era kr. EraScript era => Script era -> Credential kr (EraCrypto era)
aScriptHashObj s = ScriptHashObj . hashScript @era $ s

poolParams :: KeyHash 'StakePool StandardCrypto -> Int -> PoolParams StandardCrypto
poolParams poolhash rewAcctNum =
  PoolParams
    poolhash
    (hash 77)
    (Coin 1)
    (Coin 3)
    (fromJust (boundRational (1 % 2)))
    (rewardAccount rewAcctNum)
    mempty
    mempty
    SNothing
