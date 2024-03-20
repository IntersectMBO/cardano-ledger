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
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), SignKeyDSIGN, VKey, WitVKey (..), hashKey)
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Proof (GoodCrypto, Proof(..))
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Cardano.Ledger.DRep (DRep(DRepCredential))
import Cardano.Ledger.Crypto(StandardCrypto)
import Cardano.Ledger.UTxO(UTxO(..))
import Cardano.Ledger.TxIn(TxIn(..),TxId(..),mkTxInPartial)
import Cardano.Ledger.Address(Addr(..),RewardAccount(..))
import Cardano.Ledger.Credential(Credential(..),PaymentCredential)
import Cardano.Ledger.BaseTypes(Network(..),StrictMaybe(..),boundRational)
import Cardano.Ledger.Mary(MaryValue(..))
import Cardano.Ledger.Coin(Coin(..),CompactForm(CompactCoin))
import Cardano.Ledger.Compactible(CompactForm(..))

import Cardano.Ledger.Val(Val(..))
import qualified Cardano.Ledger.Conway as X(Conway)
import Cardano.Crypto.Hash(Hash(UnsafeHash),Blake2b_256)
import Cardano.Ledger.Crypto(HASH(..))
import Data.Binary(encode)
import Data.ByteString.Short(ShortByteString,toShort)
import qualified Data.ByteString.Lazy as Lazy(length,toStrict,replicate,reverse)
import Cardano.Crypto.Hash.Class(HashAlgorithm(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Word(Word64)
import Data.Ratio((%))

import Test.Cardano.Ledger.Generic.PrettyCore(pcUTxO,pcDState,pcPState,ppMap,pcCredential,pcCoin)
import Cardano.Ledger.UMap(UMap(..),RDPair(..),CompactForm,unify)
import Cardano.Ledger.Shelley.LedgerState(DState(..),PState(..))
import Cardano.Ledger.PoolParams(PoolParams(..))
import Test.Cardano.Ledger.Generic.ModelState(dStateZero,pStateZero)
import Data.Maybe(fromJust)
import Cardano.Ledger.Core(emptyPParams,  ppProtocolVersionL)
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Generic.Functions(protocolVersion)
import Cardano.Ledger.Shelley.LedgerState
  (incrementalStakeDistr,IncrementalStake(..),updateStakeDistribution)
import Cardano.Ledger.EpochBoundary (SnapShot (..),Stake(..))
import qualified Data.VMap as VMap

--  ppMap pcCredential (pcCoin . fromCompact) (VMap.toMap (unStake (ssStake ss))))

go = ppMap pcCredential (pcCoin . fromCompact) (VMap.toMap (unStake (ssStake snapshot)))
  where incstake = (updateStakeDistribution pparams mempty mempty utxo)
        snapshot = incrementalStakeDistr pparams incstake dstate pstate
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

theWitVKey ::
  GoodCrypto c =>
  Int ->
  SafeHash c EraIndependentTxBody ->
  WitVKey 'Witness c
theWitVKey n hash = mkWitnessVKey hash (theKeyPair n)

theKeyHashObj :: CC.Crypto c => Int -> Credential kr c
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj ::
  forall era kr. EraScript era => Proof era -> Script era -> Credential kr (EraCrypto era)
aScriptHashObj _wit s = ScriptHashObj . hashScript @era $ s

theStakeReference :: CC.Crypto c => Int -> StakeReference c
theStakeReference 0 = StakeRefNull
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)

-- =============================================================

-- inject :: forall era. Coin -> Value era
-- inject c = modifyCoin @(Value era) (const c) zero

stakeCred:: Int -> Credential 'Staking StandardCrypto
stakeCred n = theKeyHashObj n

poolHash :: Int -> KeyHash 'StakePool StandardCrypto
poolHash n = hashKey . vKey $ theKeyPair n

drep :: Int -> DRep StandardCrypto
drep n = DRepCredential (theKeyHashObj n)

addr :: Int -> Int -> Addr StandardCrypto
addr i j = Addr Testnet (theKeyHashObj i) (theStakeReference j)

txOut :: Addr StandardCrypto -> Coin -> TxOut X.Conway
txOut add c = mkBasicTxOut add (modifyCoin (const c) zero)


hash :: Int -> SafeHash StandardCrypto a
hash n = unsafeMakeSafeHash $ UnsafeHash $ toShort $ Lazy.toStrict (Lazy.reverse(encode @Int n) <> Lazy.replicate 248 35)

txIn :: Int -> Int -> TxIn StandardCrypto
txIn i j = mkTxInPartial (TxId (hash i)) (fromIntegral j)

addr1 = addr 3 4
addr2 = addr 5 0 -- 0 means No staking credential
addr3 = addr 6 0 -- 0 means No staking credential

-- =====================================

-- There are 2 Pools
pool1 = poolHash 30
pool2 = poolHash 31

-- There are 5 wallets, each with its own Credential
tom = stakeCred 1
ann = stakeCred 2
ron = stakeCred 3
john = stakeCred 4
mary = stakeCred 5

-- They each own an address
tomAddr = addr 1 0 -- 0 means tomAddr  does not have a StakeReference
annAddr = addr 2 20
ronAddr = addr 3 21
johnAddr = addr 4 22
maryAddr = addr 5 0 -- 0 means maryAddr  does not have a StakeReference

-- Each wallet has registered its credential,
rewards :: Map (Credential 'Staking StandardCrypto) RDPair
rewards = Map.fromList  -- (rdpair reward deposit) 
    [ (tom,rdpair 5 6)  -- only rewards should be distributed
    , (ann,rdpair 7 6)  -- sowe should see 53 distributed
    , (ron,rdpair 11 6)
    , (john,rdpair 13 6)
    , (mary,rdpair 17 6) ]
    
rdpair :: Word64 -> Word64 -> RDPair
rdpair x y = RDPair (CompactCoin x) (CompactCoin y)

-- Each wallet delegates to one of the pools
delegations :: Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
delegations = Map.fromList
    [ (tom,pool1)   -- since every one is delegated 
    , (ann,pool2)   -- no ones stake should be left out
    , (ron,pool1)
    , (john,pool2)
    , (mary,pool2) ]


-- Each wallet has one or more UTxO entries
-- Since tom and mary use a StakeRefNull those entries will not be distributed
utxo = UTxO(Map.fromList
   [(txIn 40 1,txOut tomAddr (Coin 23))  -- Not distrubuted 23
   ,(txIn 40 2,txOut tomAddr (Coin 29))
   ,(txIn 42 4,txOut annAddr (Coin 31))
   ,(txIn 43 1,txOut ronAddr (Coin 33))
   ,(txIn 44 2,txOut johnAddr (Coin 41))
   ,(txIn 45 1,txOut maryAddr (Coin 43)) -- Not distributed 43
   ])                 --total 200 - 66   134 should be distributed     

umap :: UMap StandardCrypto
umap = unify rewards Map.empty delegations Map.empty

dstate :: DState X.Conway
dstate = dStateZero{dsUnified = umap}

pstate :: PState X.Conway
pstate = pStateZero{psStakePoolParams =
   Map.fromList [(pool1,poolParams pool1 30),(pool2,poolParams pool2 31)]}

poolParams x n =
  PoolParams x
    (UnsafeHash $ toShort $ Lazy.toStrict (Lazy.reverse(encode @Int 77) <> Lazy.replicate 248 35))
    (Coin 1)
    (Coin 3)
    (fromJust(boundRational (1 % 2)))
    (RewardAccount Testnet (stakeCred n))
    mempty
    mempty
    SNothing

pparams :: PParams X.Conway
pparams = emptyPParams & ppProtocolVersionL .~ (protocolVersion Conway)

{-
ghci> pcDState dstate
DState
 { rewards =
     Map
      { (Key #"0fad15e7) -> ₳ 11
      , (Key #"24083eec) -> ₳ 13
      , (Key #"5d43e1f1) -> ₳ 7
      , (Key #"c53d0ffe) -> ₳ 17
      , (Key #"c6852b6a) -> ₳ 5 }
 , deposits =
     Map
      { (Key #"0fad15e7) -> ₳ 6
      , (Key #"24083eec) -> ₳ 6
      , (Key #"5d43e1f1) -> ₳ 6
      , (Key #"c53d0ffe) -> ₳ 6
      , (Key #"c6852b6a) -> ₳ 6 }
 , delegate =
     Map
      { (Key #"0fad15e7) -> #"68affb7b
      , (Key #"24083eec) -> #"cceb9603
      , (Key #"5d43e1f1) -> #"cceb9603
      , (Key #"c53d0ffe) -> #"cceb9603
      , (Key #"c6852b6a) -> #"68affb7b }
 , drepDeleg = Map{  }
 , ptrs = Map{  }
 , fGenDel = Map{  }
 , GenDel = Map{  }
 , iRewards =
     IReward
      { reserves = Map{  }
      , treasury = Map{  }
      , deltaR = ▵₳ 0
      , deltaT = ▵₳ 0 } }
ghci> putStrLn (show(pcUTxO Conway utxo))
Map
 { (TxIn #"28000000 1) ->
     (TxOut (Addr TestNet (Key #"c6852b6a) Null) (Value ₳ 23 {}) NoDatum ?-)
 , (TxIn #"28000000 2) ->
     (TxOut (Addr TestNet (Key #"c6852b6a) Null) (Value ₳ 29 {}) NoDatum ?-)
 , (TxIn #"2a000000 4) ->
     (TxOut
        (Addr TestNet (Key #"5d43e1f1) (Key #"bbf1c736))
        (Value ₳ 31 {})
        NoDatum
        ?-)
 , (TxIn #"2b000000 1) ->
     (TxOut
        (Addr TestNet (Key #"0fad15e7) (Key #"0befa008))
        (Value ₳ 33 {})
        NoDatum
        ?-)
 , (TxIn #"2c000000 2) ->
     (TxOut
        (Addr TestNet (Key #"24083eec) (Key #"2f55a332))
        (Value ₳ 41 {})
        NoDatum
        ?-)
 , (TxIn #"2d000000 1) ->
     (TxOut (Addr TestNet (Key #"c53d0ffe) Null) (Value ₳ 43 {}) NoDatum ?-) }

ghci> pcPState pstate
PState
 { regPools =
     Map
      { #"68affb7b ->
          PoolParams{ Id = #"68affb7b, reward accnt = (Key #"68affb7b) }
      , #"cceb9603 ->
          PoolParams{ Id = #"cceb9603, reward accnt = (Key #"cceb9603) } }
 , futureRegPools = Map{  }
 , retiring = Map{  }
 , poolDeposits = Map{  } }

ghci> go
Map
 { (Key #"0fad15e7) -> ₳ 11
 , (Key #"24083eec) -> ₳ 13
 , (Key #"5d43e1f1) -> ₳ 7
 , (Key #"c53d0ffe) -> ₳ 17
 , (Key #"c6852b6a) -> ₳ 5 }

-}