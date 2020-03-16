{-# LANGUAGE DataKinds #-}

module Shelley.Spec.Ledger.Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , scriptsToAddr
  , scriptToCred
  , toAddr
  , toCred
  )
where

import           Cardano.Ledger.Shelley.Crypto
import           Shelley.Spec.Ledger.Keys (KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           Shelley.Spec.Ledger.Tx (hashScript)
import           Shelley.Spec.Ledger.TxData (Addr (..), Credential (..), RewardAcnt (..))
import           Shelley.Spec.Ledger.Scripts

mkVKeyRwdAcnt
  :: Crypto crypto
  => KeyPair 'Regular crypto
  -> RewardAcnt crypto
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt
  :: Credential crypto
  -> RewardAcnt crypto
mkRwdAcnt script@(ScriptHashObj _) = RewardAcnt script
mkRwdAcnt key@(KeyHashObj _) = RewardAcnt key

toAddr
  :: Crypto crypto
  => (KeyPair 'Regular crypto, KeyPair 'Regular crypto)
  -> Addr crypto
toAddr (payKey, stakeKey) = AddrBase (toCred payKey) (toCred stakeKey)

toCred
  :: Crypto crypto
  => KeyPair 'Regular crypto
  -> Credential crypto
toCred k = KeyHashObj . hashKey $ vKey k

-- | Convert a given multi-sig script to a credential by hashing it and wrapping
-- into the `Credential` data type.
scriptToCred :: Crypto crypto => MultiSig crypto -> Credential crypto
scriptToCred = ScriptHashObj . hashScript

-- | Create a base address from a pair of multi-sig scripts (pay and stake)
scriptsToAddr :: Crypto crypto => (MultiSig crypto, MultiSig crypto) -> Addr crypto
scriptsToAddr (payScript, stakeScript) =
  AddrBase (scriptToCred payScript) (scriptToCred stakeScript)
