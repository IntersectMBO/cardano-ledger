{-# LANGUAGE DataKinds #-}

module Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , scriptsToAddr
  , scriptToCred
  , toAddr
  , toCred
  )
where

import           Cardano.Ledger.Shelley.Crypto
import           Keys (KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           Tx (hashScript)
import           TxData (Addr (..), Credential (..), RewardAcnt (..))
import           Scripts

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
scriptToCred :: Crypto crypto => Script crypto -> Credential crypto
scriptToCred = ScriptHashObj . hashScript

-- | Create a base address from a pair of multi-sig scripts (pay and stake)
scriptsToAddr :: Crypto crypto => (Script crypto, Script crypto) -> Addr crypto
scriptsToAddr (payScript, stakeScript) =
  AddrBase (scriptToCred payScript) (scriptToCred stakeScript)
