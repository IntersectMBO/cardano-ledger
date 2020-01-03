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
import           TxData (Addr (..), Credential (..), MultiSig, RewardAcnt (..))

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

scriptToCred :: Crypto crypto => MultiSig crypto -> Credential crypto
scriptToCred = ScriptHashObj . hashScript

scriptsToAddr :: Crypto crypto => (MultiSig crypto, MultiSig crypto) -> Addr crypto
scriptsToAddr (payScript, stakeScript) =
  AddrBase (scriptToCred payScript) (scriptToCred stakeScript)
