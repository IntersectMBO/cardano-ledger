{-# LANGUAGE DataKinds #-}

module Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , toAddr
  , toCred
  )
where

import           Cardano.Ledger.Shelley.Crypto
import           Keys (KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           TxData (Addr (..), Credential (..), RewardAcnt (..))

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
