{-# LANGUAGE DataKinds #-}

module Shelley.Spec.Ledger.Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , scriptsToAddr
  , scriptToCred
  , toAddr
  , toCred
  , serialiseAddr
  , deserialiseAddr
  )
where

import           Data.ByteString (ByteString)
import           Cardano.Binary (serialize', decodeFull')

import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Keys (KeyDiscriminator (..), KeyPair, hashKey, vKey)
import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.Tx (hashScript)
import           Shelley.Spec.Ledger.TxData (Addr (..), Credential (..), RewardAcnt (..))

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

-- | Serialise an address to the external format.
--
-- Note: this is not the final format, this function will be updated later to
-- use the final format.
--
-- See <https://github.com/input-output-hk/cardano-ledger-specs/issues/1367>
--
serialiseAddr :: Crypto crypto => Addr crypto -> ByteString
serialiseAddr = serialize'

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
--
-- Note: this is not the final format, this function will be updated later to
-- use the final format.
--
-- See <https://github.com/input-output-hk/cardano-ledger-specs/issues/1367>
--
deserialiseAddr :: Crypto crypto => ByteString -> Maybe (Addr crypto)
deserialiseAddr = either (const Nothing) id . decodeFull'

