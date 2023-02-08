{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Ledger.Conway.Delegation.Certificates (
  ConwayDCert (..),
  transDCert,
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  FromCBOR (..),
  FromCBORGroup (..),
  ToCBOR (..),
  ToCBORGroup (..),
  decodeRecordSum,
  encodeListLen,
  listLenInt,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Delegation.Certificates (
  ConstitutionalDelegCert (..),
  DCert (..),
  DelegCert (..),
  Delegation (..),
  PoolCert (..),
 )
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data ConwayDCert c
  = ConwayDCertDeleg !(DelegCert c)
  | ConwayDCertPool !(PoolCert c)
  | ConwayDCertConstitutional !(ConstitutionalDelegCert c)
  deriving (Show, Generic, Eq)

instance NFData (ConwayDCert c)

instance NoThunks (ConwayDCert c)

instance Crypto c => FromCBOR (ConwayDCert c) where
  fromCBOR = decodeRecordSum "ConwayDCert" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, ConwayDCertDeleg . RegKey $ x)
      1 -> do
        x <- fromCBOR
        pure (2, ConwayDCertDeleg . DeRegKey $ x)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, ConwayDCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- fromCBORGroup
        pure (fromIntegral (1 + listLenInt group), ConwayDCertPool (RegPool group))
      4 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, ConwayDCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, ConwayDCertConstitutional $ ConstitutionalDelegCert a b c)
      k -> invalidKey k

instance Crypto c => ToCBOR (ConwayDCert c) where
  toCBOR = \case
    -- DCertDeleg
    ConwayDCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR cred
    ConwayDCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR cred
    ConwayDCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR cred
        <> toCBOR poolkh
    -- DCertPool
    ConwayDCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> toCBOR (3 :: Word8)
        <> toCBORGroup poolParams
    ConwayDCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch
    -- DCertGenesis
    ConwayDCertConstitutional (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> toCBOR (5 :: Word8)
        <> toCBOR gk
        <> toCBOR kh
        <> toCBOR vrf

transDCert :: ConwayDCert c -> DCert c
transDCert (ConwayDCertDeleg dc) = DCertDeleg dc
transDCert (ConwayDCertPool pc) = DCertPool pc
transDCert (ConwayDCertConstitutional gdc) = DCertGenesis gdc
