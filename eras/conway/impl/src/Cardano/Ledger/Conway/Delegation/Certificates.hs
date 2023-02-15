{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Ledger.Conway.Delegation.Certificates (
  ConwayDCert (..),
  transDCert,
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
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

instance Crypto c => DecCBOR (ConwayDCert c) where
  decCBOR = decodeRecordSum "ConwayDCert" $
    \case
      0 -> do
        x <- decCBOR
        pure (2, ConwayDCertDeleg . RegKey $ x)
      1 -> do
        x <- decCBOR
        pure (2, ConwayDCertDeleg . DeRegKey $ x)
      2 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, ConwayDCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- decCBORGroup
        pure (fromIntegral (1 + listLenInt group), ConwayDCertPool (RegPool group))
      4 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, ConwayDCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- decCBOR
        b <- decCBOR
        c <- decCBOR
        pure (4, ConwayDCertConstitutional $ ConstitutionalDelegCert a b c)
      k -> invalidKey k

instance Crypto c => EncCBOR (ConwayDCert c) where
  encCBOR = \case
    -- DCertDeleg
    ConwayDCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR cred
    ConwayDCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR cred
    ConwayDCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR cred
        <> encCBOR poolkh
    -- DCertPool
    ConwayDCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> encCBOR (3 :: Word8)
        <> encCBORGroup poolParams
    ConwayDCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR vk
        <> encCBOR epoch
    -- DCertGenesis
    ConwayDCertConstitutional (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> encCBOR (5 :: Word8)
        <> encCBOR gk
        <> encCBOR kh
        <> encCBOR vrf

transDCert :: ConwayDCert c -> DCert c
transDCert (ConwayDCertDeleg dc) = DCertDeleg dc
transDCert (ConwayDCertPool pc) = DCertPool pc
transDCert (ConwayDCertConstitutional gdc) = DCertGenesis gdc
