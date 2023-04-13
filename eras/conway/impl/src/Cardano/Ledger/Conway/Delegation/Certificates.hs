{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Ledger.Conway.Delegation.Certificates (
  ConwayDCert (..),
  ConwayDelegCert (..),
  Delegatee (..),
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  EncCBORGroup (..),
  decCBORGroup,
  decodeRecordSum,
  encodeListLen,
  listLenInt,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential, StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (
  ConstitutionalDelegCert (..),
  PoolCert (..),
 )
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | First type argument is the deposit
data Delegatee c
  = DelegStake !(KeyHash 'StakePool c)
  | DelegVote !(Credential 'Voting c)
  | DelegStakeVote !(KeyHash 'StakePool c) !(Credential 'Voting c)
  deriving (Show, Generic, Eq)

instance Crypto c => DecCBOR (Delegatee c) where
  decCBOR = decode . Summands "Delegatee" $ \case
    0 ->
      SumD DelegStake
        <! From
    1 ->
      SumD DelegVote
        <! From
    2 ->
      SumD DelegStakeVote
        <! From
        <! From
    k -> Invalid k

instance Crypto c => EncCBOR (Delegatee c) where
  encCBOR x = encode $ case x of
    DelegStake kh ->
      Sum DelegStake 0
        !> To kh
    DelegVote c ->
      Sum DelegVote 1
        !> To c
    DelegStakeVote kh c ->
      Sum DelegStakeVote 2
        !> To kh
        !> To c

instance NFData (Delegatee c)

instance NoThunks (Delegatee c)

data ConwayDelegCert c
  = ConwayDeleg !(StakeCredential c) !(Delegatee c) !Coin
  | ConwayReDeleg !(StakeCredential c) !(Delegatee c)
  | ConwayUnDeleg !(StakeCredential c) !Coin
  deriving (Show, Generic, Eq)

instance NFData (ConwayDelegCert c)

instance NoThunks (ConwayDelegCert c)

data ConwayDCert c
  = ConwayDCertDeleg !(ConwayDelegCert c)
  | ConwayDCertPool !(PoolCert c)
  | ConwayDCertConstitutional !(ConstitutionalDelegCert c)
  deriving (Show, Generic, Eq)

instance NFData (ConwayDCert c)

instance NoThunks (ConwayDCert c)

instance Crypto c => DecCBOR (ConwayDCert c) where
  decCBOR = decodeRecordSum "ConwayDCert" $
    \case
      3 -> do
        group <- decCBORGroup
        pure (listLenInt group + 1, ConwayDCertPool (RegPool group))
      4 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, ConwayDCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- decCBOR
        b <- decCBOR
        c <- decCBOR
        pure (4, ConwayDCertConstitutional $ ConstitutionalDelegCert a b c)
      6 -> do
        cred <- decCBOR
        delegatee <- decCBOR
        deposit <- decCBOR
        pure (4, ConwayDCertDeleg $ ConwayDeleg cred delegatee deposit)
      7 -> do
        cred <- decCBOR
        delegatee <- decCBOR
        pure (3, ConwayDCertDeleg $ ConwayReDeleg cred delegatee)
      8 -> do
        cred <- decCBOR
        deposit <- decCBOR
        pure (3, ConwayDCertDeleg $ ConwayUnDeleg cred deposit)
      k -> invalidKey k

instance Crypto c => EncCBOR (ConwayDCert c) where
  encCBOR = \case
    ConwayDCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> encCBOR (3 :: Word8)
        <> encCBORGroup poolParams
    ConwayDCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR vk
        <> encCBOR epoch
    ConwayDCertConstitutional (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> encCBOR (5 :: Word8)
        <> encCBOR gk
        <> encCBOR kh
        <> encCBOR vrf
    ConwayDCertDeleg (ConwayDeleg cred delegatee deposit) ->
      encodeListLen 4
        <> encCBOR (6 :: Word8)
        <> encCBOR cred
        <> encCBOR delegatee
        <> encCBOR deposit
    ConwayDCertDeleg (ConwayReDeleg cred delegatee) ->
      encodeListLen 3
        <> encCBOR (7 :: Word8)
        <> encCBOR cred
        <> encCBOR delegatee
    ConwayDCertDeleg (ConwayUnDeleg cred deposit) ->
      encodeListLen 3
        <> encCBOR (8 :: Word8)
        <> encCBOR cred
        <> encCBOR deposit
