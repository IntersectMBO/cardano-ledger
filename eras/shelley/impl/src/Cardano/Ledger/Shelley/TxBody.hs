{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxBody (
  ShelleyDelegCert,
  Delegation (..),
  GenesisDelegCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  PoolCert (..),
  PoolMetadata (..),
  PoolParams (..),
  Ptr (..),
  RewardAcnt (..),
  StakePoolRelay (..),
  ShelleyTxBody (
    ShelleyTxBody,
    TxBodyConstr,
    stbInputs,
    stbOutputs,
    stbCerts,
    stbWithdrawals,
    stbTxFee,
    stbTTL,
    stbUpdate,
    stbMDHash
  ),
  ShelleyEraTxBody (..),
  ShelleyTxBodyRaw (..),
  EraIndependentTxBody,
  TxOut,
  ShelleyTxOut (ShelleyTxOut, TxOutCompact),
  Url,
  Withdrawals (..),
  Wdrl,
  pattern Wdrl,
  --
  module Cardano.Ledger.Keys.WitVKey,
  witKeyHash,
  wvkBytes,
  --
  SizeOfPoolOwners (..),
  SizeOfPoolRelays (..),

  -- * Helpers
  addrEitherShelleyTxOutL,
  valueEitherShelleyTxOutL,
) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Url)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Field,
  Wrapped (..),
  decode,
  encode,
  encodeKeyedStrictMaybe,
  field,
  invalidField,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxCert (
  GenesisDelegCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  ShelleyDelegCert,
 )
import Cardano.Ledger.Shelley.TxOut (
  ShelleyTxOut (..),
  addrEitherShelleyTxOutL,
  valueEitherShelleyTxOutL,
 )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- ==============================
-- The underlying type for TxBody

data ShelleyTxBodyRaw era = ShelleyTxBodyRaw
  { stbrInputs :: !(Set (TxIn (EraCrypto era)))
  , stbrOutputs :: !(StrictSeq (TxOut era))
  , stbrCerts :: !(StrictSeq (TxCert era))
  , stbrWithdrawals :: !(Withdrawals (EraCrypto era))
  , stbrTxFee :: !Coin
  , stbrTTL :: !SlotNo
  , stbrUpdate :: !(StrictMaybe (Update era))
  , stbrMDHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  }
  deriving (Generic, Typeable)

deriving instance
  (NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (ShelleyTxBodyRaw era)

deriving instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (ShelleyTxBodyRaw era)

deriving instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (ShelleyTxBodyRaw era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (ShelleyTxBodyRaw era)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ShelleyTxBody era)

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  ) =>
  DecCBOR (ShelleyTxBodyRaw era)
  where
  decCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          basicShelleyTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]
      )

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  ) =>
  DecCBOR (Annotator (ShelleyTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody ::
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  ) =>
  Word ->
  Field (ShelleyTxBodyRaw era)
boxBody 0 = field (\x tx -> tx {stbrInputs = x}) From
boxBody 1 = field (\x tx -> tx {stbrOutputs = x}) From
boxBody 4 = field (\x tx -> tx {stbrCerts = x}) From
boxBody 5 = field (\x tx -> tx {stbrWithdrawals = x}) From
boxBody 2 = field (\x tx -> tx {stbrTxFee = x}) From
boxBody 3 = field (\x tx -> tx {stbrTTL = x}) From
boxBody 6 = ofield (\x tx -> tx {stbrUpdate = x}) From
boxBody 7 = ofield (\x tx -> tx {stbrMDHash = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (Era era, EncCBOR (TxOut era), EncCBOR (TxCert era), EncCBOR (PParamsUpdate era)) =>
  ShelleyTxBodyRaw era ->
  Encode ('Closed 'Sparse) (ShelleyTxBodyRaw era)
txSparse (ShelleyTxBodyRaw input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> ShelleyTxBodyRaw i o c w f t u h)
    !> Key 0 (To input) -- We don't have to send these in ShelleyTxBodyRaw order
    !> Key 1 (To output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (To cert))
    !> Omit (null . unWithdrawals) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 update
    !> encodeKeyedStrictMaybe 7 hash

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
basicShelleyTxBodyRaw :: ShelleyTxBodyRaw era
basicShelleyTxBodyRaw =
  ShelleyTxBodyRaw
    { stbrInputs = Set.empty
    , stbrOutputs = StrictSeq.empty
    , stbrTxFee = Coin 0
    , stbrTTL = SlotNo 0
    , stbrCerts = StrictSeq.empty
    , stbrWithdrawals = Withdrawals Map.empty
    , stbrUpdate = SNothing
    , stbrMDHash = SNothing
    }

instance
  (Era era, EncCBOR (TxOut era), EncCBOR (TxCert era), EncCBOR (PParamsUpdate era)) =>
  EncCBOR (ShelleyTxBodyRaw era)
  where
  encCBOR = encode . txSparse

-- ====================================================
-- Introduce ShelleyTxBody as a newtype around a MemoBytes

newtype ShelleyTxBody era = TxBodyConstr (MemoBytes ShelleyTxBodyRaw era)
  deriving (Generic, Typeable)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized ShelleyTxBody where
  type RawType ShelleyTxBody = ShelleyTxBodyRaw

instance Crypto c => EraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxBody (ShelleyEra StandardCrypto) #-}

  type TxBody (ShelleyEra c) = ShelleyTxBody (ShelleyEra c)

  mkBasicTxBody = mkMemoized basicShelleyTxBodyRaw

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINE allInputsTxBodyF #-}

  inputsTxBodyL =
    lensMemoRawType stbrInputs $ \txBodyRaw inputs -> txBodyRaw {stbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType stbrOutputs $ \txBodyRaw outputs -> txBodyRaw {stbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType stbrTxFee $ \txBodyRaw fee -> txBodyRaw {stbrTxFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType stbrMDHash $ \txBodyRaw auxDataHash -> txBodyRaw {stbrMDHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  withdrawalsTxBodyL =
    lensMemoRawType stbrWithdrawals $ \txBodyRaw withdrawals -> txBodyRaw {stbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType stbrCerts $ \txBodyRaw certs -> txBodyRaw {stbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

instance Crypto c => ShelleyEraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyEra StandardCrypto) #-}

  ttlTxBodyL =
    lensMemoRawType stbrTTL $ \txBodyRaw ttl -> txBodyRaw {stbrTTL = ttl}
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType stbrUpdate $ \txBodyRaw update -> txBodyRaw {stbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (ShelleyTxBody era)

deriving newtype instance EraTxBody era => NFData (ShelleyTxBody era)

deriving instance EraTxBody era => Show (ShelleyTxBody era)

deriving instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (ShelleyTxBody era)

deriving via
  Mem ShelleyTxBodyRaw era
  instance
    EraTxBody era => DecCBOR (Annotator (ShelleyTxBody era))

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  (EraTxOut era, EncCBOR (TxCert era)) =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
  Withdrawals (EraCrypto era) ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  ShelleyTxBody era
pattern ShelleyTxBody
  { stbInputs
  , stbOutputs
  , stbCerts
  , stbWithdrawals
  , stbTxFee
  , stbTTL
  , stbUpdate
  , stbMDHash
  } <-
  ( getMemoRawType ->
      ShelleyTxBodyRaw
        { stbrInputs = stbInputs
        , stbrOutputs = stbOutputs
        , stbrCerts = stbCerts
        , stbrWithdrawals = stbWithdrawals
        , stbrTxFee = stbTxFee
        , stbrTTL = stbTTL
        , stbrUpdate = stbUpdate
        , stbrMDHash = stbMDHash
        }
    )
  where
    ShelleyTxBody
      inputs
      outputs
      certs
      withdrawals
      txFee
      ttl
      update
      mDHash =
        mkMemoized $
          ShelleyTxBodyRaw
            { stbrInputs = inputs
            , stbrOutputs = outputs
            , stbrCerts = certs
            , stbrWithdrawals = withdrawals
            , stbrTxFee = txFee
            , stbrTTL = ttl
            , stbrUpdate = update
            , stbrMDHash = mDHash
            }

{-# COMPLETE ShelleyTxBody #-}

-- =========================================

type instance MemoHashIndex ShelleyTxBodyRaw = EraIndependentTxBody

instance
  (Era era, c ~ EraCrypto era) =>
  HashAnnotated (ShelleyTxBody era) EraIndependentTxBody c
  where
  hashAnnotated = getMemoSafeHash

-- ===============================================================

witKeyHash :: WitVKey kr c -> KeyHash 'Witness c
witKeyHash = witVKeyHash
{-# DEPRECATED witKeyHash "In favor of `witVKeyHash`" #-}

wvkBytes :: WitVKey kr c -> BSL.ByteString
wvkBytes = witVKeyBytes
{-# DEPRECATED wvkBytes "In favor of `witVKeyBytes`" #-}
