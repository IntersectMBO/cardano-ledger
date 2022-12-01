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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
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
    TxBody,
    ShelleyTxBody
      ( ShelleyTxBody,
        TxBodyConstr,
        stbInputs,
        stbOutputs,
        stbCerts,
        stbWdrls,
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
    Wdrl (..),
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
  )
where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Url)
import Cardano.Ledger.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (..),
  )
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
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
import Cardano.Ledger.Compactible (Compactible (CompactForm))
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Ptr (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.MemoBytes
  ( Mem,
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
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..), Wdrl (..))
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxOut
  ( ShelleyTxOut (..),
    TxOut,
    addrEitherShelleyTxOutL,
    valueEitherShelleyTxOutL,
  )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (DecodeNonNegative (..))
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
  { stbrInputs :: !(Set (TxIn (EraCrypto era))),
    stbrOutputs :: !(StrictSeq (ShelleyTxOut era)),
    stbrCerts :: !(StrictSeq (DCert (EraCrypto era))),
    stbrWdrls :: !(Wdrl (EraCrypto era)),
    stbrTxFee :: !Coin,
    stbrTTL :: !SlotNo,
    stbrUpdate :: !(StrictMaybe (Update era)),
    stbrMDHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  }
  deriving (Generic, Typeable)

deriving instance NoThunks (PParamsUpdate era) => NoThunks (ShelleyTxBodyRaw era)

deriving instance (Era era, NFData (PParamsUpdate era)) => NFData (ShelleyTxBodyRaw era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (ShelleyTxBodyRaw era)

deriving instance
  (Era era, Show (PParamsUpdate era), Compactible (Value era), Show (Value era)) =>
  Show (ShelleyTxBodyRaw era)

instance
  ( Era era,
    FromCBOR (PParamsUpdate era),
    DecodeNonNegative (Value era),
    Compactible (Value era),
    Show (Value era)
  ) =>
  FromCBOR (ShelleyTxBodyRaw era)
  where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          basicShelleyTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]
      )

instance
  ( Era era,
    FromCBOR (PParamsUpdate era),
    DecodeNonNegative (Value era),
    Compactible (Value era),
    Show (Value era)
  ) =>
  FromCBOR (Annotator (ShelleyTxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody ::
  ( Era era,
    FromCBOR (PParamsUpdate era),
    DecodeNonNegative (Value era),
    Compactible (Value era),
    Show (Value era)
  ) =>
  Word ->
  Field (ShelleyTxBodyRaw era)
boxBody 0 = field (\x tx -> tx {stbrInputs = x}) From
boxBody 1 = field (\x tx -> tx {stbrOutputs = x}) From
boxBody 4 = field (\x tx -> tx {stbrCerts = x}) From
boxBody 5 = field (\x tx -> tx {stbrWdrls = x}) From
boxBody 2 = field (\x tx -> tx {stbrTxFee = x}) From
boxBody 3 = field (\x tx -> tx {stbrTTL = x}) From
boxBody 6 = ofield (\x tx -> tx {stbrUpdate = x}) From
boxBody 7 = ofield (\x tx -> tx {stbrMDHash = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (Era era, ToCBOR (PParamsUpdate era), ToCBOR (CompactForm (Value era))) =>
  ShelleyTxBodyRaw era ->
  Encode ('Closed 'Sparse) (ShelleyTxBodyRaw era)
txSparse (ShelleyTxBodyRaw input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> ShelleyTxBodyRaw i o c w f t u h)
    !> Key 0 (To input) -- We don't have to send these in ShelleyTxBodyRaw order
    !> Key 1 (To output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (To cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 update
    !> encodeKeyedStrictMaybe 7 hash

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
basicShelleyTxBodyRaw :: ShelleyTxBodyRaw era
basicShelleyTxBodyRaw =
  ShelleyTxBodyRaw
    { stbrInputs = Set.empty,
      stbrOutputs = StrictSeq.empty,
      stbrTxFee = Coin 0,
      stbrTTL = SlotNo 0,
      stbrCerts = StrictSeq.empty,
      stbrWdrls = Wdrl Map.empty,
      stbrUpdate = SNothing,
      stbrMDHash = SNothing
    }

instance
  (Era era, ToCBOR (PParamsUpdate era), ToCBOR (CompactForm (Value era))) =>
  ToCBOR (ShelleyTxBodyRaw era)
  where
  toCBOR = encode . txSparse

-- ====================================================
-- Introduce ShelleyTxBody as a newtype around a MemoBytes

newtype ShelleyTxBody era = TxBodyConstr (MemoBytes ShelleyTxBodyRaw era)
  deriving (Generic, Typeable)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized ShelleyTxBody where
  type RawType ShelleyTxBody = ShelleyTxBodyRaw

type TxBody era = ShelleyTxBody era

{-# DEPRECATED TxBody "Use `ShelleyTxBody` instead" #-}

instance CC.Crypto c => EraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxBody (ShelleyEra CC.StandardCrypto) #-}

  type TxBody (ShelleyEra c) = ShelleyTxBody (ShelleyEra c)

  mkBasicTxBody = mkMemoized basicShelleyTxBodyRaw

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

instance CC.Crypto c => ShelleyEraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyEra CC.StandardCrypto) #-}

  wdrlsTxBodyL =
    lensMemoRawType stbrWdrls $ \txBodyRaw wdrls -> txBodyRaw {stbrWdrls = wdrls}
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL =
    lensMemoRawType stbrTTL $ \txBodyRaw ttl -> txBodyRaw {stbrTTL = ttl}
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType stbrUpdate $ \txBodyRaw update -> txBodyRaw {stbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType stbrCerts $ \txBodyRaw certs -> txBodyRaw {stbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

deriving newtype instance
  (Era era, NoThunks (PParamsUpdate era)) => NoThunks (TxBody era)

deriving newtype instance EraTxBody era => NFData (TxBody era)

deriving instance EraTxBody era => Show (TxBody era)

deriving instance (Era era, Eq (PParamsUpdate era), Eq (CompactForm (Value era))) => Eq (TxBody era)

deriving via Mem ShelleyTxBodyRaw era instance EraTxBody era => FromCBOR (Annotator (TxBody era))

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  EraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  ShelleyTxBody era
pattern ShelleyTxBody
  { stbInputs,
    stbOutputs,
    stbCerts,
    stbWdrls,
    stbTxFee,
    stbTTL,
    stbUpdate,
    stbMDHash
  } <-
  ( getMemoRawType ->
      ShelleyTxBodyRaw
        { stbrInputs = stbInputs,
          stbrOutputs = stbOutputs,
          stbrCerts = stbCerts,
          stbrWdrls = stbWdrls,
          stbrTxFee = stbTxFee,
          stbrTTL = stbTTL,
          stbrUpdate = stbUpdate,
          stbrMDHash = stbMDHash
        }
    )
  where
    ShelleyTxBody
      inputs
      outputs
      certs
      wdrls
      txFee
      ttl
      update
      mDHash =
        mkMemoized $
          ShelleyTxBodyRaw
            { stbrInputs = inputs,
              stbrOutputs = outputs,
              stbrCerts = certs,
              stbrWdrls = wdrls,
              stbrTxFee = txFee,
              stbrTTL = ttl,
              stbrUpdate = update,
              stbrMDHash = mDHash
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
