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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxBody (
  TxBody (
    ShelleyTxBody,
    MkShelleyTxBody,
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
  RewardAccount (..),
  Withdrawals (..),
  getShelleyGenesisKeyHashCountTxBody,
) where

import Cardano.Ledger.Address (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
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
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.TxCert (ShelleyEraTxCert (..))
import Cardano.Ledger.Shelley.TxOut ()
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class (ShelleyEraTxCert era, EraTxBody era, ProtVerAtMost era 8) => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: Lens' (TxBody era) (StrictMaybe (Update era))

-- ==============================
-- The underlying type for TxBody

data ShelleyTxBodyRaw = ShelleyTxBodyRaw
  { stbrInputs :: !(Set TxIn)
  , stbrOutputs :: !(StrictSeq (TxOut ShelleyEra))
  , stbrCerts :: !(StrictSeq (TxCert ShelleyEra))
  , stbrWithdrawals :: !Withdrawals
  , stbrFee :: !Coin
  , stbrTtl :: !SlotNo
  , stbrUpdate :: !(StrictMaybe (Update ShelleyEra))
  , stbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  }
  deriving (Generic)

deriving instance NoThunks ShelleyTxBodyRaw

deriving instance NFData ShelleyTxBodyRaw

deriving instance Eq ShelleyTxBodyRaw

deriving instance Show ShelleyTxBodyRaw

-- | Encodes memoized bytes created upon construction.
instance EncCBOR (TxBody ShelleyEra)

instance DecCBOR ShelleyTxBodyRaw where
  decCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          basicShelleyTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]
      )

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody :: Word -> Field ShelleyTxBodyRaw
boxBody 0 = field (\x tx -> tx {stbrInputs = x}) From
boxBody 1 = field (\x tx -> tx {stbrOutputs = x}) From
boxBody 4 = field (\x tx -> tx {stbrCerts = x}) From
boxBody 5 = field (\x tx -> tx {stbrWithdrawals = x}) From
boxBody 2 = field (\x tx -> tx {stbrFee = x}) From
boxBody 3 = field (\x tx -> tx {stbrTtl = x}) From
boxBody 6 = ofield (\x tx -> tx {stbrUpdate = x}) From
boxBody 7 = ofield (\x tx -> tx {stbrAuxDataHash = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse :: ShelleyTxBodyRaw -> Encode ('Closed 'Sparse) ShelleyTxBodyRaw
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
basicShelleyTxBodyRaw :: ShelleyTxBodyRaw
basicShelleyTxBodyRaw =
  ShelleyTxBodyRaw
    { stbrInputs = Set.empty
    , stbrOutputs = StrictSeq.empty
    , stbrFee = Coin 0
    , stbrTtl = SlotNo maxBound -- transaction is eternally valid by default
    , stbrCerts = StrictSeq.empty
    , stbrWithdrawals = Withdrawals Map.empty
    , stbrUpdate = SNothing
    , stbrAuxDataHash = SNothing
    }

instance EncCBOR ShelleyTxBodyRaw where
  encCBOR = encode . txSparse

instance Memoized (TxBody ShelleyEra) where
  type RawType (TxBody ShelleyEra) = ShelleyTxBodyRaw

instance EqRaw (TxBody ShelleyEra)

instance EraTxBody ShelleyEra where
  newtype TxBody ShelleyEra = MkShelleyTxBody (MemoBytes ShelleyTxBodyRaw)
    deriving (Generic)
    deriving newtype (SafeToHash, ToCBOR)

  mkBasicTxBody = mkMemoizedEra @ShelleyEra basicShelleyTxBodyRaw

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINE allInputsTxBodyF #-}

  inputsTxBodyL =
    lensMemoRawType @ShelleyEra stbrInputs $
      \txBodyRaw inputs -> txBodyRaw {stbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @ShelleyEra stbrOutputs $
      \txBodyRaw outputs -> txBodyRaw {stbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @ShelleyEra stbrFee $
      \txBodyRaw fee -> txBodyRaw {stbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @ShelleyEra stbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {stbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  withdrawalsTxBodyL =
    lensMemoRawType @ShelleyEra stbrWithdrawals $
      \txBodyRaw withdrawals -> txBodyRaw {stbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @ShelleyEra stbrCerts $
      \txBodyRaw certs -> txBodyRaw {stbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

  upgradeTxBody =
    error $
      "Calling this function will cause a compilation error, "
        ++ "since there is no `EraTxBody` instance for `ByronEra`"

instance ShelleyEraTxBody ShelleyEra where
  ttlTxBodyL =
    lensMemoRawType @ShelleyEra stbrTtl $ \txBodyRaw ttl -> txBodyRaw {stbrTtl = ttl}
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @ShelleyEra stbrUpdate $ \txBodyRaw update -> txBodyRaw {stbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

deriving newtype instance NoThunks (TxBody ShelleyEra)

deriving newtype instance NFData (TxBody ShelleyEra)

deriving instance Show (TxBody ShelleyEra)

deriving instance Eq (TxBody ShelleyEra)

deriving newtype instance DecCBOR (TxBody ShelleyEra)

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  Set TxIn ->
  StrictSeq (TxOut ShelleyEra) ->
  StrictSeq (TxCert ShelleyEra) ->
  Withdrawals ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update ShelleyEra) ->
  StrictMaybe TxAuxDataHash ->
  TxBody ShelleyEra
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
        , stbrFee = stbTxFee
        , stbrTtl = stbTTL
        , stbrUpdate = stbUpdate
        , stbrAuxDataHash = stbMDHash
        }
    )
  where
    ShelleyTxBody
      inputs
      outputs
      certs
      withdrawals
      fee
      ttl
      update
      auxDataHash =
        mkMemoizedEra @ShelleyEra $
          ShelleyTxBodyRaw
            { stbrInputs = inputs
            , stbrOutputs = outputs
            , stbrCerts = certs
            , stbrWithdrawals = withdrawals
            , stbrFee = fee
            , stbrTtl = ttl
            , stbrUpdate = update
            , stbrAuxDataHash = auxDataHash
            }

{-# COMPLETE ShelleyTxBody #-}

-- =========================================

type instance MemoHashIndex ShelleyTxBodyRaw = EraIndependentTxBody

instance HashAnnotated (TxBody ShelleyEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- ===============================================================

-- | Count number of Genesis keys supplied in the `updateTxBodyL` field.
getShelleyGenesisKeyHashCountTxBody :: ShelleyEraTxBody era => TxBody era -> Int
getShelleyGenesisKeyHashCountTxBody txBody =
  case txBody ^. updateTxBodyL of
    SJust (Update (ProposedPPUpdates m) _) -> Map.size m
    _ -> 0
