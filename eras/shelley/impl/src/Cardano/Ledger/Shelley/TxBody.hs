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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxBody (
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
  RewardAccount (..),
  Withdrawals (..),
  getShelleyGenesisKeyHashCountTxBody,
) where

import Cardano.Ledger.Address (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class (ShelleyEraTxCert era, EraTxBody era, ProtVerAtMost era 8) => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: Lens' (TxBody era) (StrictMaybe (Update era))

-- ==============================
-- The underlying type for TxBody

data ShelleyTxBodyRaw era = ShelleyTxBodyRaw
  { stbrInputs :: !(Set TxIn)
  , stbrOutputs :: !(StrictSeq (TxOut era))
  , stbrCerts :: !(StrictSeq (TxCert era))
  , stbrWithdrawals :: !Withdrawals
  , stbrTxFee :: !Coin
  , stbrTTL :: !SlotNo
  , stbrUpdate :: !(StrictMaybe (Update era))
  , stbrMDHash :: !(StrictMaybe TxAuxDataHash)
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
    , stbrTTL = SlotNo maxBound -- transaction is eternally valid by default
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

newtype ShelleyTxBody era = TxBodyConstr (MemoBytes (ShelleyTxBodyRaw era))
  deriving (Generic)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized (ShelleyTxBody era) where
  type RawType (ShelleyTxBody era) = ShelleyTxBodyRaw era

instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  EqRaw (ShelleyTxBody era)

instance EraTxBody ShelleyEra where
  type TxBody ShelleyEra = ShelleyTxBody ShelleyEra

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
    lensMemoRawType @ShelleyEra stbrTxFee $
      \txBodyRaw fee -> txBodyRaw {stbrTxFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @ShelleyEra stbrMDHash $
      \txBodyRaw auxDataHash -> txBodyRaw {stbrMDHash = auxDataHash}
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
        ++ "since there is no TxBody instance for ByronEra"

instance ShelleyEraTxBody ShelleyEra where
  ttlTxBodyL =
    lensMemoRawType @ShelleyEra stbrTTL $ \txBodyRaw ttl -> txBodyRaw {stbrTTL = ttl}
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @ShelleyEra stbrUpdate $ \txBodyRaw update -> txBodyRaw {stbrUpdate = update}
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
  Mem (ShelleyTxBodyRaw era)
  instance
    EraTxBody era => DecCBOR (Annotator (ShelleyTxBody era))

deriving newtype instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  ) =>
  DecCBOR (ShelleyTxBody era)

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  forall era.
  (EraTxOut era, EncCBOR (TxCert era)) =>
  Set TxIn ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
  Withdrawals ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe TxAuxDataHash ->
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
        mkMemoizedEra @era $
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

type instance MemoHashIndex (ShelleyTxBodyRaw era) = EraIndependentTxBody

instance Era era => HashAnnotated (ShelleyTxBody era) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- ===============================================================

-- | Count number of Genesis keys supplied in the `updateTxBodyL` field.
getShelleyGenesisKeyHashCountTxBody :: ShelleyEraTxBody era => TxBody era -> Int
getShelleyGenesisKeyHashCountTxBody txBody =
  case txBody ^. updateTxBodyL of
    SJust (Update (ProposedPPUpdates m) _) -> Map.size m
    _ -> 0
