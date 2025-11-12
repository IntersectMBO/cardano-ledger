{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
  Annotator,
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
import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (InspectHeap (..), NoThunks (..))

class (ShelleyEraTxCert era, EraTxBody era, AtMostEra "Babbage" era) => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody TopTx era) SlotNo

  updateTxBodyL :: Lens' (TxBody TopTx era) (StrictMaybe (Update era))

-- ==============================
-- The underlying type for TxBody

data ShelleyTxBodyRaw l era where
  ShelleyTxBodyRaw ::
    { stbrInputs :: !(Set TxIn)
    , stbrOutputs :: !(StrictSeq (TxOut era))
    , stbrCerts :: !(StrictSeq (TxCert era))
    , stbrWithdrawals :: !Withdrawals
    , stbrFee :: !Coin
    , stbrTtl :: !SlotNo
    , stbrUpdate :: !(StrictMaybe (Update era))
    , stbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    } ->
    ShelleyTxBodyRaw TopTx era

instance HasEraTxLevel ShelleyTxBodyRaw ShelleyEra where
  toSTxLevel ShelleyTxBodyRaw {} = STopTxOnly @ShelleyEra

deriving via
  InspectHeap (ShelleyTxBodyRaw l era)
  instance
    (Typeable era, Typeable l) => NoThunks (ShelleyTxBodyRaw l era)

instance EraTxBody era => NFData (ShelleyTxBodyRaw t era) where
  rnf ShelleyTxBodyRaw {stbrWithdrawals, stbrUpdate, stbrAuxDataHash} =
    stbrWithdrawals `deepseq` stbrUpdate `deepseq` rnf stbrAuxDataHash

deriving instance EraTxBody era => Eq (ShelleyTxBodyRaw l era)

deriving instance EraTxBody era => Show (ShelleyTxBodyRaw l era)

instance Typeable l => DecCBOR (ShelleyTxBodyRaw l ShelleyEra) where
  decCBOR =
    mkSTxTopLevelM @l $
      decode $
        SparseKeyed
          "TxBody"
          basicShelleyTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]

instance Typeable l => DecCBOR (Annotator (ShelleyTxBodyRaw l ShelleyEra)) where
  decCBOR = pure <$> decCBOR

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody :: EraTxBody era => Word -> Field (ShelleyTxBodyRaw t era)
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
txSparse ::
  EraTxBody era =>
  ShelleyTxBodyRaw t era -> Encode (Closed Sparse) (ShelleyTxBodyRaw t era)
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
basicShelleyTxBodyRaw :: ShelleyTxBodyRaw TopTx era
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

instance EraTxBody era => EncCBOR (ShelleyTxBodyRaw l era) where
  encCBOR = encode . txSparse

instance Memoized (TxBody l ShelleyEra) where
  type RawType (TxBody l ShelleyEra) = ShelleyTxBodyRaw l ShelleyEra

instance EqRaw (TxBody l ShelleyEra)

instance HasEraTxLevel TxBody ShelleyEra where
  toSTxLevel = toSTxLevel . getMemoRawType

instance EraTxBody ShelleyEra where
  newtype TxBody l ShelleyEra = MkShelleyTxBody (MemoBytes (ShelleyTxBodyRaw l ShelleyEra))
    deriving (Generic)
    deriving newtype (SafeToHash, ToCBOR, EncCBOR)

  mkBasicTxBody = asSTxTopLevel $ mkMemoizedEra @ShelleyEra basicShelleyTxBodyRaw

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINE allInputsTxBodyF #-}

  inputsTxBodyL =
    lensMemoRawType @ShelleyEra (\ShelleyTxBodyRaw {stbrInputs} -> stbrInputs) $
      \txBodyRaw@ShelleyTxBodyRaw {} inputs -> txBodyRaw {stbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @ShelleyEra (\ShelleyTxBodyRaw {stbrOutputs} -> stbrOutputs) $
      \txBodyRaw@ShelleyTxBodyRaw {} outputs -> txBodyRaw {stbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @ShelleyEra stbrFee $
      \txBodyRaw fee -> txBodyRaw {stbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @ShelleyEra (\ShelleyTxBodyRaw {stbrAuxDataHash} -> stbrAuxDataHash) $
      \txBodyRaw@ShelleyTxBodyRaw {} auxDataHash -> txBodyRaw {stbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  withdrawalsTxBodyL =
    lensMemoRawType @ShelleyEra (\ShelleyTxBodyRaw {stbrWithdrawals} -> stbrWithdrawals) $
      \txBodyRaw@ShelleyTxBodyRaw {} withdrawals -> txBodyRaw {stbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @ShelleyEra (\ShelleyTxBodyRaw {stbrCerts} -> stbrCerts) $
      \txBodyRaw@ShelleyTxBodyRaw {} certs -> txBodyRaw {stbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

instance ShelleyEraTxBody ShelleyEra where
  ttlTxBodyL =
    lensMemoRawType @ShelleyEra stbrTtl $ \txBodyRaw ttl -> txBodyRaw {stbrTtl = ttl}
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @ShelleyEra stbrUpdate $ \txBodyRaw update -> txBodyRaw {stbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

deriving newtype instance Typeable l => NoThunks (TxBody l ShelleyEra)

deriving newtype instance NFData (TxBody l ShelleyEra)

deriving instance Show (TxBody l ShelleyEra)

deriving instance Eq (TxBody l ShelleyEra)

deriving via
  Mem (ShelleyTxBodyRaw l ShelleyEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l ShelleyEra))

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
  TxBody TopTx ShelleyEra
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

type instance MemoHashIndex (ShelleyTxBodyRaw t era) = EraIndependentTxBody

instance HashAnnotated (TxBody l ShelleyEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- ===============================================================

-- | Count number of Genesis keys supplied in the `updateTxBodyL` field.
getShelleyGenesisKeyHashCountTxBody :: ShelleyEraTxBody era => TxBody TopTx era -> Int
getShelleyGenesisKeyHashCountTxBody txBody =
  case txBody ^. updateTxBodyL of
    SJust (Update (ProposedPPUpdates m) _) -> Map.size m
    _ -> 0
