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
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash
      ),
    ShelleyEraTxBody (..),
    TxBodyRaw (..),
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

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (..),
  )
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Url)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (CompactForm))
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Ptr (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes, pattern Memo)
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Serialization
  ( decodeSet,
    decodeStrictSeq,
    encodeFoldable,
  )
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
import Cardano.Ledger.Shelley.PoolParams
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..), TxOut, addrEitherShelleyTxOutL, valueEitherShelleyTxOutL)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (DecodeNonNegative (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import Data.Coders
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
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- ========================================================================

-- ---------------------------
-- WellFormed instances

-- ==============================
-- The underlying type for TxBody

data TxBodyRaw era = TxBodyRaw
  { _inputsX :: !(Set (TxIn (EraCrypto era))),
    _outputsX :: !(StrictSeq (ShelleyTxOut era)),
    _certsX :: !(StrictSeq (DCert (EraCrypto era))),
    _wdrlsX :: !(Wdrl (EraCrypto era)),
    _txfeeX :: !Coin,
    _ttlX :: !SlotNo,
    _txUpdateX :: !(StrictMaybe (Update era)),
    _mdHashX :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  }
  deriving (Generic, Typeable)

deriving instance NoThunks (PParamsUpdate era) => NoThunks (TxBodyRaw era)

deriving instance (Era era, NFData (PParamsUpdate era)) => NFData (TxBodyRaw era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (TxBodyRaw era)

deriving instance
  (Era era, Show (PParamsUpdate era), Compactible (Value era), Show (Value era)) =>
  Show (TxBodyRaw era)

instance
  ( Era era,
    FromCBOR (PParamsUpdate era),
    DecodeNonNegative (Value era),
    Compactible (Value era),
    Show (Value era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          baseTxBodyRaw
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
  FromCBOR (Annotator (TxBodyRaw era))
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
  Field (TxBodyRaw era)
boxBody 0 = field (\x tx -> tx {_inputsX = x}) (D (decodeSet fromCBOR))
boxBody 1 = field (\x tx -> tx {_outputsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 4 = field (\x tx -> tx {_certsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 5 = field (\x tx -> tx {_wdrlsX = x}) From
boxBody 2 = field (\x tx -> tx {_txfeeX = x}) From
boxBody 3 = field (\x tx -> tx {_ttlX = x}) From
boxBody 6 = ofield (\x tx -> tx {_txUpdateX = x}) From
boxBody 7 = ofield (\x tx -> tx {_mdHashX = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (Era era, ToCBOR (PParamsUpdate era), ToCBOR (CompactForm (Value era))) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
txSparse (TxBodyRaw input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> TxBodyRaw i o c w f t u h)
    !> Key 0 (E encodeFoldable input) -- We don't have to send these in TxBodyRaw order
    !> Key 1 (E encodeFoldable output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 update
    !> encodeKeyedStrictMaybe 7 hash

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
baseTxBodyRaw :: TxBodyRaw era
baseTxBodyRaw =
  TxBodyRaw
    { _inputsX = Set.empty,
      _outputsX = StrictSeq.empty,
      _txfeeX = Coin 0,
      _ttlX = SlotNo 0,
      _certsX = StrictSeq.empty,
      _wdrlsX = Wdrl Map.empty,
      _txUpdateX = SNothing,
      _mdHashX = SNothing
    }

instance
  (Era era, ToCBOR (PParamsUpdate era), ToCBOR (CompactForm (Value era))) =>
  ToCBOR (TxBodyRaw era)
  where
  toCBOR = encode . txSparse

-- ====================================================
-- Introduce ShelleyTxBody as a newtype around a MemoBytes

newtype ShelleyTxBody era = TxBodyConstr (MemoBytes TxBodyRaw era)
  deriving (Generic, Typeable)
  deriving newtype (SafeToHash)

type TxBody era = ShelleyTxBody era

{-# DEPRECATED TxBody "Use `ShelleyTxBody` instead" #-}

instance CC.Crypto c => EraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxBody (ShelleyEra CC.StandardCrypto) #-}

  type TxBody (ShelleyEra c) = ShelleyTxBody (ShelleyEra c)

  mkBasicTxBody = mkShelleyTxBody baseTxBodyRaw

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINE allInputsTxBodyF #-}

  inputsTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _inputsX m)
      (\txBody inputs -> txBody {_inputs = inputs})
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _outputsX m)
      (\txBody outputs -> txBody {_outputs = outputs})
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _txfeeX m)
      (\txBody fee -> txBody {_txfee = fee})
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _mdHashX m)
      (\txBody auxDataHash -> txBody {_mdHash = auxDataHash})
  {-# INLINEABLE auxDataHashTxBodyL #-}

instance CC.Crypto c => ShelleyEraTxBody (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyEra CC.StandardCrypto) #-}

  wdrlsTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _wdrlsX m)
      (\txBody wdrls -> txBody {_wdrls = wdrls})
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _ttlX m)
      (\txBody ttl -> txBody {_ttl = ttl})
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _txUpdateX m)
      (\txBody update -> txBody {_txUpdate = update})
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lens
      (\(TxBodyConstr (Memo m _)) -> _certsX m)
      (\txBody certs -> txBody {_certs = certs})
  {-# INLINEABLE certsTxBodyL #-}

deriving newtype instance
  (Era era, NoThunks (PParamsUpdate era)) => NoThunks (TxBody era)

deriving newtype instance EraTxBody era => NFData (TxBody era)

deriving instance EraTxBody era => Show (TxBody era)

deriving instance Eq (TxBody era)

deriving via Mem TxBodyRaw era instance EraTxBody era => FromCBOR (Annotator (TxBody era))

-- | Pattern for use by external users
pattern ShelleyTxBody ::
  (EraTxOut era, ToCBOR (PParamsUpdate era)) =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  ShelleyTxBody era
pattern ShelleyTxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { _inputsX = _inputs,
            _outputsX = _outputs,
            _certsX = _certs,
            _wdrlsX = _wdrls,
            _txfeeX = _txfee,
            _ttlX = _ttl,
            _txUpdateX = _txUpdate,
            _mdHashX = _mdHash
          }
        _
      )
  where
    ShelleyTxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      mkShelleyTxBody (TxBodyRaw _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash)

{-# COMPLETE ShelleyTxBody #-}

mkShelleyTxBody :: (EraTxOut era, ToCBOR (PParamsUpdate era)) => TxBodyRaw era -> ShelleyTxBody era
mkShelleyTxBody = TxBodyConstr . memoBytes . txSparse

-- =========================================

type instance MemoHashIndex TxBodyRaw = EraIndependentTxBody

instance
  (Era era, c ~ EraCrypto era) =>
  HashAnnotated (ShelleyTxBody era) EraIndependentTxBody c
  where
  hashAnnotated (TxBodyConstr mb) = mbHash mb

instance Era era => ToCBOR (TxBody era) where
  toCBOR (TxBodyConstr memo) = toCBOR memo

-- ===============================================================

witKeyHash :: WitVKey kr c -> KeyHash 'Witness c
witKeyHash = witVKeyHash
{-# DEPRECATED witKeyHash "In favor of `witVKeyHash`" #-}

wvkBytes :: WitVKey kr c -> BSL.ByteString
wvkBytes = witVKeyBytes
{-# DEPRECATED wvkBytes "In favor of `witVKeyBytes`" #-}
