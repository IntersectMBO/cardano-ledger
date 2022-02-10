{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Constraints where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
  ( AnnotatedData,
    AuxiliaryData,
    ChainData,
    PParams,
    PParamsDelta,
    Script,
    SerialisableData,
    TxBody,
    TxOut,
    Value,
  )
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash (HashAnnotated)
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import GHC.Records

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

type UsesTxBody era =
  ( Era era,
    ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) EraIndependentTxBody (Crypto era)
  )

class
  ( Era era,
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    SerialisableData (Value era),
    DecodeNonNegative (Value era),
    EncodeMint (Value era),
    DecodeMint (Value era)
  ) =>
  UsesValue era

class
  ( Era era,
    ChainData (TxOut era),
    ToCBOR (TxOut era),
    FromCBOR (TxOut era)
  ) =>
  UsesTxOut era
  where
  makeTxOut :: Proxy era -> Addr (Crypto era) -> Value era -> TxOut era

  -- | Extract from TxOut either an address or its compact version by doing the
  -- least amount of work. Default implementation relies on the "address" field.
  getTxOutEitherAddr ::
    TxOut era ->
    Either (Addr (Crypto era)) (CompactAddr (Crypto era))
  getTxOutEitherAddr = Left . getField @"address"

  getTxOutAddr :: TxOut era -> Addr (Crypto era)
  getTxOutAddr t =
    case getTxOutEitherAddr t of
      Left a -> a
      Right ca -> decompactAddr ca

  getTxOutCompactAddr :: TxOut era -> CompactAddr (Crypto era)
  getTxOutCompactAddr t =
    case getTxOutEitherAddr t of
      Left a -> compactAddr a
      Right ca -> ca

type UsesScript era =
  ( Era era,
    Eq (Script era),
    Show (Script era),
    AnnotatedData (Script era)
  )

type UsesAuxiliary era =
  ( Era era,
    Eq (AuxiliaryData era),
    Show (AuxiliaryData era),
    ValidateAuxiliaryData era (Crypto era),
    AnnotatedData (AuxiliaryData era)
  )

class
  ( Era era,
    Eq (PParams era),
    Show (PParams era),
    SerialisableData (PParams era),
    ChainData (PParamsDelta era),
    Ord (PParamsDelta era),
    SerialisableData (PParamsDelta era)
  ) =>
  UsesPParams era
  where
  mergePPUpdates ::
    proxy era ->
    PParams era ->
    PParamsDelta era ->
    PParams era

-- | Apply 'c' to all the types transitively involved with Value when
-- (Core.Value era) is an instance of Compactible
type TransValue (c :: Type -> Constraint) era =
  ( Era era,
    Compactible (Value era),
    c (Value era)
  )
