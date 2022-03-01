{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Cardano.Ledger.Era (Crypto, Era (..))
import Cardano.Ledger.Hashes (DataHash, EraIndependentTxBody)
import Cardano.Ledger.SafeHash (HashAnnotated)
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)
import Data.Kind (Constraint, Type)
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Proxy (Proxy)
import GHC.Records (HasField (..))

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
  getTxOutExtras :: TxOut era -> (StrictMaybe (DataHash (Crypto era)), StrictMaybe (Script era))
  getTxOutExtras _txout = (SNothing, SNothing)

txOutView ::
  forall era.
  UsesTxOut era =>
  TxOut era ->
  (Addr (Crypto era), Value era, StrictMaybe (DataHash (Crypto era)), StrictMaybe (Script era))
txOutView txout =
  case getTxOutExtras txout of
    (mdh, mds) -> (getTxOutAddr txout, getField @"value" txout, mdh, mds)

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
