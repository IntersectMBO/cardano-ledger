{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Constraints
  ( AnnotatedData,
    ChainData,
    SerialisableData,
    UsesTxBody,
    UsesValue,
    UsesScript,
    UsesAuxiliary,
    UsesTx,
    TransValue,
    NoThunks,
  )
where

import Cardano.Binary (Annotator, FromCBOR, ToCBOR)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
  ( AnnotatedData,
    ChainData,
    SerialisableData,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)
import Data.Kind (Constraint, Type)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Hashing
  ( EraIndependentTxBody,
    HashAnnotated (..),
  )

-- import Cardano.Ledger.Constraints(UsesTxBody,UsesValue,UsesScript,UsesTx,UsesAuxiliary,TransValue)
-- ===========================================================================
-- One constraint for each abstract type family. Think of these as the
-- minimum properties that each type family instance must support.
-- There is nothing magic about these, they were picked by trial and error,
-- and seem to be sufficient for the current state of affairs.

type UsesTxBody era =
  ( Era era,
    HashIndex (Core.TxBody era) ~ EraIndependentTxBody,
    HashAnnotated (Core.TxBody era) era,
    Eq (Core.TxBody era),
    Show (Core.TxBody era),
    FromCBOR (Annotator (Core.TxBody era)),
    ToCBOR (Core.TxBody era)
  )

{-
type UsesValue era =
  ( Era era,
    Val (Core.Value era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era)),
    DecodeNonNegative (Core.Value era),
    EncodeMint (Core.Value era),
    DecodeMint (Core.Value era),
    Torsor (Core.Value era),
    Eq (Delta (Core.Value era)),
    Eq (CompactForm (Core.Value era)),
    ToCBOR(Core.Value era),
    ToCBOR (CompactForm (Core.Value era)),
    FromCBOR(Core.Value era),
    FromCBOR (CompactForm (Core.Value era))
  )
-}

class
  ( Era era,
    Val (Core.Value era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era)),
    DecodeNonNegative (Core.Value era),
    EncodeMint (Core.Value era),
    DecodeMint (Core.Value era),
    Torsor (Core.Value era),
    Eq (Delta (Core.Value era)),
    Eq (CompactForm (Core.Value era)),
    ToCBOR (Core.Value era),
    ToCBOR (CompactForm (Core.Value era)),
    FromCBOR (Core.Value era),
    FromCBOR (CompactForm (Core.Value era))
  ) =>
  UsesValue era

type UsesTx era = (Era era, ToCBOR (Core.AuxiliaryData era))

type UsesScript era =
  ( Era era,
    Eq (Core.Script era),
    Show (Core.Script era),
    ToCBOR (Core.Script era),
    FromCBOR (Annotator (Core.Script era))
  )

type UsesAuxiliary era =
  ( Era era,
    Show (Core.AuxiliaryData era),
    ToCBOR (Core.AuxiliaryData era),
    FromCBOR (Annotator (Core.AuxiliaryData era))
  )

-- | Apply 'c' to all the types transitively involved with Value when
-- (Core.Value era) is an instance of Compactible and Torsor
type TransValue (c :: Type -> Constraint) era =
  ( Era era,
    Compactible (Core.Value era),
    Torsor (Core.Value era),
    c (Core.Value era),
    c (CompactForm (Core.Value era)),
    c (Delta (Core.Value era))
  )
