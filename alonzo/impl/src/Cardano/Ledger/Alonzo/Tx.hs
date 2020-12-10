{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Tx
  ( IsValidating (..),
    Tx (Tx, body, wits, isValidating, auxiliaryData),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, Val)
import Data.Coders
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe, maybeToStrictMaybe, strictMaybeToMaybe)

-- | Tag indicating whether non-native scripts in this transaction are expected
-- to validate. This is added by the block creator when constructing the block.
newtype IsValidating = IsValidating Bool
  deriving (Eq, NoThunks, Show)

data TxRaw era = TxRaw
  { _body :: !(TxBody era),
    _wits :: !(TxWitness era),
    _isValidating :: !IsValidating,
    _auxiliaryData :: !(StrictMaybe (Core.AuxiliaryData era))
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.Script era),
    Eq (Core.Value era),
    Eq (CompactForm (Core.Value era))
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxRaw era)

instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.Script era),
    NoThunks (Core.Value era)
  ) =>
  NoThunks (TxRaw era)

newtype Tx era = TxConstr (MemoBytes (TxRaw era))
  deriving (ToCBOR)

deriving newtype instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.Script era),
    Eq (Core.Value era),
    Eq (CompactForm (Core.Value era))
  ) =>
  Eq (Tx era)

deriving newtype instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.AuxiliaryData era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (Tx era)

deriving newtype instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.Script era),
    NoThunks (Core.Value era)
  ) =>
  NoThunks (Tx era)

pattern Tx ::
  (Era era, ToCBOR (Core.AuxiliaryData era)) =>
  TxBody era ->
  TxWitness era ->
  IsValidating ->
  StrictMaybe (Core.AuxiliaryData era) ->
  Tx era
pattern Tx {body, wits, isValidating, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { _body = body,
            _wits = wits,
            _isValidating = isValidating,
            _auxiliaryData = auxiliaryData
          }
        _
      )
  where
    Tx b w v a = TxConstr $ memoBytes (encodeTxRaw $ TxRaw b w v a)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

deriving newtype instance FromCBOR IsValidating

deriving newtype instance ToCBOR IsValidating

encodeTxRaw ::
  (Era era, ToCBOR (Core.AuxiliaryData era)) =>
  TxRaw era ->
  Encode ('Closed 'Dense) (TxRaw era)
encodeTxRaw TxRaw {_body, _wits, _isValidating, _auxiliaryData} =
  Rec TxRaw
    !> To _body
    !> To _wits
    !> To _isValidating
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) _auxiliaryData

instance
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    ToCBOR (Core.Script era),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    DecodeNonNegative (Core.Value era),
    DecodeMint (Core.Value era),
    Show (Core.Value era),
    Val (Core.Value era)
  ) =>
  FromCBOR (Annotator (TxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via
  Mem (TxRaw era)
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      FromCBOR (Annotator (Core.AuxiliaryData era)),
      ToCBOR (Core.Script era),
      Typeable (Core.Script era),
      Typeable (Core.AuxiliaryData era),
      Compactible (Core.Value era),
      DecodeNonNegative (Core.Value era),
      DecodeMint (Core.Value era),
      Show (Core.Value era),
      Val (Core.Value era)
    ) =>
    FromCBOR (Annotator (Tx era))
