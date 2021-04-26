{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Era-generic transactions.
--
-- We make the assertion that the transaction format is independent between
-- eras, varying only in its components. Thus a transaction consists of a body,
-- witnesses, and any auxiliary data.
module Cardano.Ledger.Tx
  ( Tx (Tx, body, wits, auxiliaryData),
    unsafeConstructTxWithBytes,
  )
where

import Cardano.Binary
  ( Annotator (Annotator),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.SafeHash (SafeToHash)
import qualified Data.ByteString.Short as SBS
import Data.Coders
  ( Decode (Ann, D, From, RecD),
    Density (Dense),
    Encode (E, Rec, To),
    Wrapped (Closed),
    decode,
    decodeNullMaybe,
    encodeNullMaybe,
    (!>),
    (<*!),
  )
import Data.Maybe.Strict (StrictMaybe, maybeToStrictMaybe, strictMaybeToMaybe)
import Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)

data TxRaw era = TxRaw
  { _body :: !(Core.TxBody era),
    _wits :: !(Core.Witnesses era),
    _auxiliaryData :: !(StrictMaybe (Core.AuxiliaryData era))
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.TxBody era),
    Eq (Core.Witnesses era)
  ) =>
  Eq (TxRaw era)

deriving instance
  ( Era era,
    Show (Core.AuxiliaryData era),
    Show (Core.TxBody era),
    Show (Core.Witnesses era)
  ) =>
  Show (TxRaw era)

instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.TxBody era),
    NoThunks (Core.Witnesses era)
  ) =>
  NoThunks (TxRaw era)

newtype Tx era = TxConstr (MemoBytes (TxRaw era))
  deriving newtype (SafeToHash, ToCBOR)

deriving newtype instance
  ( Era era,
    Eq (Core.AuxiliaryData era),
    Eq (Core.TxBody era),
    Eq (Core.Witnesses era)
  ) =>
  Eq (Tx era)

deriving newtype instance
  ( Era era,
    Show (Core.AuxiliaryData era),
    Show (Core.TxBody era),
    Show (Core.Witnesses era)
  ) =>
  Show (Tx era)

deriving newtype instance
  ( Era era,
    NoThunks (Core.AuxiliaryData era),
    NoThunks (Core.TxBody era),
    NoThunks (Core.Witnesses era)
  ) =>
  NoThunks (Tx era)

pattern Tx ::
  ( Era era,
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.Witnesses era)
  ) =>
  Core.TxBody era ->
  Core.Witnesses era ->
  StrictMaybe (Core.AuxiliaryData era) ->
  Tx era
pattern Tx {body, wits, auxiliaryData} <-
  TxConstr
    ( Memo
        TxRaw
          { _body = body,
            _wits = wits,
            _auxiliaryData = auxiliaryData
          }
        _
      )
  where
    Tx b w a = TxConstr $ memoBytes (encodeTxRaw $ TxRaw b w a)

{-# COMPLETE Tx #-}

--------------------------------------------------------------------------------
-- Field accessors
--------------------------------------------------------------------------------

instance
  aux ~ Core.AuxiliaryData era =>
  HasField "auxiliaryData" (Tx era) (StrictMaybe aux)
  where
  getField (TxConstr (Memo (TxRaw _ _ a) _)) = a

instance (body ~ Core.TxBody era) => HasField "body" (Tx era) body where
  getField (TxConstr (Memo (TxRaw b _ _) _)) = b

instance
  (wits ~ Core.Witnesses era) =>
  HasField "wits" (Tx era) wits
  where
  getField (TxConstr (Memo (TxRaw _ w _) _)) = w

instance HasField "txsize" (Tx era) Integer where
  getField (TxConstr (Memo _ bytes)) = fromIntegral $ SBS.length bytes

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxRaw ::
  ( ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.Witnesses era)
  ) =>
  TxRaw era ->
  Encode ('Closed 'Dense) (TxRaw era)
encodeTxRaw TxRaw {_body, _wits, _auxiliaryData} =
  Rec TxRaw
    !> To _body
    !> To _wits
    !> E (encodeNullMaybe toCBOR . strictMaybeToMaybe) _auxiliaryData

instance
  ( Era era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Witnesses era))
  ) =>
  FromCBOR (Annotator (TxRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe fromCBOR
          )

deriving via
  Mem (TxRaw era)
  instance
    ( Era era,
      FromCBOR (Annotator (Core.TxBody era)),
      FromCBOR (Annotator (Core.AuxiliaryData era)),
      FromCBOR (Annotator (Core.Witnesses era))
    ) =>
    FromCBOR (Annotator (Tx era))

-- | Construct a Tx containing the explicit serialised bytes.
--
--   This function is marked as unsafe since it makes no guarantee that the
--   represented bytes are indeed the correct serialisation of the transaction.
--   Thus, when calling this function, the caller is responsible for making this
--   guarantee.
--
--   The only intended use case for this is for segregated witness.
unsafeConstructTxWithBytes ::
  Core.TxBody era ->
  Core.Witnesses era ->
  StrictMaybe (Core.AuxiliaryData era) ->
  SBS.ShortByteString ->
  Tx era
unsafeConstructTxWithBytes b w a bytes = TxConstr (Memo (TxRaw b w a) bytes)
