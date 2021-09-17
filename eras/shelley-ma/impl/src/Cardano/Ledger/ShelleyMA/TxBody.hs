{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA.TxBody
  ( TxBody
      ( TxBody,
        TxBodyConstr,
        TxBody',
        adHash',
        certs',
        inputs',
        mint',
        outputs',
        txfee',
        update',
        vldt',
        wdrls'
      ),
    TxBodyRaw (..),
    FamsFrom,
    FamsTo,
    txSparse,
    bodyFields,
    StrictMaybe (..),
    fromSJust,
    ValidityInterval (..),
    initial,
    ppTxBody,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing), isSNothing)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParamsDelta, Script, Value)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppAuxiliaryDataHash,
    ppCoin,
    ppDCert,
    ppRecord,
    ppSet,
    ppStrictMaybe,
    ppStrictSeq,
    ppTxIn,
    ppTxOut,
    ppUpdate,
    ppWdrl,
  )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization (encodeFoldable)
import Cardano.Ledger.Shelley.Constraints (TransValue)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..), ppValidityInterval)
import Cardano.Ledger.Val
  ( DecodeMint (..),
    DecodeNonNegative,
    EncodeMint (..),
    Val (..),
  )
import Control.DeepSeq (NFData (..))
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    decodeSet,
    decodeStrictSeq,
    field,
    (!>),
  )
import qualified Data.Map as Map
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )

-- =====================================================
-- TxBody has three Era dependent type families
-- (Value era), (AuxiliaryData era), and (Script era) (hidden in DCert) in
-- order to make CBOR instances of things we are going to
-- have to assume some properties about these.

type FamsFrom era =
  ( Era era,
    Typeable era,
    Typeable (Script era),
    Typeable (Core.AuxiliaryData era),
    Show (Value era),
    DecodeNonNegative (Value era),
    DecodeMint (Value era),
    FromCBOR (Core.PParams era),
    FromCBOR (PParamsDelta era),
    FromCBOR (Value era),
    FromCBOR (Annotator (Script era)) -- Arises becaause DCert memoizes its bytes
  )

type FamsTo era =
  ( Era era,
    ToCBOR (Value era),
    EncodeMint (Value era),
    ToCBOR (Script era),
    ToCBOR (Core.PParams era),
    ToCBOR (PParamsDelta era),
    Typeable (Core.AuxiliaryData era)
  )

-- =======================================================

data TxBodyRaw era = TxBodyRaw
  { inputs :: !(Set (TxIn (Crypto era))),
    outputs :: !(StrictSeq (TxOut era)),
    certs :: !(StrictSeq (DCert (Crypto era))),
    wdrls :: !(Wdrl (Crypto era)),
    txfee :: !Coin,
    vldt :: !ValidityInterval, -- imported from Timelocks
    update :: !(StrictMaybe (Update era)),
    adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    mint :: !(Value era)
  }
  deriving (Typeable)

-- For each instance we try and use the weakest constraint possible
-- The surprising (Compactible (Value era))) constraint comes from the fact that TxOut
-- stores a (Value era) in a compactible form.

deriving instance
  (NFData (Value era), Era era, NFData (PParamsDelta era)) =>
  NFData (TxBodyRaw era)

deriving instance
  (TransValue Eq era, Eq (PParamsDelta era)) =>
  Eq (TxBodyRaw era)

deriving instance
  (TransValue Show era, Show (PParamsDelta era)) =>
  Show (TxBodyRaw era)

deriving instance Generic (TxBodyRaw era)

deriving instance
  (NoThunks (Value era), NoThunks (PParamsDelta era)) =>
  NoThunks (TxBodyRaw era)

instance (FamsFrom era) => FromCBOR (TxBodyRaw era) where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBodyRaw"
          initial
          bodyFields
          [(0, "inputs"), (1, "outputs"), (2, "txfee")]
      )

instance
  (FamsFrom era) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

fromSJust :: StrictMaybe a -> a
fromSJust (SJust x) = x
fromSJust SNothing = error "SNothing in fromSJust"

encodeKeyedStrictMaybe ::
  ToCBOR a =>
  Word ->
  StrictMaybe a ->
  Encode ('Closed 'Sparse) (StrictMaybe a)
encodeKeyedStrictMaybe key x = Omit isSNothing (Key key (E (toCBOR . fromSJust) x))

-- Sparse encodings of TxBodyRaw, the key values are fixed by backwarad compatibility
-- concerns as we want the Shelley era TxBody to deserialise as a Shelley-ma TxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.

txSparse ::
  (FamsTo era) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
txSparse (TxBodyRaw inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
  Keyed (\i o f topx c w u h botx forg -> TxBodyRaw i o c w f (ValidityInterval botx topx) u h forg)
    !> Key 0 (E encodeFoldable inp) -- We don't have to send these in TxBodyX order
    !> Key 1 (E encodeFoldable out) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> encodeKeyedStrictMaybe 3 top
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 up
    !> encodeKeyedStrictMaybe 7 hash
    !> encodeKeyedStrictMaybe 8 bot
    !> Omit isZero (Key 9 (E encodeMint frge))

bodyFields :: FamsFrom era => Word -> Field (TxBodyRaw era)
bodyFields 0 = field (\x tx -> tx {inputs = x}) (D (decodeSet fromCBOR))
bodyFields 1 = field (\x tx -> tx {outputs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 2 = field (\x tx -> tx {txfee = x}) From
bodyFields 3 = field (\x tx -> tx {vldt = (vldt tx) {invalidHereafter = x}}) (D (SJust <$> fromCBOR))
bodyFields 4 = field (\x tx -> tx {certs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 5 = field (\x tx -> tx {wdrls = x}) From
bodyFields 6 = field (\x tx -> tx {update = x}) (D (SJust <$> fromCBOR))
bodyFields 7 = field (\x tx -> tx {adHash = x}) (D (SJust <$> fromCBOR))
bodyFields 8 = field (\x tx -> tx {vldt = (vldt tx) {invalidBefore = x}}) (D (SJust <$> fromCBOR))
bodyFields 9 = field (\x tx -> tx {mint = x}) (D decodeMint)
bodyFields n = field (\_ t -> t) (Invalid n)

initial :: (Val (Value era)) => TxBodyRaw era
initial =
  TxBodyRaw
    empty
    (fromList [])
    (fromList [])
    (Wdrl Map.empty)
    (Coin 0)
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
    zero

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern construtor.

newtype TxBody e = TxBodyConstr (MemoBytes (TxBodyRaw e))
  deriving (Typeable)
  deriving newtype (SafeToHash)

deriving instance
  (TransValue Eq era, Eq (PParamsDelta era)) =>
  Eq (TxBody era)

deriving instance
  (TransValue Show era, Show (PParamsDelta era)) =>
  Show (TxBody era)

deriving instance Generic (TxBody era)

deriving newtype instance
  (Typeable era, NoThunks (Value era), NoThunks (PParamsDelta era)) =>
  NoThunks (TxBody era)

deriving newtype instance
  ( NFData (Value era),
    NFData (PParamsDelta era),
    Era era
  ) =>
  NFData (TxBody era)

deriving newtype instance (Typeable era) => ToCBOR (TxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    (FamsFrom era) =>
    FromCBOR (Annotator (TxBody era))

instance (c ~ Crypto era, Era era) => HashAnnotated (TxBody era) EraIndependentTxBody c

-- Make a Pattern so the newtype and the MemoBytes are hidden

pattern TxBody ::
  FamsTo era =>
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Value era ->
  TxBody era
pattern TxBody inputs outputs certs wdrls txfee vldt update adHash mint <-
  TxBodyConstr
    ( Memo
        TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}
        _
      )
  where
    TxBody inputs outputs certs wdrls txfee vldt update adHash mint =
      TxBodyConstr $
        memoBytes $
          txSparse
            TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}

{-# COMPLETE TxBody #-}

-- | This pattern is for deconstruction only but accompanied with fields and
-- projection functions.
pattern TxBody' ::
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  Value era ->
  TxBody era
pattern TxBody' {inputs', outputs', certs', wdrls', txfee', vldt', update', adHash', mint'} <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { inputs = inputs',
            outputs = outputs',
            certs = certs',
            wdrls = wdrls',
            txfee = txfee',
            vldt = vldt',
            update = update',
            adHash = adHash',
            mint = mint'
          }
        _
      )

{-# COMPLETE TxBody' #-}

-- ==================================================================
-- Promote the fields of TxBodyRaw to be fields of TxBody. Either
-- automatically or by hand. Both methods have drawbacks.

{-
instance HasField tag (TxBodyRaw e) c => HasField (tag::Symbol) (TxBody e) c where
   getField (TxBodyConstr (Memo x _)) = getField @tag x

-- The method above autmatically lifts the Hasfield instances from TxBodyRaw to TxBody
-- the problem is, if some other file imports this file, it needs to import both
-- the hidden type TxBodyRaw and its constructors like this
-- import Cardano.Ledger.ShelleyMA.TxBody(TxBodyRaw(..))     OR
-- import qualified Cardano.Ledger.ShelleyMA.TxBody as XXX
-- Both are very ugly, but at least in the second way, one doesn't need to know the name of TxBodyRaw
-- So instead we tediously write by hand explicit HasField instances for TxBody
-}

-- ========================================
-- WellFormed era (and a few other) instances

instance Crypto era ~ crypto => HasField "inputs" (TxBody era) (Set (TxIn crypto)) where
  getField (TxBodyConstr (Memo m _)) = getField @"inputs" m

instance HasField "outputs" (TxBody era) (StrictSeq (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = getField @"outputs" m

instance Crypto era ~ crypto => HasField "certs" (TxBody era) (StrictSeq (DCert crypto)) where
  getField (TxBodyConstr (Memo m _)) = getField @"certs" m

instance Crypto era ~ crypto => HasField "wdrls" (TxBody era) (Wdrl crypto) where
  getField (TxBodyConstr (Memo m _)) = getField @"wdrls" m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = getField @"txfee" m

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = getField @"vldt" m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyConstr (Memo m _)) = getField @"update" m

instance
  Crypto era ~ crypto =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash crypto))
  where
  getField (TxBodyConstr (Memo m _)) = getField @"adHash" m

instance Value era ~ value => HasField "mint" (TxBody era) value where
  getField (TxBodyConstr (Memo m _)) = getField @"mint" m

-- ============================================

ppTxBody ::
  ( Era era,
    PrettyA (Value era),
    PrettyA (PParamsDelta era)
  ) =>
  TxBody era ->
  PDoc
ppTxBody (TxBodyConstr (Memo (TxBodyRaw i o d w fee vi u m mint) _)) =
  ppRecord
    "TxBody(Mary or Allegra)"
    [ ("inputs", ppSet ppTxIn i),
      ("outputs", ppStrictSeq ppTxOut o),
      ("certificates", ppStrictSeq ppDCert d),
      ("withdrawals", ppWdrl w),
      ("txfee", ppCoin fee),
      ("vldt", ppValidityInterval vi),
      ("update", ppStrictMaybe ppUpdate u),
      ("auxDataHash", ppStrictMaybe ppAuxiliaryDataHash m),
      ("mint", prettyA mint)
    ]

instance
  ( Era era,
    PrettyA (Value era),
    PrettyA (PParamsDelta era)
  ) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody
