{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.TxBody
  ( MATxBody
      ( MATxBody,
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
    TxBody,
    ShelleyMAEraTxBody (..),
    TxBodyRaw (..),
    txSparse,
    bodyFields,
    StrictMaybe (..),
    fromSJust,
    ValidityInterval (..),
    initial,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core hiding (TxBody)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization (encodeFoldable)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    ShelleyEraTxBody (..),
    ShelleyTxOut (..),
    Wdrl (..),
    addrEitherShelleyTxOutL,
    valueEitherShelleyTxOutL,
  )
import Cardano.Ledger.ShelleyMA.Era (MAClass (getScriptHash, promoteMultiAsset), MaryOrAllegra (..), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeMint (..),
    EncodeMint (..),
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
    encodeKeyedStrictMaybe,
    field,
    invalidField,
    ofield,
    (!>),
  )
import qualified Data.Map.Strict as Map
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Proxy
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- =======================================================

data TxBodyRaw era = TxBodyRaw
  { inputs :: !(Set (TxIn (Crypto era))),
    outputs :: !(StrictSeq (ShelleyTxOut era)),
    certs :: !(StrictSeq (DCert (Crypto era))),
    wdrls :: !(Wdrl (Crypto era)),
    txfee :: !Coin,
    vldt :: !ValidityInterval, -- imported from Timelocks
    update :: !(StrictMaybe (Update era)),
    adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    mint :: !(MultiAsset (Crypto era))
  }

deriving instance
  (Era era, NFData (Value era), NFData (PParamsUpdate era)) =>
  NFData (TxBodyRaw era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (Value era), Eq (CompactForm (Value era))) =>
  Eq (TxBodyRaw era)

deriving instance
  (Era era, Compactible (Value era), Show (Value era), Show (PParamsUpdate era)) =>
  Show (TxBodyRaw era)

deriving instance Generic (TxBodyRaw era)

deriving instance
  (Era era, NoThunks (PParamsUpdate era), NoThunks (Value era)) =>
  NoThunks (TxBodyRaw era)

instance ShelleyMAEraTxBody era => FromCBOR (TxBodyRaw era) where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBodyRaw"
          initial
          bodyFields
          [(0, "inputs"), (1, "outputs"), (2, "txfee")]
      )

instance ShelleyMAEraTxBody era => FromCBOR (Annotator (TxBodyRaw era)) where
  fromCBOR = pure <$> fromCBOR

fromSJust :: StrictMaybe a -> a
fromSJust (SJust x) = x
fromSJust SNothing = error "SNothing in fromSJust"

-- Sparse encodings of TxBodyRaw, the key values are fixed by backwarad compatibility
-- concerns as we want the Shelley era TxBody to deserialise as a Shelley-ma TxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.

txSparse :: ShelleyMAEraTxBody era => TxBodyRaw era -> Encode ('Closed 'Sparse) (TxBodyRaw era)
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
    !> Omit (== mempty) (Key 9 (E encodeMint frge))

bodyFields :: ShelleyMAEraTxBody era => Word -> Field (TxBodyRaw era)
bodyFields 0 = field (\x tx -> tx {inputs = x}) (D (decodeSet fromCBOR))
bodyFields 1 = field (\x tx -> tx {outputs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 2 = field (\x tx -> tx {txfee = x}) From
bodyFields 3 = ofield (\x tx -> tx {vldt = (vldt tx) {invalidHereafter = x}}) From
bodyFields 4 = field (\x tx -> tx {certs = x}) (D (decodeStrictSeq fromCBOR))
bodyFields 5 = field (\x tx -> tx {wdrls = x}) From
bodyFields 6 = ofield (\x tx -> tx {update = x}) From
bodyFields 7 = ofield (\x tx -> tx {adHash = x}) From
bodyFields 8 = ofield (\x tx -> tx {vldt = (vldt tx) {invalidBefore = x}}) From
bodyFields 9 = field (\x tx -> tx {mint = x}) (D decodeMint)
bodyFields n = invalidField n

initial :: TxBodyRaw era
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
    mempty

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern construtor.

newtype MATxBody e = TxBodyConstr (MemoBytes (TxBodyRaw e))
  deriving newtype (SafeToHash)

type TxBody era = MATxBody era

{-# DEPRECATED TxBody "Use `MATxBody` instead" #-}

deriving instance Eq (MATxBody era)

deriving instance
  (Era era, Show (Value era), Compactible (Value era), Show (PParamsUpdate era)) =>
  Show (MATxBody era)

deriving instance Generic (MATxBody era)

deriving newtype instance
  (Era era, NoThunks (Value era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MATxBody era)

deriving newtype instance
  ( NFData (Value era),
    NFData (PParamsUpdate era),
    Era era
  ) =>
  NFData (MATxBody era)

deriving newtype instance Typeable era => ToCBOR (MATxBody era)

deriving via
  Mem (TxBodyRaw era)
  instance
    ShelleyMAEraTxBody era => FromCBOR (Annotator (MATxBody era))

instance (c ~ Crypto era, Era era) => HashAnnotated (MATxBody era) EraIndependentTxBody c

-- Make a Pattern so the newtype and the MemoBytes are hidden

pattern MATxBody ::
  ShelleyMAEraTxBody era =>
  Set (TxIn (Crypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  MultiAsset (Crypto era) ->
  MATxBody era
pattern MATxBody inputs outputs certs wdrls txfee vldt update adHash mint <-
  TxBodyConstr
    ( Memo
        TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}
        _
      )
  where
    MATxBody inputs outputs certs wdrls txfee vldt update adHash mint =
      mkMATxBody $
        TxBodyRaw {inputs, outputs, certs, wdrls, txfee, vldt, update, adHash, mint}

{-# COMPLETE MATxBody #-}

mkMATxBody ::
  ShelleyMAEraTxBody era =>
  TxBodyRaw era ->
  MATxBody era
mkMATxBody = TxBodyConstr . memoBytes . txSparse
{-# INLINE mkMATxBody #-}

-- | This pattern is for deconstruction only but accompanied with fields and
-- projection functions.
pattern TxBody' ::
  Set (TxIn (Crypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  MultiAsset (Crypto era) ->
  MATxBody era
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

lensTxBodyRaw ::
  ShelleyMAEraTxBody era =>
  (TxBodyRaw era -> a) ->
  (TxBodyRaw era -> t -> TxBodyRaw era) ->
  Lens (MATxBody era) (MATxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkMATxBody $ setter txBodyRaw val)
{-# INLINE lensTxBodyRaw #-}

instance MAClass ma crypto => EraTxBody (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxBody (ShelleyMAEra ma crypto) = MATxBody (ShelleyMAEra ma crypto)

  mkBasicTxBody = mkMATxBody initial

  inputsTxBodyL =
    lensTxBodyRaw inputs (\txBodyRaw inputs_ -> txBodyRaw {inputs = inputs_})
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensTxBodyRaw outputs (\txBodyRaw outputs_ -> txBodyRaw {outputs = outputs_})
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL =
    lensTxBodyRaw txfee (\txBodyRaw fee_ -> txBodyRaw {txfee = fee_})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensTxBodyRaw adHash (\txBodyRaw auxDataHash -> txBodyRaw {adHash = auxDataHash})
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINE allInputsTxBodyF #-}

  mintedTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> getScriptHash (Proxy @ma) (mint txBodyRaw))
  {-# INLINE mintedTxBodyF #-}

instance MAClass ma crypto => ShelleyEraTxBody (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  wdrlsTxBodyL =
    lensTxBodyRaw wdrls (\txBodyRaw wdrls_ -> txBodyRaw {wdrls = wdrls_})
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL =
    lensTxBodyRaw update (\txBodyRaw update_ -> txBodyRaw {update = update_})
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL =
    lensTxBodyRaw certs (\txBodyRaw certs_ -> txBodyRaw {certs = certs_})
  {-# INLINE certsTxBodyL #-}

class
  (ShelleyEraTxBody era, EncodeMint (Value era), DecodeMint (Value era)) =>
  ShelleyMAEraTxBody era
  where
  vldtTxBodyL :: Lens' (Core.TxBody era) ValidityInterval

  mintTxBodyL :: Lens' (Core.TxBody era) (MultiAsset (Crypto era))

  mintValueTxBodyF :: SimpleGetter (Core.TxBody era) (Core.Value era)

instance MAClass ma crypto => ShelleyMAEraTxBody (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  vldtTxBodyL =
    lensTxBodyRaw vldt (\txBodyRaw vldt_ -> txBodyRaw {vldt = vldt_})
  {-# INLINE vldtTxBodyL #-}

  mintTxBodyL =
    lensTxBodyRaw mint (\txBodyRaw mint_ -> txBodyRaw {mint = mint_})
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> promoteMultiAsset (Proxy @ma) (mint txBodyRaw))

instance MAClass ma crypto => EraTxOut (ShelleyMAEra ma crypto) where
  {-# SPECIALIZE instance EraTxOut (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxOut (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxOut (ShelleyMAEra ma crypto) = ShelleyTxOut (ShelleyMAEra ma crypto)

  mkBasicTxOut = ShelleyTxOut

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}
