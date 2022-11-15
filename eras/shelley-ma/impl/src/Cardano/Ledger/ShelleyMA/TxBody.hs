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
        matbAuxDataHash,
        matbCerts,
        matbInputs,
        matbMint,
        matbOutputs,
        matbTxFee,
        matbUpdate,
        matbValidityInterval,
        matbWdrls
      ),
    TxBody,
    ShelleyMAEraTxBody (..),
    MATxBodyRaw (..),
    txSparse,
    bodyFields,
    StrictMaybe (..),
    fromSJust,
    ValidityInterval (..),
    initial,
    scaledMinDeposit,
  )
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    encodeKeyedStrictMaybe,
    field,
    invalidField,
    ofield,
    (!>),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core hiding (TxBody)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes)
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    ShelleyEraTxBody (..),
    ShelleyTxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.ShelleyMA.Core (ShelleyMAEraTxBody (..))
import Cardano.Ledger.ShelleyMA.Era
  ( MAClass (getScriptHash, promoteMultiAsset),
    MaryOrAllegra (..),
    ShelleyMAEra,
  )
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxOut
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeMint (..),
    EncodeMint (..),
  )
import Control.DeepSeq (NFData (..))
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- =======================================================

data MATxBodyRaw era = MATxBodyRaw
  { matbrInputs :: !(Set (TxIn (EraCrypto era))),
    matbrOutputs :: !(StrictSeq (ShelleyTxOut era)),
    matbrCerts :: !(StrictSeq (DCert (EraCrypto era))),
    matbrWdrls :: !(Wdrl (EraCrypto era)),
    matbrTxFee :: !Coin,
    matbrValidityInterval :: !ValidityInterval, -- imported from Timelocks
    matbrUpdate :: !(StrictMaybe (Update era)),
    matbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    matbrMint :: !(MultiAsset (EraCrypto era))
  }

deriving instance
  (Era era, NFData (Value era), NFData (PParamsUpdate era)) =>
  NFData (MATxBodyRaw era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (Value era), Eq (CompactForm (Value era))) =>
  Eq (MATxBodyRaw era)

deriving instance
  (Era era, Compactible (Value era), Show (Value era), Show (PParamsUpdate era)) =>
  Show (MATxBodyRaw era)

deriving instance Generic (MATxBodyRaw era)

deriving instance
  (Era era, NoThunks (PParamsUpdate era), NoThunks (Value era)) =>
  NoThunks (MATxBodyRaw era)

instance ShelleyMAEraTxBody era => FromCBOR (MATxBodyRaw era) where
  fromCBOR =
    decode
      ( SparseKeyed
          "ShelleyMATxBodyRaw"
          initial
          bodyFields
          [(0, "matbrInputs"), (1, "matbrOutputs"), (2, "matbrTxFee")]
      )

instance ShelleyMAEraTxBody era => FromCBOR (Annotator (MATxBodyRaw era)) where
  fromCBOR = pure <$> fromCBOR

fromSJust :: StrictMaybe a -> a
fromSJust (SJust x) = x
fromSJust SNothing = error "SNothing in fromSJust"

-- Sparse encodings of ShelleyMATxBodyRaw, the key values are fixed by backwarad compatibility
-- concerns as we want the Shelley era TxBody to deserialise as a Shelley-ma TxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.

txSparse ::
  EraTxOut era =>
  MATxBodyRaw era ->
  Encode ('Closed 'Sparse) (MATxBodyRaw era)
txSparse (MATxBodyRaw inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
  Keyed (\i o f topx c w u h botx forg -> MATxBodyRaw i o c w f (ValidityInterval botx topx) u h forg)
    !> Key 0 (To inp) -- We don't have to send these in TxBodyX order
    !> Key 1 (To out) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> encodeKeyedStrictMaybe 3 top
    !> Omit null (Key 4 (To cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 up
    !> encodeKeyedStrictMaybe 7 hash
    !> encodeKeyedStrictMaybe 8 bot
    !> Omit (== mempty) (Key 9 (E encodeMint frge))

bodyFields :: EraTxOut era => Word -> Field (MATxBodyRaw era)
bodyFields 0 = field (\x tx -> tx {matbrInputs = x}) From
bodyFields 1 = field (\x tx -> tx {matbrOutputs = x}) From
bodyFields 2 = field (\x tx -> tx {matbrTxFee = x}) From
bodyFields 3 =
  ofield
    ( \x tx ->
        tx
          { matbrValidityInterval =
              (matbrValidityInterval tx) {invalidHereafter = x}
          }
    )
    From
bodyFields 4 = field (\x tx -> tx {matbrCerts = x}) From
bodyFields 5 = field (\x tx -> tx {matbrWdrls = x}) From
bodyFields 6 = ofield (\x tx -> tx {matbrUpdate = x}) From
bodyFields 7 = ofield (\x tx -> tx {matbrAuxDataHash = x}) From
bodyFields 8 =
  ofield
    ( \x tx ->
        tx
          { matbrValidityInterval =
              (matbrValidityInterval tx) {invalidBefore = x}
          }
    )
    From
bodyFields 9 = field (\x tx -> tx {matbrMint = x}) (D decodeMint)
bodyFields n = invalidField n

initial :: MATxBodyRaw era
initial =
  MATxBodyRaw
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

newtype MATxBody e = TxBodyConstr (MemoBytes MATxBodyRaw e)
  deriving newtype (SafeToHash)

type TxBody era = MATxBody era

{-# DEPRECATED TxBody "Use `MATxBody` instead" #-}

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (Value era), Eq (CompactForm (Value era))) =>
  Eq (MATxBody era)

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
  Mem MATxBodyRaw era
  instance
    ShelleyMAEraTxBody era => FromCBOR (Annotator (MATxBody era))

type instance MemoHashIndex MATxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era, Era era) => HashAnnotated (MATxBody era) EraIndependentTxBody c where
  hashAnnotated (TxBodyConstr mb) = mbHash mb

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern MATxBody ::
  EraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (ShelleyTxOut era) ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  MATxBody era
pattern MATxBody
  { matbInputs,
    matbOutputs,
    matbCerts,
    matbWdrls,
    matbTxFee,
    matbValidityInterval,
    matbUpdate,
    matbAuxDataHash,
    matbMint
  } <-
  TxBodyConstr
    ( Memo
        MATxBodyRaw
          { matbrInputs = matbInputs,
            matbrOutputs = matbOutputs,
            matbrCerts = matbCerts,
            matbrWdrls = matbWdrls,
            matbrTxFee = matbTxFee,
            matbrValidityInterval = matbValidityInterval,
            matbrUpdate = matbUpdate,
            matbrAuxDataHash = matbAuxDataHash,
            matbrMint = matbMint
          }
        _
      )
  where
    MATxBody
      inputs
      outputs
      certs
      wdrls
      txFee
      validityInterval
      update
      auxDataHash
      mint =
        mkMATxBody $
          MATxBodyRaw
            { matbrInputs = inputs,
              matbrOutputs = outputs,
              matbrCerts = certs,
              matbrWdrls = wdrls,
              matbrTxFee = txFee,
              matbrValidityInterval = validityInterval,
              matbrUpdate = update,
              matbrAuxDataHash = auxDataHash,
              matbrMint = mint
            }

{-# COMPLETE MATxBody #-}

mkMATxBody ::
  EraTxOut era =>
  MATxBodyRaw era ->
  MATxBody era
mkMATxBody = TxBodyConstr . memoBytes . txSparse
{-# INLINEABLE mkMATxBody #-}

lensTxBodyRaw ::
  ShelleyMAEraTxBody era =>
  (MATxBodyRaw era -> a) ->
  (MATxBodyRaw era -> t -> MATxBodyRaw era) ->
  Lens (MATxBody era) (MATxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkMATxBody $ setter txBodyRaw val)
{-# INLINEABLE lensTxBodyRaw #-}

instance MAClass ma c => EraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxBody (ShelleyMAEra ma c) = MATxBody (ShelleyMAEra ma c)

  mkBasicTxBody = mkMATxBody initial

  inputsTxBodyL =
    lensTxBodyRaw matbrInputs (\txBodyRaw inputs_ -> txBodyRaw {matbrInputs = inputs_})
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensTxBodyRaw matbrOutputs (\txBodyRaw outputs_ -> txBodyRaw {matbrOutputs = outputs_})
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensTxBodyRaw matbrTxFee (\txBodyRaw fee_ -> txBodyRaw {matbrTxFee = fee_})
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensTxBodyRaw matbrAuxDataHash (\txBodyRaw auxDataHash -> txBodyRaw {matbrAuxDataHash = auxDataHash})
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

instance MAClass ma c => ShelleyEraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  wdrlsTxBodyL =
    lensTxBodyRaw matbrWdrls (\txBodyRaw wdrls_ -> txBodyRaw {matbrWdrls = wdrls_})
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensTxBodyRaw matbrUpdate (\txBodyRaw update_ -> txBodyRaw {matbrUpdate = update_})
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lensTxBodyRaw matbrCerts (\txBodyRaw certs_ -> txBodyRaw {matbrCerts = certs_})
  {-# INLINEABLE certsTxBodyL #-}

instance MAClass ma c => ShelleyMAEraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  vldtTxBodyL =
    lensTxBodyRaw matbrValidityInterval (\txBodyRaw vldt_ -> txBodyRaw {matbrValidityInterval = vldt_})
  {-# INLINEABLE vldtTxBodyL #-}

  mintTxBodyL =
    lensTxBodyRaw matbrMint (\txBodyRaw mint_ -> txBodyRaw {matbrMint = mint_})
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> promoteMultiAsset (Proxy @ma) (matbrMint txBodyRaw))
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> getScriptHash (Proxy @ma) (matbrMint txBodyRaw))
  {-# INLINEABLE mintedTxBodyF #-}
