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
{-# LANGUAGE ViewPatterns #-}
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
    StrictMaybe (..),
    ValidityInterval (..),
    scaledMinDeposit,
  )
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
    Encode (..),
    Field,
    decode,
    encode,
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
import Cardano.Ledger.MemoBytes
  ( Mem,
    MemoBytes,
    MemoHashIndex,
    Memoized (RawType),
    getMemoRawType,
    getMemoSafeHash,
    lensMemoRawType,
    mkMemoized,
  )
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
          "MATxBodyRaw"
          emptyMATxBodyRaw
          bodyFields
          [(0, "matbrInputs"), (1, "matbrOutputs"), (2, "matbrTxFee")]
      )

instance ShelleyMAEraTxBody era => FromCBOR (Annotator (MATxBodyRaw era)) where
  fromCBOR = pure <$> fromCBOR

-- Sparse encodings of ShelleyMATxBodyRaw, the key values are fixed by backward compatibility
-- concerns as we want the Shelley era TxBody to deserialise as a Shelley-ma TxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.
instance EraTxOut era => ToCBOR (MATxBodyRaw era) where
  toCBOR (MATxBodyRaw inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
    encode $
      Keyed
        ( \i o f topx c w u h botx forg ->
            MATxBodyRaw i o c w f (ValidityInterval botx topx) u h forg
        )
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

emptyMATxBodyRaw :: MATxBodyRaw era
emptyMATxBodyRaw =
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

instance Memoized MATxBody where
  type RawType MATxBody = MATxBodyRaw

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
  hashAnnotated = getMemoSafeHash

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
  ( getMemoRawType ->
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
        mkMemoized $
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

instance MAClass ma c => EraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type TxBody (ShelleyMAEra ma c) = MATxBody (ShelleyMAEra ma c)

  mkBasicTxBody = mkMemoized emptyMATxBodyRaw

  inputsTxBodyL =
    lensMemoRawType matbrInputs $ \txBodyRaw inputs -> txBodyRaw {matbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType matbrOutputs $ \txBodyRaw outputs -> txBodyRaw {matbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType matbrTxFee $ \txBodyRaw fee -> txBodyRaw {matbrTxFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType matbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {matbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

instance MAClass ma c => ShelleyEraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  wdrlsTxBodyL =
    lensMemoRawType matbrWdrls $ \txBodyRaw wdrls -> txBodyRaw {matbrWdrls = wdrls}
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType matbrUpdate $ \txBodyRaw update -> txBodyRaw {matbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType matbrCerts $ \txBodyRaw certs -> txBodyRaw {matbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

instance MAClass ma c => ShelleyMAEraTxBody (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance ShelleyMAEraTxBody (ShelleyMAEra 'Allegra StandardCrypto) #-}

  vldtTxBodyL =
    lensMemoRawType matbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {matbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

  mintTxBodyL =
    lensMemoRawType matbrMint $ \txBodyRaw mint -> txBodyRaw {matbrMint = mint}
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF = to (promoteMultiAsset (Proxy @ma) . matbrMint . getMemoRawType)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF = to (getScriptHash (Proxy @ma) . matbrMint . getMemoRawType)
  {-# INLINEABLE mintedTxBodyF #-}
