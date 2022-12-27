{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxBody
  ( ConwayTxBody
      ( ConwayTxBody,
        ctbSpendInputs,
        ctbCollateralInputs,
        ctbReferenceInputs,
        ctbOutputs,
        ctbCollateralReturn,
        ctbTotalCollateral,
        ctbCerts,
        ctbWdrls,
        ctbTxfee,
        ctbVldt,
        ctbReqSignerHashes,
        ctbMint,
        ctbScriptIntegrityHash,
        ctbAdHash,
        ctbTxNetworkId,
        ctbGovActions,
        ctbVotes
      ),
  )
where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.Core (ScriptIntegrityHash)
import Cardano.Ledger.Babbage.TxBody as BabbageTxBodyReExports
  ( AllegraEraTxBody (..),
    AlonzoEraTxBody (..),
    BabbageEraTxBody (..),
    MaryEraTxBody (..),
    ShelleyEraTxBody (..),
  )
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary
  ( Annotator,
    FromCBOR (..),
    Sized (..),
    ToCBOR (..),
    mkSized,
  )
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field (..),
    Wrapped (..),
    decode,
    encode,
    encodeKeyedStrictMaybe,
    field,
    ofield,
    (!>),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
  ( ConwayEraTxBody (..),
  )
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert, transDCert)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance (GovernanceActionInfo, Vote)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value
  ( MaryValue (..),
    MultiAsset (..),
    PolicyID (..),
    policies,
  )
import Cardano.Ledger.MemoBytes
  ( Mem,
    MemoBytes (..),
    MemoHashIndex,
    Memoized (..),
    getMemoRawType,
    getMemoSafeHash,
    getterMemoRawType,
    lensMemoRawType,
    mkMemoized,
  )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (to, (^.))
import NoThunks.Class (NoThunks)

instance Memoized ConwayTxBody where
  type RawType ConwayTxBody = ConwayTxBodyRaw

data ConwayTxBodyRaw era = ConwayTxBodyRaw
  { ctbrSpendInputs :: !(Set (TxIn (EraCrypto era))),
    ctbrCollateralInputs :: !(Set (TxIn (EraCrypto era))),
    ctbrReferenceInputs :: !(Set (TxIn (EraCrypto era))),
    ctbrOutputs :: !(StrictSeq (Sized (TxOut era))),
    ctbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era))),
    ctbrTotalCollateral :: !(StrictMaybe Coin),
    ctbrCerts :: !(StrictSeq (ConwayDCert (EraCrypto era))),
    ctbrWdrls :: !(Wdrl (EraCrypto era)),
    ctbrTxfee :: !Coin,
    ctbrVldt :: !ValidityInterval,
    ctbrReqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era))),
    ctbrMint :: !(MultiAsset (EraCrypto era)),
    ctbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    ctbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    ctbrTxNetworkId :: !(StrictMaybe Network),
    ctbrGovActions :: !(StrictSeq (GovernanceActionInfo era)),
    ctbrVotes :: !(StrictSeq (Vote era))
  }
  deriving (Generic, Typeable)

deriving instance
  (Era era, Eq (TxOut era), Eq (PParamsUpdate era)) =>
  Eq (ConwayTxBodyRaw era)

instance
  (NoThunks (TxOut era), NoThunks (PParamsUpdate era)) =>
  NoThunks (ConwayTxBodyRaw era)

instance
  (Era era, NFData (TxOut era), NFData (PParamsUpdate era)) =>
  NFData (ConwayTxBodyRaw era)

deriving instance
  (Era era, Show (TxOut era), Show (PParamsUpdate era)) =>
  Show (ConwayTxBodyRaw era)

instance
  (Era era, FromCBOR (TxOut era), FromCBOR (PParamsUpdate era)) =>
  FromCBOR (ConwayTxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        basicConwayTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (ConwayTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {ctbrSpendInputs = x}) From
      bodyFields 13 = field (\x tx -> tx {ctbrCollateralInputs = x}) From
      bodyFields 18 = field (\x tx -> tx {ctbrReferenceInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {ctbrOutputs = x}) From
      bodyFields 16 = ofield (\x tx -> tx {ctbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {ctbrTotalCollateral = x}) From
      bodyFields 2 = field (\x tx -> tx {ctbrTxfee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {ctbrVldt = (ctbrVldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {ctbrCerts = x}) From
      bodyFields 5 = field (\x tx -> tx {ctbrWdrls = x}) From
      bodyFields 7 = ofield (\x tx -> tx {ctbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {ctbrVldt = (ctbrVldt tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {ctbrMint = x}) From
      bodyFields 11 = ofield (\x tx -> tx {ctbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {ctbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {ctbrTxNetworkId = x}) From
      bodyFields 19 = field (\x tx -> tx {ctbrGovActions = x}) From
      bodyFields 20 = field (\x tx -> tx {ctbrVotes = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

newtype ConwayTxBody era = TxBodyConstr (MemoBytes ConwayTxBodyRaw era)
  deriving (Generic, SafeToHash, ToCBOR)

instance
  (Era era, NoThunks (TxOut era), NoThunks (PParamsUpdate era)) =>
  NoThunks (ConwayTxBody era)

deriving instance
  (Era era, Eq (TxOut era), Eq (PParamsUpdate era)) =>
  Eq (ConwayTxBody era)

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (PParamsUpdate era)) =>
  NFData (ConwayTxBody era)

deriving instance
  (Era era, Show (TxOut era), Show (PParamsUpdate era)) =>
  Show (ConwayTxBody era)

type instance MemoHashIndex ConwayTxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era) => HashAnnotated (ConwayTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

instance
  (Era era, FromCBOR (TxOut era), FromCBOR (PParamsUpdate era)) =>
  FromCBOR (Annotator (ConwayTxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

deriving via
  (Mem ConwayTxBodyRaw era)
  instance
    (Era era, FromCBOR (TxOut era), FromCBOR (PParamsUpdate era)) =>
    FromCBOR (Annotator (ConwayTxBody era))

mkConwayTxBody :: ConwayEraTxBody era => ConwayTxBody era
mkConwayTxBody = mkMemoized basicConwayTxBodyRaw

basicConwayTxBodyRaw :: ConwayTxBodyRaw era
basicConwayTxBodyRaw =
  ConwayTxBodyRaw
    mempty
    mempty
    mempty
    StrictSeq.empty
    SNothing
    SNothing
    StrictSeq.empty
    (Wdrl mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    SNothing
    SNothing
    SNothing
    mempty
    mempty

instance Crypto c => EraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance EraTxBody (ConwayEra StandardCrypto) #-}

  type TxBody (ConwayEra c) = ConwayTxBody (ConwayEra c)

  mkBasicTxBody = mkConwayTxBody

  inputsTxBodyL = lensMemoRawType ctbrSpendInputs (\txb x -> txb {ctbrSpendInputs = x})
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType
      (fmap sizedValue . ctbrOutputs)
      (\txb x -> txb {ctbrOutputs = mkSized (eraProtVerLow @(ConwayEra c)) <$> x})
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType ctbrTxfee (\txb x -> txb {ctbrTxfee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType ctbrAuxDataHash (\txb x -> txb {ctbrAuxDataHash = x})
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF =
    to $ \txBody ->
      Set.unions
        [ txBody ^. inputsTxBodyL,
          txBody ^. collateralInputsTxBodyL,
          txBody ^. referenceInputsTxBodyL
        ]
  {-# INLINE allInputsTxBodyF #-}

instance Crypto c => ShelleyEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ConwayEra StandardCrypto) #-}

  wdrlsTxBodyL = lensMemoRawType ctbrWdrls (\txb x -> txb {ctbrWdrls = x})
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = notSupportedInThisEraL
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL = notSupportedInThisEraL
  {-# INLINE certsTxBodyL #-}

  certsTxBodyG = getterMemoRawType (fmap transDCert . ctbrCerts)

instance Crypto c => AllegraEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (ConwayEra StandardCrypto) #-}

  vldtTxBodyL = lensMemoRawType ctbrVldt (\txb x -> txb {ctbrVldt = x})
  {-# INLINE vldtTxBodyL #-}

instance Crypto c => MaryEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (ConwayEra StandardCrypto) #-}

  mintTxBodyL = lensMemoRawType ctbrMint (\txb x -> txb {ctbrMint = x})
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue 0)

  mintedTxBodyF = to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (ctbrMint txBodyRaw)))
  {-# INLINE mintedTxBodyF #-}

instance Crypto c => AlonzoEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (ConwayEra StandardCrypto) #-}

  collateralInputsTxBodyL = lensMemoRawType ctbrCollateralInputs (\txb x -> txb {ctbrCollateralInputs = x})
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = lensMemoRawType ctbrReqSignerHashes (\txb x -> txb {ctbrReqSignerHashes = x})
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = lensMemoRawType ctbrScriptIntegrityHash (\txb x -> txb {ctbrScriptIntegrityHash = x})
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensMemoRawType ctbrTxNetworkId (\txb x -> txb {ctbrTxNetworkId = x})
  {-# INLINE networkIdTxBodyL #-}

instance Crypto c => BabbageEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (ConwayEra StandardCrypto) #-}

  sizedOutputsTxBodyL = lensMemoRawType ctbrOutputs (\txb x -> txb {ctbrOutputs = x})
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = lensMemoRawType ctbrReferenceInputs (\txb x -> txb {ctbrReferenceInputs = x})
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = lensMemoRawType ctbrTotalCollateral (\txb x -> txb {ctbrTotalCollateral = x})
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensMemoRawType
      (fmap sizedValue . ctbrCollateralReturn)
      (\txb x -> txb {ctbrCollateralReturn = mkSized (eraProtVerLow @(ConwayEra c)) <$> x})
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL = lensMemoRawType ctbrCollateralReturn (\txb x -> txb {ctbrCollateralReturn = x})
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = to $ \txb ->
    let txOuts = txb ^. sizedOutputsTxBodyL
     in case txb ^. sizedCollateralReturnTxBodyL of
          SNothing -> txOuts
          SJust collTxOut -> txOuts |> collTxOut
  {-# INLINE allSizedOutputsTxBodyF #-}

instance Crypto c => ConwayEraTxBody (ConwayEra c) where
  govActionsTxBodyL = lensMemoRawType ctbrGovActions (\txb x -> txb {ctbrGovActions = x})
  votesTxBodyL = lensMemoRawType ctbrVotes (\txb x -> txb {ctbrVotes = x})
  conwayCertsTxBodyL = lensMemoRawType ctbrCerts (\txb x -> txb {ctbrCerts = x})

pattern ConwayTxBody ::
  ConwayEraTxBody era =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Sized (TxOut era)) ->
  StrictMaybe (Sized (TxOut era)) ->
  StrictMaybe Coin ->
  StrictSeq (ConwayDCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  StrictMaybe (ScriptIntegrityHash (EraCrypto era)) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  StrictMaybe Network ->
  StrictSeq (GovernanceActionInfo era) ->
  StrictSeq (Vote era) ->
  ConwayTxBody era
pattern ConwayTxBody
  { ctbSpendInputs,
    ctbCollateralInputs,
    ctbReferenceInputs,
    ctbOutputs,
    ctbCollateralReturn,
    ctbTotalCollateral,
    ctbCerts,
    ctbWdrls,
    ctbTxfee,
    ctbVldt,
    ctbReqSignerHashes,
    ctbMint,
    ctbScriptIntegrityHash,
    ctbAdHash,
    ctbTxNetworkId,
    ctbGovActions,
    ctbVotes
  } <-
  ( getMemoRawType ->
      ConwayTxBodyRaw
        { ctbrSpendInputs = ctbSpendInputs,
          ctbrCollateralInputs = ctbCollateralInputs,
          ctbrReferenceInputs = ctbReferenceInputs,
          ctbrOutputs = ctbOutputs,
          ctbrCollateralReturn = ctbCollateralReturn,
          ctbrTotalCollateral = ctbTotalCollateral,
          ctbrCerts = ctbCerts,
          ctbrWdrls = ctbWdrls,
          ctbrTxfee = ctbTxfee,
          ctbrVldt = ctbVldt,
          ctbrReqSignerHashes = ctbReqSignerHashes,
          ctbrMint = ctbMint,
          ctbrScriptIntegrityHash = ctbScriptIntegrityHash,
          ctbrAuxDataHash = ctbAdHash,
          ctbrTxNetworkId = ctbTxNetworkId,
          ctbrGovActions = ctbGovActions,
          ctbrVotes = ctbVotes
        }
    )
  where
    ConwayTxBody
      inputsX
      collateralX
      referenceInputsX
      outputsX
      collateralReturnX
      totalCollateralX
      certsX
      wdrlsX
      txfeeX
      vldtX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      govActions
      votes =
        mkMemoized $
          ConwayTxBodyRaw
            inputsX
            collateralX
            referenceInputsX
            outputsX
            collateralReturnX
            totalCollateralX
            certsX
            wdrlsX
            txfeeX
            vldtX
            reqSignerHashesX
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            govActions
            votes

{-# COMPLETE ConwayTxBody #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxBodyRaw ::
  ConwayEraTxBody era =>
  ConwayTxBodyRaw era ->
  Encode ('Closed 'Sparse) (ConwayTxBodyRaw era)
encodeTxBodyRaw ConwayTxBodyRaw {..} =
  let ValidityInterval bot top = ctbrVldt
   in Keyed
        ( \i ifee ri o cr tc f t c w b rsh mi sh ah ni ga vs ->
            ConwayTxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) rsh mi sh ah ni ga vs
        )
        !> Key 0 (To ctbrSpendInputs)
        !> Omit null (Key 13 (To ctbrCollateralInputs))
        !> Omit null (Key 18 (To ctbrReferenceInputs))
        !> Key 1 (To ctbrOutputs)
        !> encodeKeyedStrictMaybe 16 ctbrCollateralReturn
        !> encodeKeyedStrictMaybe 17 ctbrTotalCollateral
        !> Key 2 (To ctbrTxfee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit null (Key 4 (To ctbrCerts))
        !> Omit (null . unWdrl) (Key 5 (To ctbrWdrls))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To ctbrReqSignerHashes))
        !> Omit (== mempty) (Key 9 (To ctbrMint))
        !> encodeKeyedStrictMaybe 11 ctbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 ctbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 ctbrTxNetworkId
        !> Omit null (Key 19 (To ctbrGovActions))
        !> Omit null (Key 20 (To ctbrVotes))

instance ConwayEraTxBody era => ToCBOR (ConwayTxBodyRaw era) where
  toCBOR = encode . encodeTxBodyRaw
