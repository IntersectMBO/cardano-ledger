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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxBody
  ( ConwayTxBody
      ( ConwayTxBody,
        _spendInputs,
        _collateralInputs,
        _referenceInputs,
        _outputs,
        _collateralReturn,
        _totalCollateral,
        _certs,
        _wdrls,
        _txfee,
        _vldt,
        _reqSignerHashes,
        _mint,
        _scriptIntegrityHash,
        _adHash,
        _txNetworkId,
        _govActions,
        _votes
      ),
  )
where

import Cardano.Ledger.Babbage.TxBody as BabbageTxBodyReExports
  ( AllegraEraTxBody (..),
    AlonzoEraTxBody (..),
    BabbageEraTxBody (..),
    MaryEraTxBody (..),
    ShelleyEraTxBody (..),
  )
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..))
import Cardano.Ledger.Babbage.Core (ScriptIntegrityHash)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut, fromCborTxOutWithAddr)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary (Annotator, FromCBOR (..), Sized (..), ToCBOR (..), decodeMap, decodeSet, decodeSized, decodeStrictSeq, mkSized)
import Cardano.Ledger.Binary.Coders (Decode (..), Density (..), Encode (..), Field (..), Wrapped (..), decode, encodeKeyedStrictMaybe, field, ofield, (!>))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.CompactAddress (fromCborBothAddr, fromCborRewardAcnt)
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..), GovernanceAction, Vote)
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams ()
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..), policies)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, getMemoBytesHash, memoBytes)
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (DecodeNonNegative, Val)
import Control.DeepSeq
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens, lens, to, (^.))
import NoThunks.Class (NoThunks)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))

data TxBodyRaw era = TxBodyRaw
  { spendInputs :: !(Set (TxIn (EraCrypto era))),
    collateralInputs :: !(Set (TxIn (EraCrypto era))),
    referenceInputs :: !(Set (TxIn (EraCrypto era))),
    outputs :: !(StrictSeq (Sized (BabbageTxOut era))),
    collateralReturn :: !(StrictMaybe (Sized (BabbageTxOut era))),
    totalCollateral :: !(StrictMaybe Coin),
    certs :: !(StrictSeq (ConwayDCert (EraCrypto era))),
    wdrls :: !(Wdrl (EraCrypto era)),
    txfee :: !Coin,
    vldt :: !ValidityInterval,
    reqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era))),
    mint :: !(MultiAsset (EraCrypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    adHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    txNetworkId :: !(StrictMaybe Network),
    govActions :: !(StrictSeq (GovernanceAction era)),
    votes :: !(StrictSeq (Vote era))
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Script era),
    Eq (CompactForm (Value era))
  ) =>
  Eq (TxBodyRaw era)

initialTxBodyRaw :: TxBodyRaw era
initialTxBodyRaw =
  TxBodyRaw
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

instance NoThunks (TxBodyRaw era)

instance Era era => NFData (TxBodyRaw era)

deriving instance
  ( Era era,
    Val (Value era),
    Show (Value era),
    Show (Script era)
  ) =>
  Show (TxBodyRaw era)

instance
  ( Era era,
    Val (Value era),
    DecodeNonNegative (Value era),
    FromCBOR (Annotator (Script era)),
    FromCBOR (PParamsUpdate era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        initialTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: (Word -> Field (TxBodyRaw era))
      bodyFields 0 =
        field
          (\x tx -> tx {spendInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {collateralInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 18 =
        field
          (\x tx -> tx {referenceInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {outputs = x})
          (D (decodeStrictSeq (decodeSized (fromCborTxOutWithAddr fromCborBothAddr))))
      bodyFields 16 =
        ofield
          (\x tx -> tx {collateralReturn = x})
          (D (decodeSized (fromCborTxOutWithAddr fromCborBothAddr)))
      bodyFields 17 =
        ofield
          (\x tx -> tx {totalCollateral = x})
          From
      bodyFields 2 = field (\x tx -> tx {txfee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {vldt = (vldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        field
          (\x tx -> tx {certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 =
        field
          (\x tx -> tx {wdrls = x})
          (D (Wdrl <$> decodeMap fromCborRewardAcnt fromCBOR))
      bodyFields 7 = ofield (\x tx -> tx {adHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {vldt = (vldt tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {mint = x}) (D fromCBOR)
      bodyFields 11 = ofield (\x tx -> tx {scriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = ofield (\x tx -> tx {txNetworkId = x}) From
      bodyFields 19 = field (\x tx -> tx {govActions = x}) (D (decodeStrictSeq fromCBOR))
      bodyFields 20 = field (\x tx -> tx {votes = x}) (D (decodeStrictSeq fromCBOR))
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

newtype ConwayTxBody era = TxBodyConstr (MemoBytes TxBodyRaw era)
  deriving (Generic, SafeToHash, NoThunks, ToCBOR)

deriving instance (Era era, Eq (Script era), Eq (CompactForm (Value era))) => Eq (ConwayTxBody era)

deriving newtype instance Era era => NFData (ConwayTxBody era)

deriving instance
  ( Era era,
    Val (Value era),
    Show (Value era),
    Show (Script era)
  ) =>
  Show (ConwayTxBody era)

type instance MemoHashIndex TxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era) => HashAnnotated (ConwayTxBody era) EraIndependentTxBody c where
  hashAnnotated (TxBodyConstr mb) = getMemoBytesHash mb

instance
  ( Era era,
    Val (Value era),
    DecodeNonNegative (Value era),
    FromCBOR (PParamsUpdate era),
    FromCBOR (Annotator (Script era))
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

deriving via
  (Mem TxBodyRaw era)
  instance
    ( Era era,
      Val (Value era),
      DecodeNonNegative (Value era),
      FromCBOR (PParamsUpdate era),
      FromCBOR (Annotator (Script era))
    ) =>
    FromCBOR (Annotator (ConwayTxBody era))

mkConwayTxBodyFromRaw :: ConwayEraTxBody era => TxBodyRaw era -> ConwayTxBody era
mkConwayTxBodyFromRaw = TxBodyConstr . memoBytes . encodeTxBodyRaw

lensTxBodyRaw ::
  ConwayEraTxBody era =>
  (TxBodyRaw era -> a) ->
  (TxBodyRaw era -> t -> TxBodyRaw era) ->
  Lens (ConwayTxBody era) (ConwayTxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkConwayTxBodyFromRaw $ setter txBodyRaw val)
{-# INLINEABLE lensTxBodyRaw #-}

instance CC.Crypto c => EraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance EraTxBody (ConwayEra CC.StandardCrypto) #-}

  type TxBody (ConwayEra c) = ConwayTxBody (ConwayEra c)

  mkBasicTxBody = mkConwayTxBodyFromRaw initialTxBodyRaw

  inputsTxBodyL = lensTxBodyRaw spendInputs (\txb x -> txb {spendInputs = x})
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL = lensTxBodyRaw (fmap sizedValue . outputs) (\txb x -> txb {outputs = mkSized (eraProtVerLow @(ConwayEra c)) <$> x})
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensTxBodyRaw txfee (\txb x -> txb {txfee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensTxBodyRaw adHash (\txb x -> txb {adHash = x})
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF =
    to $ \txBody ->
      Set.unions
        [ txBody ^. inputsTxBodyL,
          txBody ^. collateralInputsTxBodyL,
          txBody ^. referenceInputsTxBodyL
        ]
  {-# INLINE allInputsTxBodyF #-}

instance CC.Crypto c => ShelleyEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (ConwayEra CC.StandardCrypto) #-}

  wdrlsTxBodyL = lensTxBodyRaw wdrls (\txb x -> txb {wdrls = x})
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = notSupportedInThisEraL
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL = notSupportedInThisEraL
  {-# INLINE certsTxBodyL #-}

instance CC.Crypto c => MaryEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (ConwayEra CC.StandardCrypto) #-}

  mintTxBodyL = lensTxBodyRaw mint (\txb x -> txb {mint = x})
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue 0)

  mintedTxBodyF = to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (mint txBodyRaw)))
  {-# INLINE mintedTxBodyF #-}

instance CC.Crypto c => AllegraEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (ConwayEra CC.StandardCrypto) #-}

  vldtTxBodyL = lensTxBodyRaw vldt (\txb x -> txb {vldt = x})
  {-# INLINE vldtTxBodyL #-}

instance CC.Crypto c => AlonzoEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (ConwayEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL = lensTxBodyRaw collateralInputs (\txb x -> txb {collateralInputs = x})
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = lensTxBodyRaw reqSignerHashes (\txb x -> txb {reqSignerHashes = x})
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = lensTxBodyRaw scriptIntegrityHash (\txb x -> txb {scriptIntegrityHash = x})
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensTxBodyRaw txNetworkId (\txb x -> txb {txNetworkId = x})
  {-# INLINE networkIdTxBodyL #-}

instance CC.Crypto c => BabbageEraTxBody (ConwayEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (ConwayEra CC.StandardCrypto) #-}

  sizedOutputsTxBodyL = lensTxBodyRaw outputs (\txb x -> txb {outputs = x})
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = lensTxBodyRaw referenceInputs (\txb x -> txb {referenceInputs = x})
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = lensTxBodyRaw totalCollateral (\txb x -> txb {totalCollateral = x})
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensTxBodyRaw
      (fmap sizedValue . collateralReturn)
      (\txb x -> txb {collateralReturn = mkSized (eraProtVerLow @(ConwayEra c)) <$> x})
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL = lensTxBodyRaw collateralReturn (\txb x -> txb {collateralReturn = x})
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = to $ \txb ->
    let txOuts = txb ^. sizedOutputsTxBodyL
     in case txb ^. sizedCollateralReturnTxBodyL of
          SNothing -> txOuts
          SJust collTxOut -> txOuts |> collTxOut
  {-# INLINE allSizedOutputsTxBodyF #-}

instance CC.Crypto c => ConwayEraTxBody (ConwayEra c) where
  govActionsL = lensTxBodyRaw govActions (\txb x -> txb {govActions = x})
  votesL = lensTxBodyRaw votes (\txb x -> txb {votes = x})

pattern ConwayTxBody ::
  ConwayEraTxBody era =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Sized (BabbageTxOut era)) ->
  StrictMaybe (Sized (BabbageTxOut era)) ->
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
  StrictSeq (GovernanceAction era) ->
  StrictSeq (Vote era) ->
  ConwayTxBody era
pattern ConwayTxBody
  { _spendInputs,
    _collateralInputs,
    _referenceInputs,
    _outputs,
    _collateralReturn,
    _totalCollateral,
    _certs,
    _wdrls,
    _txfee,
    _vldt,
    _reqSignerHashes,
    _mint,
    _scriptIntegrityHash,
    _adHash,
    _txNetworkId,
    _govActions,
    _votes
  } <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { spendInputs = _spendInputs,
            collateralInputs = _collateralInputs,
            referenceInputs = _referenceInputs,
            outputs = _outputs,
            collateralReturn = _collateralReturn,
            totalCollateral = _totalCollateral,
            certs = _certs,
            wdrls = _wdrls,
            txfee = _txfee,
            vldt = _vldt,
            reqSignerHashes = _reqSignerHashes,
            mint = _mint,
            scriptIntegrityHash = _scriptIntegrityHash,
            adHash = _adHash,
            txNetworkId = _txNetworkId,
            govActions = _govActions,
            votes = _votes
          }
        _
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
        mkConwayTxBodyFromRaw $
          TxBodyRaw
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
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw TxBodyRaw {..} =
  let ValidityInterval bot top = vldt
   in Keyed
        ( \i ifee ri o cr tc f t c w b rsh mi sh ah ni ga vs ->
            TxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) rsh mi sh ah ni ga vs
        )
        !> Key 0 (To spendInputs)
        !> Omit null (Key 13 (To collateralInputs))
        !> Omit null (Key 18 (To referenceInputs))
        !> Key 1 (To outputs)
        !> encodeKeyedStrictMaybe 16 collateralReturn
        !> encodeKeyedStrictMaybe 17 totalCollateral
        !> Key 2 (To txfee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit null (Key 4 (To certs))
        !> Omit (null . unWdrl) (Key 5 (To wdrls))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To reqSignerHashes))
        !> Omit (== mempty) (Key 9 (To mint))
        !> encodeKeyedStrictMaybe 11 scriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 adHash
        !> encodeKeyedStrictMaybe 15 txNetworkId
        !> Omit null (Key 19 (To govActions))
        !> Omit null (Key 20 (To votes))
