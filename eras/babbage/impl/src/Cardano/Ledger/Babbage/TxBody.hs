{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxBody
  ( BabbageTxOut
      ( BabbageTxOut,
        TxOutCompact,
        TxOutCompactDH,
        TxOutCompactDatum,
        TxOutCompactRefScript
      ),
    AlonzoEraTxOut (..),
    BabbageEraTxOut (..),
    addrEitherBabbageTxOutL,
    valueEitherBabbageTxOutL,
    dataHashBabbageTxOutL,
    dataBabbageTxOutL,
    datumBabbageTxOutL,
    referenceScriptBabbageTxOutL,
    allSizedOutputsBabbageTxBodyF,
    getDatumBabbageTxOut,
    babbageMinUTxOValue,
    BabbageTxBody
      ( BabbageTxBody,
        btbInputs,
        btbCollateral,
        btbReferenceInputs,
        btbOutputs,
        btbCollateralReturn,
        btbTotalCollateral,
        btbCerts,
        btbWdrls,
        btbTxFee,
        btbValidityInterval,
        btbUpdate,
        btbReqSignerHashes,
        btbMint,
        btbScriptIntegrityHash,
        btbAuxDataHash,
        btbTxNetworkId
      ),
    mkBabbageTxBody,
    inputsBabbageTxBodyL,
    outputsBabbageTxBodyL,
    feeBabbageTxBodyL,
    auxDataHashBabbageTxBodyL,
    allInputsBabbageTxBodyF,
    mintedBabbageTxBodyF,
    mintValueBabbageTxBodyF,
    wdrlsBabbbageTxBodyL,
    notSupportedInThisEraL,
    updateBabbageTxBodyL,
    certsBabbageTxBodyL,
    vldtBabbageTxBodyL,
    mintBabbageTxBodyL,
    collateralInputsBabbageTxBodyL,
    reqSignerHashesBabbageTxBodyL,
    scriptIntegrityHashBabbageTxBodyL,
    networkIdBabbageTxBodyL,
    sizedOutputsBabbageTxBodyL,
    referenceInputsBabbageTxBodyL,
    totalCollateralBabbageTxBodyL,
    collateralReturnBabbageTxBodyL,
    sizedCollateralReturnBabbageTxBodyL,
    BabbageEraTxBody (..),
    module AlonzoTxBodyReExports,
    Datum (..),
    spendInputs',
    collateralInputs',
    referenceInputs',
    outputs',
    collateralReturn',
    totalCollateral',
    certs',
    wdrls',
    txfee',
    vldt',
    update',
    reqSignerHashes',
    mint',
    scriptIntegrityHash',
    adHash',
    txnetworkid',
    getEitherAddrBabbageTxOut,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
    txOutData,
    txOutDataHash,
    txOutScript,

    -- * Deprecated
    TxOut,
    TxBody,
  )
where

import Cardano.Ledger.Alonzo.Data
  ( AuxiliaryDataHash (..),
    Datum (..),
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxOut (..),
  )
import Cardano.Ledger.Alonzo.TxBody as AlonzoTxBodyReExports
  ( AlonzoEraTxBody (..),
    ShelleyEraTxBody (..),
    ShelleyMAEraTxBody (..),
  )
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody (..))
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Cardano.Ledger.Binary
  ( Annotator (..),
    FromCBOR (..),
    Sized (..),
    ToCBOR (..),
    decodeMap,
    decodeSized,
    decodeStrictSeq,
    mkSized,
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress
  ( fromCborBothAddr,
    fromCborRewardAcnt,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core (TxBody, TxOut)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset, policies, policyID)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes)
import Cardano.Ledger.SafeHash
  ( HashAnnotated (..),
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    Val (..),
    decodeMint,
    encodeMint,
  )
import Control.DeepSeq (NFData)
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Prelude hiding (lookup)

-- ======================================

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

data BabbageTxBodyRaw era = BabbageTxBodyRaw
  { btbrSpendInputs :: !(Set (TxIn (EraCrypto era))),
    btbrCollateralInputs :: !(Set (TxIn (EraCrypto era))),
    btbrReferenceInputs :: !(Set (TxIn (EraCrypto era))),
    btbrOutputs :: !(StrictSeq (Sized (BabbageTxOut era))),
    btbrCollateralReturn :: !(StrictMaybe (Sized (BabbageTxOut era))),
    btbrTotalCollateral :: !(StrictMaybe Coin),
    btbrCerts :: !(StrictSeq (DCert (EraCrypto era))),
    btbrWdrls :: !(Wdrl (EraCrypto era)),
    btbrTxFee :: !Coin,
    btbrValidityInterval :: !ValidityInterval,
    btbrUpdate :: !(StrictMaybe (Update era)),
    btbrReqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era))),
    btbrMint :: !(MultiAsset (EraCrypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    btbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    btbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    btbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

type instance MemoHashIndex BabbageTxBodyRaw = EraIndependentTxBody

deriving instance
  (Era era, Eq (Script era), Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (BabbageTxBodyRaw era)

instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (BabbageTxBodyRaw era)

instance (CC.Crypto (EraCrypto era), NFData (PParamsUpdate era)) => NFData (BabbageTxBodyRaw era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBodyRaw era)

newtype BabbageTxBody era = TxBodyConstr (MemoBytes BabbageTxBodyRaw era)
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  (CC.Crypto (EraCrypto era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBody era)

lensTxBodyRaw ::
  BabbageEraTxBody era =>
  (BabbageTxBodyRaw era -> a) ->
  (BabbageTxBodyRaw era -> t -> BabbageTxBodyRaw era) ->
  Lens (BabbageTxBody era) (BabbageTxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkBabbageTxBodyFromRaw $ setter txBodyRaw val)
{-# INLINEABLE lensTxBodyRaw #-}

inputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
inputsBabbageTxBodyL =
  lensTxBodyRaw btbrSpendInputs (\txBodyRaw inputs_ -> txBodyRaw {btbrSpendInputs = inputs_})
{-# INLINEABLE inputsBabbageTxBodyL #-}

outputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (BabbageTxOut era))
outputsBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . btbrOutputs)
    (\txBodyRaw outputs_ -> txBodyRaw {btbrOutputs = mkSized (eraProtVerLow @era) <$> outputs_})
{-# INLINEABLE outputsBabbageTxBodyL #-}

feeBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) Coin
feeBabbageTxBodyL =
  lensTxBodyRaw btbrTxFee (\txBodyRaw fee_ -> txBodyRaw {btbrTxFee = fee_})
{-# INLINEABLE feeBabbageTxBodyL #-}

auxDataHashBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
auxDataHashBabbageTxBodyL =
  lensTxBodyRaw btbrAuxDataHash (\txBodyRaw auxDataHash -> txBodyRaw {btbrAuxDataHash = auxDataHash})
{-# INLINEABLE auxDataHashBabbageTxBodyL #-}

allInputsBabbageTxBodyF ::
  BabbageEraTxBody era => SimpleGetter (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
allInputsBabbageTxBodyF =
  to $ \txBody ->
    (txBody ^. inputsBabbageTxBodyL)
      `Set.union` (txBody ^. collateralInputsBabbageTxBodyL)
      `Set.union` (txBody ^. referenceInputsBabbageTxBodyL)
{-# INLINEABLE allInputsBabbageTxBodyF #-}

mintedBabbageTxBodyF :: Era era => SimpleGetter (BabbageTxBody era) (Set (ScriptHash (EraCrypto era)))
mintedBabbageTxBodyF =
  to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (btbrMint txBodyRaw)))
{-# INLINEABLE mintedBabbageTxBodyF #-}

wdrlsBabbbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (Wdrl (EraCrypto era))
wdrlsBabbbageTxBodyL =
  lensTxBodyRaw btbrWdrls (\txBodyRaw wdrls_ -> txBodyRaw {btbrWdrls = wdrls_})
{-# INLINEABLE wdrlsBabbbageTxBodyL #-}

updateBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Update era))
updateBabbageTxBodyL =
  lensTxBodyRaw btbrUpdate (\txBodyRaw update_ -> txBodyRaw {btbrUpdate = update_})
{-# INLINEABLE updateBabbageTxBodyL #-}

certsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (DCert (EraCrypto era)))
certsBabbageTxBodyL =
  lensTxBodyRaw btbrCerts (\txBodyRaw certs_ -> txBodyRaw {btbrCerts = certs_})
{-# INLINEABLE certsBabbageTxBodyL #-}

vldtBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) ValidityInterval
vldtBabbageTxBodyL =
  lensTxBodyRaw btbrValidityInterval (\txBodyRaw vldt_ -> txBodyRaw {btbrValidityInterval = vldt_})
{-# INLINEABLE vldtBabbageTxBodyL #-}

mintBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (MultiAsset (EraCrypto era))
mintBabbageTxBodyL =
  lensTxBodyRaw btbrMint (\txBodyRaw mint_ -> txBodyRaw {btbrMint = mint_})
{-# INLINEABLE mintBabbageTxBodyL #-}

mintValueBabbageTxBodyF ::
  (BabbageEraTxBody era, Value era ~ MaryValue (EraCrypto era)) =>
  SimpleGetter (BabbageTxBody era) (Value era)
mintValueBabbageTxBodyF = mintBabbageTxBodyL . to (MaryValue 0)
{-# INLINEABLE mintValueBabbageTxBodyF #-}

collateralInputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
collateralInputsBabbageTxBodyL =
  lensTxBodyRaw
    btbrCollateralInputs
    (\txBodyRaw collateral_ -> txBodyRaw {btbrCollateralInputs = collateral_})
{-# INLINEABLE collateralInputsBabbageTxBodyL #-}

reqSignerHashesBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (KeyHash 'Witness (EraCrypto era)))
reqSignerHashesBabbageTxBodyL =
  lensTxBodyRaw
    btbrReqSignerHashes
    (\txBodyRaw reqSignerHashes_ -> txBodyRaw {btbrReqSignerHashes = reqSignerHashes_})
{-# INLINEABLE reqSignerHashesBabbageTxBodyL #-}

scriptIntegrityHashBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
scriptIntegrityHashBabbageTxBodyL =
  lensTxBodyRaw
    btbrScriptIntegrityHash
    (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {btbrScriptIntegrityHash = scriptIntegrityHash_})
{-# INLINEABLE scriptIntegrityHashBabbageTxBodyL #-}

networkIdBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Network)
networkIdBabbageTxBodyL =
  lensTxBodyRaw btbrTxNetworkId (\txBodyRaw networkId -> txBodyRaw {btbrTxNetworkId = networkId})
{-# INLINEABLE networkIdBabbageTxBodyL #-}

sizedOutputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictSeq (Sized (BabbageTxOut era)))
sizedOutputsBabbageTxBodyL =
  lensTxBodyRaw btbrOutputs (\txBodyRaw outputs_ -> txBodyRaw {btbrOutputs = outputs_})
{-# INLINEABLE sizedOutputsBabbageTxBodyL #-}

referenceInputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
referenceInputsBabbageTxBodyL =
  lensTxBodyRaw
    btbrReferenceInputs
    (\txBodyRaw reference_ -> txBodyRaw {btbrReferenceInputs = reference_})
{-# INLINEABLE referenceInputsBabbageTxBodyL #-}

totalCollateralBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Coin)
totalCollateralBabbageTxBodyL =
  lensTxBodyRaw
    btbrTotalCollateral
    (\txBodyRaw totalCollateral_ -> txBodyRaw {btbrTotalCollateral = totalCollateral_})
{-# INLINEABLE totalCollateralBabbageTxBodyL #-}

collateralReturnBabbageTxBodyL ::
  forall era.
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (BabbageTxOut era))
collateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . btbrCollateralReturn)
    (\txBodyRaw collateralReturn -> txBodyRaw {btbrCollateralReturn = mkSized (eraProtVerLow @era) <$> collateralReturn})
{-# INLINEABLE collateralReturnBabbageTxBodyL #-}

sizedCollateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (Sized (BabbageTxOut era)))
sizedCollateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    btbrCollateralReturn
    (\txBodyRaw collateralReturn_ -> txBodyRaw {btbrCollateralReturn = collateralReturn_})
{-# INLINEABLE sizedCollateralReturnBabbageTxBodyL #-}

allSizedOutputsBabbageTxBodyF ::
  (BabbageEraTxBody era, Core.TxOut era ~ BabbageTxOut era) =>
  SimpleGetter (Core.TxBody era) (StrictSeq (Sized (BabbageTxOut era)))
allSizedOutputsBabbageTxBodyF =
  to $ \txBody ->
    let txOuts = txBody ^. sizedOutputsTxBodyL
     in case txBody ^. sizedCollateralReturnTxBodyL of
          SNothing -> txOuts
          SJust collTxOut -> txOuts |> collTxOut
{-# INLINEABLE allSizedOutputsBabbageTxBodyF #-}

instance CC.Crypto c => EraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance EraTxBody (BabbageEra CC.StandardCrypto) #-}

  type TxBody (BabbageEra c) = BabbageTxBody (BabbageEra c)

  mkBasicTxBody = mkBabbageTxBody

  inputsTxBodyL = inputsBabbageTxBodyL
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL = outputsBabbageTxBodyL
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = feeBabbageTxBodyL
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = auxDataHashBabbageTxBodyL
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF = allInputsBabbageTxBodyF
  {-# INLINE allInputsTxBodyF #-}

instance CC.Crypto c => ShelleyEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (BabbageEra CC.StandardCrypto) #-}

  wdrlsTxBodyL = wdrlsBabbbageTxBodyL
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = updateBabbageTxBodyL
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL = certsBabbageTxBodyL
  {-# INLINE certsTxBodyL #-}

instance CC.Crypto c => ShelleyMAEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (BabbageEra CC.StandardCrypto) #-}

  vldtTxBodyL = vldtBabbageTxBodyL
  {-# INLINE vldtTxBodyL #-}

  mintTxBodyL = mintBabbageTxBodyL
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintValueBabbageTxBodyF
  {-# INLINE mintValueTxBodyF #-}

  mintedTxBodyF = mintedBabbageTxBodyF
  {-# INLINE mintedTxBodyF #-}

instance CC.Crypto c => AlonzoEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (BabbageEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL = collateralInputsBabbageTxBodyL
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = reqSignerHashesBabbageTxBodyL
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = scriptIntegrityHashBabbageTxBodyL
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = networkIdBabbageTxBodyL
  {-# INLINE networkIdTxBodyL #-}

instance CC.Crypto c => BabbageEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (BabbageEra CC.StandardCrypto) #-}

  sizedOutputsTxBodyL = sizedOutputsBabbageTxBodyL
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL = referenceInputsBabbageTxBodyL
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL = totalCollateralBabbageTxBodyL
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL = collateralReturnBabbageTxBodyL
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL = sizedCollateralReturnBabbageTxBodyL
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = allSizedOutputsBabbageTxBodyF
  {-# INLINE allSizedOutputsTxBodyF #-}

type TxBody era = BabbageTxBody era

{-# DEPRECATED TxBody "Use `BabbageTxBody` instead" #-}

deriving newtype instance
  (Era era, Eq (Script era), Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (BabbageTxBody era)

deriving instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (BabbageTxBody era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBody era)

deriving via
  (Mem BabbageTxBodyRaw era)
  instance
    ( Era era,
      Val (Value era),
      DecodeNonNegative (Value era),
      FromCBOR (PParamsUpdate era),
      FromCBOR (Annotator (Script era))
    ) =>
    FromCBOR (Annotator (BabbageTxBody era))

instance
  ( Era era,
    Val (Value era),
    DecodeNonNegative (Value era),
    FromCBOR (PParamsUpdate era),
    FromCBOR (Annotator (Script era))
  ) =>
  FromCBOR (Annotator (BabbageTxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

pattern BabbageTxBody ::
  BabbageEraTxBody era =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Sized (BabbageTxOut era)) ->
  StrictMaybe (Sized (BabbageTxOut era)) ->
  StrictMaybe Coin ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  StrictMaybe (ScriptIntegrityHash (EraCrypto era)) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  StrictMaybe Network ->
  BabbageTxBody era
pattern BabbageTxBody
  { btbInputs,
    btbCollateral,
    btbReferenceInputs,
    btbOutputs,
    btbCollateralReturn,
    btbTotalCollateral,
    btbCerts,
    btbWdrls,
    btbTxFee,
    btbValidityInterval,
    btbUpdate,
    btbReqSignerHashes,
    btbMint,
    btbScriptIntegrityHash,
    btbAuxDataHash,
    btbTxNetworkId
  } <-
  TxBodyConstr
    ( Memo
        BabbageTxBodyRaw
          { btbrSpendInputs = btbInputs,
            btbrCollateralInputs = btbCollateral,
            btbrReferenceInputs = btbReferenceInputs,
            btbrOutputs = btbOutputs,
            btbrCollateralReturn = btbCollateralReturn,
            btbrTotalCollateral = btbTotalCollateral,
            btbrCerts = btbCerts,
            btbrWdrls = btbWdrls,
            btbrTxFee = btbTxFee,
            btbrValidityInterval = btbValidityInterval,
            btbrUpdate = btbUpdate,
            btbrReqSignerHashes = btbReqSignerHashes,
            btbrMint = btbMint,
            btbrScriptIntegrityHash = btbScriptIntegrityHash,
            btbrAuxDataHash = btbAuxDataHash,
            btbrTxNetworkId = btbTxNetworkId
          }
        _
      )
  where
    BabbageTxBody
      inputs
      collateral
      referenceInputs
      outputs
      collateralReturn
      totalCollateral
      certs
      wdrls
      txFee
      validityInterval
      update
      reqSignerHashes
      mint
      scriptIntegrityHash
      auxDataHash
      txNetworkId =
        mkBabbageTxBodyFromRaw $
          BabbageTxBodyRaw
            { btbrSpendInputs = inputs,
              btbrCollateralInputs = collateral,
              btbrReferenceInputs = referenceInputs,
              btbrOutputs = outputs,
              btbrCollateralReturn = collateralReturn,
              btbrTotalCollateral = totalCollateral,
              btbrCerts = certs,
              btbrWdrls = wdrls,
              btbrTxFee = txFee,
              btbrValidityInterval = validityInterval,
              btbrUpdate = update,
              btbrReqSignerHashes = reqSignerHashes,
              btbrMint = mint,
              btbrScriptIntegrityHash = scriptIntegrityHash,
              btbrAuxDataHash = auxDataHash,
              btbrTxNetworkId = txNetworkId
            }

{-# COMPLETE BabbageTxBody #-}

mkBabbageTxBodyFromRaw :: BabbageEraTxBody era => BabbageTxBodyRaw era -> BabbageTxBody era
mkBabbageTxBodyFromRaw = TxBodyConstr . memoBytes . encodeTxBodyRaw

mkBabbageTxBody :: BabbageEraTxBody era => BabbageTxBody era
mkBabbageTxBody = mkBabbageTxBodyFromRaw initialTxBodyRaw

instance (c ~ EraCrypto era) => HashAnnotated (BabbageTxBody era) EraIndependentTxBody c where
  hashAnnotated (TxBodyConstr mb) = mbHash mb

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: Era era => TxBody era -> Set (TxIn (EraCrypto era))
collateralInputs' :: Era era => TxBody era -> Set (TxIn (EraCrypto era))
referenceInputs' :: Era era => TxBody era -> Set (TxIn (EraCrypto era))
outputs' :: Era era => TxBody era -> StrictSeq (BabbageTxOut era)
collateralReturn' :: Era era => TxBody era -> StrictMaybe (BabbageTxOut era)
totalCollateral' :: Era era => TxBody era -> StrictMaybe Coin
certs' :: Era era => TxBody era -> StrictSeq (DCert (EraCrypto era))
txfee' :: Era era => TxBody era -> Coin
wdrls' :: Era era => TxBody era -> Wdrl (EraCrypto era)
vldt' :: Era era => TxBody era -> ValidityInterval
update' :: Era era => TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: Era era => TxBody era -> Set (KeyHash 'Witness (EraCrypto era))
adHash' :: Era era => TxBody era -> StrictMaybe (AuxiliaryDataHash (EraCrypto era))
mint' :: Era era => TxBody era -> MultiAsset (EraCrypto era)
scriptIntegrityHash' :: Era era => TxBody era -> StrictMaybe (ScriptIntegrityHash (EraCrypto era))
spendInputs' (TxBodyConstr (Memo raw _)) = btbrSpendInputs raw

txnetworkid' :: Era era => TxBody era -> StrictMaybe Network

collateralInputs' (TxBodyConstr (Memo raw _)) = btbrCollateralInputs raw

referenceInputs' (TxBodyConstr (Memo raw _)) = btbrReferenceInputs raw

outputs' (TxBodyConstr (Memo raw _)) = sizedValue <$> btbrOutputs raw

collateralReturn' (TxBodyConstr (Memo raw _)) = sizedValue <$> btbrCollateralReturn raw

totalCollateral' (TxBodyConstr (Memo raw _)) = btbrTotalCollateral raw

certs' (TxBodyConstr (Memo raw _)) = btbrCerts raw

wdrls' (TxBodyConstr (Memo raw _)) = btbrWdrls raw

txfee' (TxBodyConstr (Memo raw _)) = btbrTxFee raw

vldt' (TxBodyConstr (Memo raw _)) = btbrValidityInterval raw

update' (TxBodyConstr (Memo raw _)) = btbrUpdate raw

reqSignerHashes' (TxBodyConstr (Memo raw _)) = btbrReqSignerHashes raw

adHash' (TxBodyConstr (Memo raw _)) = btbrAuxDataHash raw

mint' (TxBodyConstr (Memo raw _)) = btbrMint raw

scriptIntegrityHash' (TxBodyConstr (Memo raw _)) = btbrScriptIntegrityHash raw

txnetworkid' (TxBodyConstr (Memo raw _)) = btbrTxNetworkId raw

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxBodyRaw ::
  BabbageEraTxBody era =>
  BabbageTxBodyRaw era ->
  Encode ('Closed 'Sparse) (BabbageTxBodyRaw era)
encodeTxBodyRaw
  BabbageTxBodyRaw
    { btbrSpendInputs,
      btbrCollateralInputs,
      btbrReferenceInputs,
      btbrOutputs,
      btbrCollateralReturn,
      btbrTotalCollateral,
      btbrCerts,
      btbrWdrls,
      btbrTxFee,
      btbrValidityInterval = ValidityInterval bot top,
      btbrUpdate,
      btbrReqSignerHashes,
      btbrMint,
      btbrScriptIntegrityHash,
      btbrAuxDataHash,
      btbrTxNetworkId
    } =
    Keyed
      ( \i ifee ri o cr tc f t c w u b rsh mi sh ah ni ->
          BabbageTxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (To btbrSpendInputs)
      !> Omit null (Key 13 (To btbrCollateralInputs))
      !> Omit null (Key 18 (To btbrReferenceInputs))
      !> Key 1 (To btbrOutputs)
      !> encodeKeyedStrictMaybe 16 btbrCollateralReturn
      !> encodeKeyedStrictMaybe 17 btbrTotalCollateral
      !> Key 2 (To btbrTxFee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (To btbrCerts))
      !> Omit (null . unWdrl) (Key 5 (To btbrWdrls))
      !> encodeKeyedStrictMaybe 6 btbrUpdate
      !> encodeKeyedStrictMaybe 8 bot
      !> Omit null (Key 14 (To btbrReqSignerHashes))
      !> Omit (== mempty) (Key 9 (E encodeMint btbrMint))
      !> encodeKeyedStrictMaybe 11 btbrScriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 btbrAuxDataHash
      !> encodeKeyedStrictMaybe 15 btbrTxNetworkId

instance
  ( Era era,
    Val (Value era),
    DecodeNonNegative (Value era),
    FromCBOR (Annotator (Script era)),
    FromCBOR (PParamsUpdate era)
  ) =>
  FromCBOR (BabbageTxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "BabbageTxBodyRaw"
        initialTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: (Word -> Field (BabbageTxBodyRaw era))
      bodyFields 0 =
        field (\x tx -> tx {btbrSpendInputs = x}) From
      bodyFields 13 =
        field (\x tx -> tx {btbrCollateralInputs = x}) From
      bodyFields 18 =
        field (\x tx -> tx {btbrReferenceInputs = x}) From
      bodyFields 1 =
        field
          (\x tx -> tx {btbrOutputs = x})
          (D (decodeStrictSeq (decodeSized (fromCborTxOutWithAddr fromCborBothAddr))))
      bodyFields 16 =
        ofield
          (\x tx -> tx {btbrCollateralReturn = x})
          (D (decodeSized (fromCborTxOutWithAddr fromCborBothAddr)))
      bodyFields 17 =
        ofield
          (\x tx -> tx {btbrTotalCollateral = x})
          From
      bodyFields 2 = field (\x tx -> tx {btbrTxFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {btbrCerts = x}) From
      bodyFields 5 =
        field
          (\x tx -> tx {btbrWdrls = x})
          (D (Wdrl <$> decodeMap fromCborRewardAcnt fromCBOR))
      bodyFields 6 = ofield (\x tx -> tx {btbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {btbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {btbrMint = x}) (D decodeMint)
      bodyFields 11 = ofield (\x tx -> tx {btbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {btbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {btbrTxNetworkId = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

initialTxBodyRaw :: BabbageTxBodyRaw era
initialTxBodyRaw =
  BabbageTxBodyRaw
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
    SNothing
    mempty
    mempty
    SNothing
    SNothing
    SNothing
