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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut (
    BabbageTxOut,
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
  BabbageTxBody (
    BabbageTxBody,
    btbInputs,
    btbCollateral,
    btbReferenceInputs,
    btbOutputs,
    btbCollateralReturn,
    btbTotalCollateral,
    btbCerts,
    btbWithdrawals,
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
  withdrawalsBabbbageTxBodyL,
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
  withdrawals',
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
) where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Datum (..))
import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.TxBody as AlonzoTxBodyReExports (
  AllegraEraTxBody (..),
  AlonzoEraTxBody (..),
  MaryEraTxBody (..),
  ShelleyEraTxBody (..),
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Delegation ()
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.Babbage.TxOut hiding (TxOut)
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  EncCBOR (..),
  Sized (..),
  ToCBOR (..),
  mkSized,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset, policies, policyID)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData)
import Data.Sequence.Strict (StrictSeq, (|>))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Prelude hiding (lookup)

-- ======================================

data BabbageTxBodyRaw era = BabbageTxBodyRaw
  { btbrSpendInputs :: !(Set (TxIn (EraCrypto era)))
  , btbrCollateralInputs :: !(Set (TxIn (EraCrypto era)))
  , btbrReferenceInputs :: !(Set (TxIn (EraCrypto era)))
  , btbrOutputs :: !(StrictSeq (Sized (TxOut era)))
  , btbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era)))
  , btbrTotalCollateral :: !(StrictMaybe Coin)
  , btbrCerts :: !(StrictSeq (DCert era))
  , btbrWithdrawals :: !(Withdrawals (EraCrypto era))
  , btbrTxFee :: !Coin
  , btbrValidityInterval :: !ValidityInterval
  , btbrUpdate :: !(StrictMaybe (Update era))
  , btbrReqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era)))
  , btbrMint :: !(MultiAsset (EraCrypto era))
  , -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    btbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  , btbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  , btbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

type instance MemoHashIndex BabbageTxBodyRaw = EraIndependentTxBody

deriving instance
  (Era era, Eq (TxOut era), Eq (DCert era), Eq (PParamsUpdate era)) =>
  Eq (BabbageTxBodyRaw era)

instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (BabbageTxBodyRaw era)

instance
  (Era era, NFData (TxOut era), NFData (DCert era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBodyRaw era)

deriving instance
  (Era era, Show (TxOut era), Show (DCert era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBodyRaw era)

newtype BabbageTxBody era = TxBodyConstr (MemoBytes BabbageTxBodyRaw era)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized BabbageTxBody where
  type RawType BabbageTxBody = BabbageTxBodyRaw

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (DCert era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBody era)

inputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
inputsBabbageTxBodyL =
  lensMemoRawType btbrSpendInputs $ \txBodyRaw inputs -> txBodyRaw {btbrSpendInputs = inputs}
{-# INLINEABLE inputsBabbageTxBodyL #-}

outputsBabbageTxBodyL ::
  forall era. BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (TxOut era))
outputsBabbageTxBodyL =
  lensMemoRawType (fmap sizedValue . btbrOutputs) $
    \txBodyRaw outputs -> txBodyRaw {btbrOutputs = mkSized (eraProtVerLow @era) <$> outputs}
{-# INLINEABLE outputsBabbageTxBodyL #-}

feeBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) Coin
feeBabbageTxBodyL =
  lensMemoRawType btbrTxFee $ \txBodyRaw fee -> txBodyRaw {btbrTxFee = fee}
{-# INLINEABLE feeBabbageTxBodyL #-}

auxDataHashBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
auxDataHashBabbageTxBodyL =
  lensMemoRawType btbrAuxDataHash $ \txBodyRaw auxDataHash -> txBodyRaw {btbrAuxDataHash = auxDataHash}
{-# INLINEABLE auxDataHashBabbageTxBodyL #-}

allInputsBabbageTxBodyF ::
  BabbageEraTxBody era => SimpleGetter (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
allInputsBabbageTxBodyF =
  to $ \txBody ->
    (txBody ^. inputsBabbageTxBodyL)
      `Set.union` (txBody ^. collateralInputsBabbageTxBodyL)
      `Set.union` (txBody ^. referenceInputsBabbageTxBodyL)
{-# INLINEABLE allInputsBabbageTxBodyF #-}

mintedBabbageTxBodyF :: SimpleGetter (BabbageTxBody era) (Set (ScriptHash (EraCrypto era)))
mintedBabbageTxBodyF = to (Set.map policyID . policies . btbrMint . getMemoRawType)
{-# INLINEABLE mintedBabbageTxBodyF #-}

withdrawalsBabbbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (Withdrawals (EraCrypto era))
withdrawalsBabbbageTxBodyL =
  lensMemoRawType btbrWithdrawals $ \txBodyRaw withdrawals -> txBodyRaw {btbrWithdrawals = withdrawals}
{-# INLINEABLE withdrawalsBabbbageTxBodyL #-}

updateBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Update era))
updateBabbageTxBodyL =
  lensMemoRawType btbrUpdate $ \txBodyRaw update -> txBodyRaw {btbrUpdate = update}
{-# INLINEABLE updateBabbageTxBodyL #-}

certsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (DCert era))
certsBabbageTxBodyL =
  lensMemoRawType btbrCerts $ \txBodyRaw certs -> txBodyRaw {btbrCerts = certs}
{-# INLINEABLE certsBabbageTxBodyL #-}

vldtBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) ValidityInterval
vldtBabbageTxBodyL =
  lensMemoRawType btbrValidityInterval $ \txBodyRaw vldt -> txBodyRaw {btbrValidityInterval = vldt}
{-# INLINEABLE vldtBabbageTxBodyL #-}

mintBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (MultiAsset (EraCrypto era))
mintBabbageTxBodyL =
  lensMemoRawType btbrMint $ \txBodyRaw mint -> txBodyRaw {btbrMint = mint}
{-# INLINEABLE mintBabbageTxBodyL #-}

mintValueBabbageTxBodyF ::
  (BabbageEraTxBody era, Value era ~ MaryValue (EraCrypto era)) =>
  SimpleGetter (BabbageTxBody era) (Value era)
mintValueBabbageTxBodyF = mintBabbageTxBodyL . to (MaryValue 0)
{-# INLINEABLE mintValueBabbageTxBodyF #-}

collateralInputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
collateralInputsBabbageTxBodyL =
  lensMemoRawType btbrCollateralInputs $
    \txBodyRaw collateral -> txBodyRaw {btbrCollateralInputs = collateral}
{-# INLINEABLE collateralInputsBabbageTxBodyL #-}

reqSignerHashesBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (KeyHash 'Witness (EraCrypto era)))
reqSignerHashesBabbageTxBodyL =
  lensMemoRawType btbrReqSignerHashes $
    \txBodyRaw reqSignerHashes -> txBodyRaw {btbrReqSignerHashes = reqSignerHashes}
{-# INLINEABLE reqSignerHashesBabbageTxBodyL #-}

scriptIntegrityHashBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
scriptIntegrityHashBabbageTxBodyL =
  lensMemoRawType btbrScriptIntegrityHash $
    \txBodyRaw scriptIntegrityHash -> txBodyRaw {btbrScriptIntegrityHash = scriptIntegrityHash}
{-# INLINEABLE scriptIntegrityHashBabbageTxBodyL #-}

networkIdBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Network)
networkIdBabbageTxBodyL =
  lensMemoRawType btbrTxNetworkId $ \txBodyRaw networkId -> txBodyRaw {btbrTxNetworkId = networkId}
{-# INLINEABLE networkIdBabbageTxBodyL #-}

sizedOutputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictSeq (Sized (TxOut era)))
sizedOutputsBabbageTxBodyL =
  lensMemoRawType btbrOutputs $ \txBodyRaw outputs -> txBodyRaw {btbrOutputs = outputs}
{-# INLINEABLE sizedOutputsBabbageTxBodyL #-}

referenceInputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
referenceInputsBabbageTxBodyL =
  lensMemoRawType btbrReferenceInputs $
    \txBodyRaw reference -> txBodyRaw {btbrReferenceInputs = reference}
{-# INLINEABLE referenceInputsBabbageTxBodyL #-}

totalCollateralBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Coin)
totalCollateralBabbageTxBodyL =
  lensMemoRawType btbrTotalCollateral $
    \txBodyRaw totalCollateral -> txBodyRaw {btbrTotalCollateral = totalCollateral}
{-# INLINEABLE totalCollateralBabbageTxBodyL #-}

collateralReturnBabbageTxBodyL ::
  forall era.
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (TxOut era))
collateralReturnBabbageTxBodyL =
  lensMemoRawType (fmap sizedValue . btbrCollateralReturn) $
    \txBodyRaw collateralReturn ->
      txBodyRaw {btbrCollateralReturn = mkSized (eraProtVerLow @era) <$> collateralReturn}
{-# INLINEABLE collateralReturnBabbageTxBodyL #-}

sizedCollateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (Sized (TxOut era)))
sizedCollateralReturnBabbageTxBodyL =
  lensMemoRawType btbrCollateralReturn $
    \txBodyRaw collateralReturn -> txBodyRaw {btbrCollateralReturn = collateralReturn}
{-# INLINEABLE sizedCollateralReturnBabbageTxBodyL #-}

allSizedOutputsBabbageTxBodyF ::
  BabbageEraTxBody era =>
  SimpleGetter (TxBody era) (StrictSeq (Sized (TxOut era)))
allSizedOutputsBabbageTxBodyF =
  to $ \txBody ->
    let txOuts = txBody ^. sizedOutputsTxBodyL
     in case txBody ^. sizedCollateralReturnTxBodyL of
          SNothing -> txOuts
          SJust collTxOut -> txOuts |> collTxOut
{-# INLINEABLE allSizedOutputsBabbageTxBodyF #-}

instance Crypto c => EraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance EraTxBody (BabbageEra StandardCrypto) #-}

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

  withdrawalsTxBodyL = withdrawalsBabbbageTxBodyL
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL = certsBabbageTxBodyL
  {-# INLINE certsTxBodyL #-}

instance Crypto c => ShelleyEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (BabbageEra StandardCrypto) #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINE ttlTxBodyL #-}

  updateTxBodyL = updateBabbageTxBodyL
  {-# INLINE updateTxBodyL #-}

instance Crypto c => AllegraEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (BabbageEra StandardCrypto) #-}

  vldtTxBodyL = vldtBabbageTxBodyL
  {-# INLINE vldtTxBodyL #-}

instance Crypto c => MaryEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (BabbageEra StandardCrypto) #-}

  mintTxBodyL = mintBabbageTxBodyL
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintValueBabbageTxBodyF
  {-# INLINE mintValueTxBodyF #-}

  mintedTxBodyF = mintedBabbageTxBodyF
  {-# INLINE mintedTxBodyF #-}

instance Crypto c => AlonzoEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (BabbageEra StandardCrypto) #-}

  collateralInputsTxBodyL = collateralInputsBabbageTxBodyL
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = reqSignerHashesBabbageTxBodyL
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL = scriptIntegrityHashBabbageTxBodyL
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = networkIdBabbageTxBodyL
  {-# INLINE networkIdTxBodyL #-}

instance Crypto c => BabbageEraTxBody (BabbageEra c) where
  {-# SPECIALIZE instance BabbageEraTxBody (BabbageEra StandardCrypto) #-}

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

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (DCert era), Eq (PParamsUpdate era)) =>
  Eq (BabbageTxBody era)

deriving instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (BabbageTxBody era)

deriving instance
  (Era era, Show (TxOut era), Show (DCert era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBody era)

deriving via
  (Mem BabbageTxBodyRaw era)
  instance
    (Era era, DecCBOR (TxOut era), DecCBOR (DCert era), DecCBOR (PParamsUpdate era)) =>
    DecCBOR (Annotator (BabbageTxBody era))

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (DCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (Annotator (BabbageTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

pattern BabbageTxBody ::
  BabbageEraTxBody era =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Sized (TxOut era)) ->
  StrictMaybe (Sized (TxOut era)) ->
  StrictMaybe Coin ->
  StrictSeq (DCert era) ->
  Withdrawals (EraCrypto era) ->
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
  { btbInputs
  , btbCollateral
  , btbReferenceInputs
  , btbOutputs
  , btbCollateralReturn
  , btbTotalCollateral
  , btbCerts
  , btbWithdrawals
  , btbTxFee
  , btbValidityInterval
  , btbUpdate
  , btbReqSignerHashes
  , btbMint
  , btbScriptIntegrityHash
  , btbAuxDataHash
  , btbTxNetworkId
  } <-
  ( getMemoRawType ->
      BabbageTxBodyRaw
        { btbrSpendInputs = btbInputs
        , btbrCollateralInputs = btbCollateral
        , btbrReferenceInputs = btbReferenceInputs
        , btbrOutputs = btbOutputs
        , btbrCollateralReturn = btbCollateralReturn
        , btbrTotalCollateral = btbTotalCollateral
        , btbrCerts = btbCerts
        , btbrWithdrawals = btbWithdrawals
        , btbrTxFee = btbTxFee
        , btbrValidityInterval = btbValidityInterval
        , btbrUpdate = btbUpdate
        , btbrReqSignerHashes = btbReqSignerHashes
        , btbrMint = btbMint
        , btbrScriptIntegrityHash = btbScriptIntegrityHash
        , btbrAuxDataHash = btbAuxDataHash
        , btbrTxNetworkId = btbTxNetworkId
        }
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
      withdrawals
      txFee
      validityInterval
      update
      reqSignerHashes
      mint
      scriptIntegrityHash
      auxDataHash
      txNetworkId =
        mkMemoized $
          BabbageTxBodyRaw
            { btbrSpendInputs = inputs
            , btbrCollateralInputs = collateral
            , btbrReferenceInputs = referenceInputs
            , btbrOutputs = outputs
            , btbrCollateralReturn = collateralReturn
            , btbrTotalCollateral = totalCollateral
            , btbrCerts = certs
            , btbrWithdrawals = withdrawals
            , btbrTxFee = txFee
            , btbrValidityInterval = validityInterval
            , btbrUpdate = update
            , btbrReqSignerHashes = reqSignerHashes
            , btbrMint = mint
            , btbrScriptIntegrityHash = scriptIntegrityHash
            , btbrAuxDataHash = auxDataHash
            , btbrTxNetworkId = txNetworkId
            }

{-# COMPLETE BabbageTxBody #-}

mkBabbageTxBody :: BabbageEraTxBody era => BabbageTxBody era
mkBabbageTxBody = mkMemoized basicBabbageTxBodyRaw

instance (c ~ EraCrypto era) => HashAnnotated (BabbageTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: BabbageTxBody era -> Set (TxIn (EraCrypto era))
collateralInputs' :: BabbageTxBody era -> Set (TxIn (EraCrypto era))
referenceInputs' :: BabbageTxBody era -> Set (TxIn (EraCrypto era))
outputs' :: BabbageTxBody era -> StrictSeq (TxOut era)
collateralReturn' :: BabbageTxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: BabbageTxBody era -> StrictMaybe Coin
certs' :: BabbageTxBody era -> StrictSeq (DCert era)
txfee' :: BabbageTxBody era -> Coin
withdrawals' :: BabbageTxBody era -> Withdrawals (EraCrypto era)
vldt' :: BabbageTxBody era -> ValidityInterval
update' :: BabbageTxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: BabbageTxBody era -> Set (KeyHash 'Witness (EraCrypto era))
adHash' :: BabbageTxBody era -> StrictMaybe (AuxiliaryDataHash (EraCrypto era))
mint' :: BabbageTxBody era -> MultiAsset (EraCrypto era)
scriptIntegrityHash' :: BabbageTxBody era -> StrictMaybe (ScriptIntegrityHash (EraCrypto era))
spendInputs' = btbrSpendInputs . getMemoRawType

txnetworkid' :: BabbageTxBody era -> StrictMaybe Network

collateralInputs' = btbrCollateralInputs . getMemoRawType

referenceInputs' = btbrReferenceInputs . getMemoRawType

outputs' = fmap sizedValue . btbrOutputs . getMemoRawType

collateralReturn' = fmap sizedValue . btbrCollateralReturn . getMemoRawType

totalCollateral' = btbrTotalCollateral . getMemoRawType

certs' = btbrCerts . getMemoRawType

withdrawals' = btbrWithdrawals . getMemoRawType

txfee' = btbrTxFee . getMemoRawType

vldt' = btbrValidityInterval . getMemoRawType

update' = btbrUpdate . getMemoRawType

reqSignerHashes' = btbrReqSignerHashes . getMemoRawType

adHash' = btbrAuxDataHash . getMemoRawType

mint' = btbrMint . getMemoRawType

scriptIntegrityHash' = btbrScriptIntegrityHash . getMemoRawType

txnetworkid' = btbrTxNetworkId . getMemoRawType

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (BabbageTxBody era)

instance
  (Era era, EncCBOR (TxOut era), EncCBOR (DCert era), EncCBOR (PParamsUpdate era)) =>
  EncCBOR (BabbageTxBodyRaw era)
  where
  encCBOR
    BabbageTxBodyRaw
      { btbrSpendInputs
      , btbrCollateralInputs
      , btbrReferenceInputs
      , btbrOutputs
      , btbrCollateralReturn
      , btbrTotalCollateral
      , btbrCerts
      , btbrWithdrawals
      , btbrTxFee
      , btbrValidityInterval = ValidityInterval bot top
      , btbrUpdate
      , btbrReqSignerHashes
      , btbrMint
      , btbrScriptIntegrityHash
      , btbrAuxDataHash
      , btbrTxNetworkId
      } =
      encode $
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
          !> Omit (null . unWithdrawals) (Key 5 (To btbrWithdrawals))
          !> encodeKeyedStrictMaybe 6 btbrUpdate
          !> encodeKeyedStrictMaybe 8 bot
          !> Omit null (Key 14 (To btbrReqSignerHashes))
          !> Omit (== mempty) (Key 9 (To btbrMint))
          !> encodeKeyedStrictMaybe 11 btbrScriptIntegrityHash
          !> encodeKeyedStrictMaybe 7 btbrAuxDataHash
          !> encodeKeyedStrictMaybe 15 btbrTxNetworkId

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (DCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (BabbageTxBodyRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "BabbageTxBodyRaw"
        basicBabbageTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (BabbageTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {btbrSpendInputs = x}) From
      bodyFields 13 = field (\x tx -> tx {btbrCollateralInputs = x}) From
      bodyFields 18 = field (\x tx -> tx {btbrReferenceInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {btbrOutputs = x}) From
      bodyFields 16 = ofield (\x tx -> tx {btbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {btbrTotalCollateral = x}) From
      bodyFields 2 = field (\x tx -> tx {btbrTxFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {btbrCerts = x}) From
      bodyFields 5 = field (\x tx -> tx {btbrWithdrawals = x}) From
      bodyFields 6 = ofield (\x tx -> tx {btbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {btbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {btbrValidityInterval = (btbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {btbrMint = x}) From
      bodyFields 11 = ofield (\x tx -> tx {btbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {btbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {btbrTxNetworkId = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]

basicBabbageTxBodyRaw :: BabbageTxBodyRaw era
basicBabbageTxBodyRaw =
  BabbageTxBodyRaw
    mempty
    mempty
    mempty
    StrictSeq.empty
    SNothing
    SNothing
    StrictSeq.empty
    (Withdrawals mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    SNothing
    mempty
    mempty
    SNothing
    SNothing
    SNothing
