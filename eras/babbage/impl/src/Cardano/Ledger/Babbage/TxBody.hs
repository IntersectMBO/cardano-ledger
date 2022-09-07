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
        inputs,
        collateral,
        referenceInputs,
        outputs,
        collateralReturn,
        totalCollateral,
        txcerts,
        txwdrls,
        txfee,
        txvldt,
        txUpdates,
        reqSignerHashes,
        mint,
        scriptIntegrityHash,
        adHash,
        txnetworkid
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

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
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
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.Babbage.TxOut
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress
  ( fromCborBothAddr,
    fromCborRewardAcnt,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core (TxBody)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset, policies, policyID)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, memoBytes)
import Cardano.Ledger.SafeHash
  ( HashAnnotated (..),
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Serialization (Sized (..), mkSized, sizedDecoder)
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
import Data.Coders hiding (to)
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

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _spendInputs :: !(Set (TxIn (EraCrypto era))),
    _collateralInputs :: !(Set (TxIn (EraCrypto era))),
    _referenceInputs :: !(Set (TxIn (EraCrypto era))),
    _outputs :: !(StrictSeq (Sized (BabbageTxOut era))),
    _collateralReturn :: !(StrictMaybe (Sized (BabbageTxOut era))),
    _totalCollateral :: !(StrictMaybe Coin),
    _certs :: !(StrictSeq (DCert (EraCrypto era))),
    _wdrls :: !(Wdrl (EraCrypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: !(Set (KeyHash 'Witness (EraCrypto era))),
    _mint :: !(MultiAsset (EraCrypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

type instance MemoHashIndex TxBodyRaw = EraIndependentTxBody

deriving instance
  (Era era, Eq (Script era), Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (TxBodyRaw era)

instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (TxBodyRaw era)

instance (CC.Crypto (EraCrypto era), NFData (PParamsUpdate era)) => NFData (TxBodyRaw era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (TxBodyRaw era)

newtype BabbageTxBody era = TxBodyConstr (MemoBytes TxBodyRaw era)
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  (CC.Crypto (EraCrypto era), NFData (PParamsUpdate era)) =>
  NFData (BabbageTxBody era)

lensTxBodyRaw ::
  BabbageEraTxBody era =>
  (TxBodyRaw era -> a) ->
  (TxBodyRaw era -> t -> TxBodyRaw era) ->
  Lens (BabbageTxBody era) (BabbageTxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkBabbageTxBodyFromRaw $ setter txBodyRaw val)
{-# INLINEABLE lensTxBodyRaw #-}

inputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
inputsBabbageTxBodyL =
  lensTxBodyRaw _spendInputs (\txBodyRaw inputs_ -> txBodyRaw {_spendInputs = inputs_})
{-# INLINEABLE inputsBabbageTxBodyL #-}

outputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (BabbageTxOut era))
outputsBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . _outputs)
    (\txBodyRaw outputs_ -> txBodyRaw {_outputs = mkSized <$> outputs_})
{-# INLINEABLE outputsBabbageTxBodyL #-}

feeBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) Coin
feeBabbageTxBodyL =
  lensTxBodyRaw _txfee (\txBodyRaw fee_ -> txBodyRaw {_txfee = fee_})
{-# INLINEABLE feeBabbageTxBodyL #-}

auxDataHashBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
auxDataHashBabbageTxBodyL =
  lensTxBodyRaw _adHash (\txBodyRaw auxDataHash -> txBodyRaw {_adHash = auxDataHash})
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
  to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (_mint txBodyRaw)))
{-# INLINEABLE mintedBabbageTxBodyF #-}

wdrlsBabbbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (Wdrl (EraCrypto era))
wdrlsBabbbageTxBodyL =
  lensTxBodyRaw _wdrls (\txBodyRaw wdrls_ -> txBodyRaw {_wdrls = wdrls_})
{-# INLINEABLE wdrlsBabbbageTxBodyL #-}

updateBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Update era))
updateBabbageTxBodyL =
  lensTxBodyRaw _update (\txBodyRaw update_ -> txBodyRaw {_update = update_})
{-# INLINEABLE updateBabbageTxBodyL #-}

certsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (DCert (EraCrypto era)))
certsBabbageTxBodyL =
  lensTxBodyRaw _certs (\txBodyRaw certs_ -> txBodyRaw {_certs = certs_})
{-# INLINEABLE certsBabbageTxBodyL #-}

vldtBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) ValidityInterval
vldtBabbageTxBodyL =
  lensTxBodyRaw _vldt (\txBodyRaw vldt_ -> txBodyRaw {_vldt = vldt_})
{-# INLINEABLE vldtBabbageTxBodyL #-}

mintBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (MultiAsset (EraCrypto era))
mintBabbageTxBodyL =
  lensTxBodyRaw _mint (\txBodyRaw mint_ -> txBodyRaw {_mint = mint_})
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
    _collateralInputs
    (\txBodyRaw collateral_ -> txBodyRaw {_collateralInputs = collateral_})
{-# INLINEABLE collateralInputsBabbageTxBodyL #-}

reqSignerHashesBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (KeyHash 'Witness (EraCrypto era)))
reqSignerHashesBabbageTxBodyL =
  lensTxBodyRaw
    _reqSignerHashes
    (\txBodyRaw reqSignerHashes_ -> txBodyRaw {_reqSignerHashes = reqSignerHashes_})
{-# INLINEABLE reqSignerHashesBabbageTxBodyL #-}

scriptIntegrityHashBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
scriptIntegrityHashBabbageTxBodyL =
  lensTxBodyRaw
    _scriptIntegrityHash
    (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {_scriptIntegrityHash = scriptIntegrityHash_})
{-# INLINEABLE scriptIntegrityHashBabbageTxBodyL #-}

networkIdBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Network)
networkIdBabbageTxBodyL =
  lensTxBodyRaw _txnetworkid (\txBodyRaw networkId -> txBodyRaw {_txnetworkid = networkId})
{-# INLINEABLE networkIdBabbageTxBodyL #-}

sizedOutputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictSeq (Sized (BabbageTxOut era)))
sizedOutputsBabbageTxBodyL =
  lensTxBodyRaw _outputs (\txBodyRaw outputs_ -> txBodyRaw {_outputs = outputs_})
{-# INLINEABLE sizedOutputsBabbageTxBodyL #-}

referenceInputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (Set (TxIn (EraCrypto era)))
referenceInputsBabbageTxBodyL =
  lensTxBodyRaw
    _referenceInputs
    (\txBodyRaw reference_ -> txBodyRaw {_referenceInputs = reference_})
{-# INLINEABLE referenceInputsBabbageTxBodyL #-}

totalCollateralBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Coin)
totalCollateralBabbageTxBodyL =
  lensTxBodyRaw
    _totalCollateral
    (\txBodyRaw totalCollateral_ -> txBodyRaw {_totalCollateral = totalCollateral_})
{-# INLINEABLE totalCollateralBabbageTxBodyL #-}

collateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (BabbageTxOut era))
collateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . _collateralReturn)
    (\txBodyRaw collateralReturn_ -> txBodyRaw {_collateralReturn = mkSized <$> collateralReturn_})
{-# INLINEABLE collateralReturnBabbageTxBodyL #-}

sizedCollateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (Sized (BabbageTxOut era)))
sizedCollateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    _collateralReturn
    (\txBodyRaw collateralReturn_ -> txBodyRaw {_collateralReturn = collateralReturn_})
{-# INLINEABLE sizedCollateralReturnBabbageTxBodyL #-}

allSizedOutputsBabbageTxBodyF ::
  BabbageEraTxBody era =>
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

  mintedTxBodyF = mintedBabbageTxBodyF
  {-# INLINE mintedTxBodyF #-}

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

class (AlonzoEraTxBody era, BabbageEraTxOut era) => BabbageEraTxBody era where
  sizedOutputsTxBodyL :: Lens' (Core.TxBody era) (StrictSeq (Sized (BabbageTxOut era)))

  referenceInputsTxBodyL :: Lens' (Core.TxBody era) (Set (TxIn (EraCrypto era)))

  totalCollateralTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe Coin)

  collateralReturnTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe (BabbageTxOut era))

  sizedCollateralReturnTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe (Sized (BabbageTxOut era)))

  allSizedOutputsTxBodyF :: SimpleGetter (TxBody era) (StrictSeq (Sized (BabbageTxOut era)))

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

deriving newtype instance CC.Crypto (EraCrypto era) => Eq (BabbageTxBody era)

deriving instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (BabbageTxBody era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBody era)

deriving via
  (Mem TxBodyRaw era)
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
  FromCBOR (Annotator (TxBodyRaw era))
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
  { inputs,
    collateral,
    referenceInputs,
    outputs,
    collateralReturn,
    totalCollateral,
    txcerts,
    txwdrls,
    txfee,
    txvldt,
    txUpdates,
    reqSignerHashes,
    mint,
    scriptIntegrityHash,
    adHash,
    txnetworkid
  } <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { _spendInputs = inputs,
            _collateralInputs = collateral,
            _referenceInputs = referenceInputs,
            _outputs = outputs,
            _collateralReturn = collateralReturn,
            _totalCollateral = totalCollateral,
            _certs = txcerts,
            _wdrls = txwdrls,
            _txfee = txfee,
            _vldt = txvldt,
            _update = txUpdates,
            _reqSignerHashes = reqSignerHashes,
            _mint = mint,
            _scriptIntegrityHash = scriptIntegrityHash,
            _adHash = adHash,
            _txnetworkid = txnetworkid
          }
        _
      )
  where
    BabbageTxBody
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
      updateX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX =
        mkBabbageTxBodyFromRaw $
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
            updateX
            reqSignerHashesX
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX

{-# COMPLETE BabbageTxBody #-}

mkBabbageTxBodyFromRaw :: BabbageEraTxBody era => TxBodyRaw era -> BabbageTxBody era
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
spendInputs' (TxBodyConstr (Memo raw _)) = _spendInputs raw

txnetworkid' :: Era era => TxBody era -> StrictMaybe Network

collateralInputs' (TxBodyConstr (Memo raw _)) = _collateralInputs raw

referenceInputs' (TxBodyConstr (Memo raw _)) = _referenceInputs raw

outputs' (TxBodyConstr (Memo raw _)) = sizedValue <$> _outputs raw

collateralReturn' (TxBodyConstr (Memo raw _)) = sizedValue <$> _collateralReturn raw

totalCollateral' (TxBodyConstr (Memo raw _)) = _totalCollateral raw

certs' (TxBodyConstr (Memo raw _)) = _certs raw

wdrls' (TxBodyConstr (Memo raw _)) = _wdrls raw

txfee' (TxBodyConstr (Memo raw _)) = _txfee raw

vldt' (TxBodyConstr (Memo raw _)) = _vldt raw

update' (TxBodyConstr (Memo raw _)) = _update raw

reqSignerHashes' (TxBodyConstr (Memo raw _)) = _reqSignerHashes raw

adHash' (TxBodyConstr (Memo raw _)) = _adHash raw

mint' (TxBodyConstr (Memo raw _)) = _mint raw

scriptIntegrityHash' (TxBodyConstr (Memo raw _)) = _scriptIntegrityHash raw

txnetworkid' (TxBodyConstr (Memo raw _)) = _txnetworkid raw

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxBodyRaw ::
  BabbageEraTxBody era =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { _spendInputs,
      _collateralInputs,
      _referenceInputs,
      _outputs,
      _collateralReturn,
      _totalCollateral,
      _certs,
      _wdrls,
      _txfee,
      _vldt = ValidityInterval bot top,
      _update,
      _reqSignerHashes,
      _mint,
      _scriptIntegrityHash,
      _adHash,
      _txnetworkid
    } =
    Keyed
      ( \i ifee ri o cr tc f t c w u b rsh mi sh ah ni ->
          TxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (E encodeFoldable _spendInputs)
      !> Omit null (Key 13 (E encodeFoldable _collateralInputs))
      !> Omit null (Key 18 (E encodeFoldable _referenceInputs))
      !> Key 1 (E encodeFoldable _outputs)
      !> encodeKeyedStrictMaybe 16 _collateralReturn
      !> encodeKeyedStrictMaybe 17 _totalCollateral
      !> Key 2 (To _txfee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (E encodeFoldable _certs))
      !> Omit (null . unWdrl) (Key 5 (To _wdrls))
      !> encodeKeyedStrictMaybe 6 _update
      !> encodeKeyedStrictMaybe 8 bot
      !> Omit null (Key 14 (E encodeFoldable _reqSignerHashes))
      !> Omit (== mempty) (Key 9 (E encodeMint _mint))
      !> encodeKeyedStrictMaybe 11 _scriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 _adHash
      !> encodeKeyedStrictMaybe 15 _txnetworkid

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
          (\x tx -> tx {_spendInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {_collateralInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 18 =
        field
          (\x tx -> tx {_referenceInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {_outputs = x})
          (D (decodeStrictSeq (sizedDecoder (fromCborTxOutWithAddr fromCborBothAddr))))
      bodyFields 16 =
        ofield
          (\x tx -> tx {_collateralReturn = x})
          (D (sizedDecoder (fromCborTxOutWithAddr fromCborBothAddr)))
      bodyFields 17 =
        ofield
          (\x tx -> tx {_totalCollateral = x})
          From
      bodyFields 2 = field (\x tx -> tx {_txfee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        field
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 =
        field
          (\x tx -> tx {_wdrls = x})
          (D (Wdrl <$> decodeMap fromCborRewardAcnt fromCBOR))
      bodyFields 6 = ofield (\x tx -> tx {_update = x}) From
      bodyFields 7 = ofield (\x tx -> tx {_adHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {_vldt = (_vldt tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {_mint = x}) (D decodeMint)
      bodyFields 11 = ofield (\x tx -> tx {_scriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {_reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = ofield (\x tx -> tx {_txnetworkid = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

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
    SNothing
    mempty
    mempty
    SNothing
    SNothing
    SNothing
