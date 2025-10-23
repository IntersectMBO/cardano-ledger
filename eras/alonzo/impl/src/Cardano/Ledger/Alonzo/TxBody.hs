{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxBody (
  AlonzoTxOut (..),
  AlonzoEraTxOut (..),
  -- Constructors are not exported for safety:
  Addr28Extra,
  DataHash32,
  TxBody (
    MkAlonzoTxBody,
    AlonzoTxBody,
    atbInputs,
    atbCollateral,
    atbOutputs,
    atbCerts,
    atbWithdrawals,
    atbTxFee,
    atbValidityInterval,
    atbUpdate,
    atbReqSignerHashes,
    atbMint,
    atbScriptIntegrityHash,
    atbAuxDataHash,
    atbTxNetworkId
  ),
  AlonzoTxBodyRaw (..),
  AlonzoEraTxBody (..),
  ShelleyEraTxBody (..),
  AllegraEraTxBody (..),
  MaryEraTxBody (..),
  Indexable (..),
  inputs',
  collateral',
  outputs',
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
  getAdaOnly,
  decodeDataHash32,
  encodeDataHash32,
  encodeAddress28,
  decodeAddress28,
  viewCompactTxOut,
  viewTxOut,
  EraIndependentScriptIntegrity,
  ScriptIntegrityHash,
  getAlonzoTxOutEitherAddr,
  utxoEntrySize,
  alonzoRedeemerPointer,
  alonzoRedeemerPointerInverse,
) where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIx (..),
  AsIxItem (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Alonzo.TxAuxData ()
import Cardano.Ledger.Alonzo.TxCert ()
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value (
  MultiAsset (..),
  PolicyID (..),
  policies,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import qualified Data.Map.Strict as Map
import Data.OSet.Strict (OSet)
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

type ScriptIntegrityHash = SafeHash EraIndependentScriptIntegrity

class (MaryEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody era) (Set TxIn)

  reqSignerHashesTxBodyL :: AtMostEra "Conway" era => Lens' (TxBody era) (Set (KeyHash 'Guard))

  reqSignerHashesTxBodyG ::
    SimpleGetter (TxBody era) (Set (KeyHash Guard))
  default reqSignerHashesTxBodyG ::
    AtMostEra "Conway" era => SimpleGetter (TxBody era) (Set (KeyHash Guard))
  reqSignerHashesTxBodyG = reqSignerHashesTxBodyL

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody era) (StrictMaybe ScriptIntegrityHash)

  networkIdTxBodyL :: Lens' (TxBody era) (StrictMaybe Network)

  -- | This function is called @rdptr@ in the spec. Given a `TxBody` and a plutus
  -- purpose with an item, we should be able to find the plutus purpose as in index
  redeemerPointer ::
    TxBody era ->
    PlutusPurpose AsItem era ->
    StrictMaybe (PlutusPurpose AsIx era)

  -- | This is an inverse of `redeemerPointer`. Given purpose as an index return it as an item.
  redeemerPointerInverse ::
    TxBody era ->
    PlutusPurpose AsIx era ->
    StrictMaybe (PlutusPurpose AsIxItem era)

-- ======================================

data AlonzoTxBodyRaw = AlonzoTxBodyRaw
  { atbrInputs :: !(Set TxIn)
  , atbrCollateral :: !(Set TxIn)
  , atbrOutputs :: !(StrictSeq (TxOut AlonzoEra))
  , atbrCerts :: !(StrictSeq (TxCert AlonzoEra))
  , atbrWithdrawals :: !Withdrawals
  , atbrTxFee :: !Coin
  , atbrValidityInterval :: !ValidityInterval
  , atbrUpdate :: !(StrictMaybe (Update AlonzoEra))
  , atbrReqSignerHashes :: Set (KeyHash 'Guard)
  , atbrMint :: !MultiAsset
  , atbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , atbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , atbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic)

deriving instance Eq AlonzoTxBodyRaw

instance NoThunks AlonzoTxBodyRaw

instance NFData AlonzoTxBodyRaw

deriving instance Show AlonzoTxBodyRaw

instance Memoized (TxBody AlonzoEra) where
  type RawType (TxBody AlonzoEra) = AlonzoTxBodyRaw

instance EraTxBody AlonzoEra where
  newtype TxBody AlonzoEra = MkAlonzoTxBody (MemoBytes AlonzoTxBodyRaw)
    deriving (ToCBOR, Generic)
    deriving newtype (SafeToHash)

  mkBasicTxBody = mkMemoizedEra @AlonzoEra emptyAlonzoTxBodyRaw

  inputsTxBodyL =
    lensMemoRawType @AlonzoEra atbrInputs $
      \txBodyRaw inputs_ -> txBodyRaw {atbrInputs = inputs_}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @AlonzoEra atbrOutputs $
      \txBodyRaw outputs_ -> txBodyRaw {atbrOutputs = outputs_}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @AlonzoEra atbrTxFee $
      \txBodyRaw fee_ -> txBodyRaw {atbrTxFee = fee_}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @AlonzoEra atbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = allInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @AlonzoEra atbrWithdrawals $
      \txBodyRaw withdrawals_ -> txBodyRaw {atbrWithdrawals = withdrawals_}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @AlonzoEra atbrCerts $
      \txBodyRaw certs_ -> txBodyRaw {atbrCerts = certs_}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

instance ShelleyEraTxBody AlonzoEra where
  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL =
    lensMemoRawType @AlonzoEra atbrUpdate $
      \txBodyRaw update_ -> txBodyRaw {atbrUpdate = update_}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody AlonzoEra where
  vldtTxBodyL =
    lensMemoRawType @AlonzoEra atbrValidityInterval $
      \txBodyRaw vldt_ -> txBodyRaw {atbrValidityInterval = vldt_}
  {-# INLINEABLE vldtTxBodyL #-}

instance MaryEraTxBody AlonzoEra where
  mintTxBodyL =
    lensMemoRawType @AlonzoEra atbrMint $
      \txBodyRaw mint_ -> txBodyRaw {atbrMint = mint_}
  {-# INLINEABLE mintTxBodyL #-}

  mintedTxBodyF = to (policies . atbrMint . getMemoRawType)
  {-# INLINEABLE mintedTxBodyF #-}

instance AlonzoEraTxBody AlonzoEra where
  collateralInputsTxBodyL =
    lensMemoRawType @AlonzoEra atbrCollateral $
      \txBodyRaw collateral_ -> txBodyRaw {atbrCollateral = collateral_}
  {-# INLINEABLE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType @AlonzoEra atbrReqSignerHashes $
      \txBodyRaw reqSignerHashes_ -> txBodyRaw {atbrReqSignerHashes = reqSignerHashes_}
  {-# INLINEABLE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType @AlonzoEra atbrScriptIntegrityHash $
      \txBodyRaw scriptIntegrityHash_ -> txBodyRaw {atbrScriptIntegrityHash = scriptIntegrityHash_}
  {-# INLINEABLE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL =
    lensMemoRawType @AlonzoEra atbrTxNetworkId $
      \txBodyRaw networkId -> txBodyRaw {atbrTxNetworkId = networkId}
  {-# INLINEABLE networkIdTxBodyL #-}

  redeemerPointer = alonzoRedeemerPointer

  redeemerPointerInverse = alonzoRedeemerPointerInverse

deriving newtype instance Eq (TxBody AlonzoEra)

deriving instance NoThunks (TxBody AlonzoEra)

deriving instance NFData (TxBody AlonzoEra)

deriving instance Show (TxBody AlonzoEra)

deriving via Mem AlonzoTxBodyRaw instance DecCBOR (Annotator (TxBody AlonzoEra))

pattern AlonzoTxBody ::
  Set TxIn ->
  Set TxIn ->
  StrictSeq (TxOut AlonzoEra) ->
  StrictSeq (TxCert AlonzoEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update AlonzoEra) ->
  Set (KeyHash 'Guard) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  TxBody AlonzoEra
pattern AlonzoTxBody
  { atbInputs
  , atbCollateral
  , atbOutputs
  , atbCerts
  , atbWithdrawals
  , atbTxFee
  , atbValidityInterval
  , atbUpdate
  , atbReqSignerHashes
  , atbMint
  , atbScriptIntegrityHash
  , atbAuxDataHash
  , atbTxNetworkId
  } <-
  ( getMemoRawType ->
      AlonzoTxBodyRaw
        { atbrInputs = atbInputs
        , atbrCollateral = atbCollateral
        , atbrOutputs = atbOutputs
        , atbrCerts = atbCerts
        , atbrWithdrawals = atbWithdrawals
        , atbrTxFee = atbTxFee
        , atbrValidityInterval = atbValidityInterval
        , atbrUpdate = atbUpdate
        , atbrReqSignerHashes = atbReqSignerHashes
        , atbrMint = atbMint
        , atbrScriptIntegrityHash = atbScriptIntegrityHash
        , atbrAuxDataHash = atbAuxDataHash
        , atbrTxNetworkId = atbTxNetworkId
        }
    )
  where
    AlonzoTxBody
      inputs
      collateral
      outputs
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
        mkMemoizedEra @AlonzoEra $
          AlonzoTxBodyRaw
            { atbrInputs = inputs
            , atbrCollateral = collateral
            , atbrOutputs = outputs
            , atbrCerts = certs
            , atbrWithdrawals = withdrawals
            , atbrTxFee = txFee
            , atbrValidityInterval = validityInterval
            , atbrUpdate = update
            , atbrReqSignerHashes = reqSignerHashes
            , atbrMint = mint
            , atbrScriptIntegrityHash = scriptIntegrityHash
            , atbrAuxDataHash = auxDataHash
            , atbrTxNetworkId = txNetworkId
            }

{-# COMPLETE AlonzoTxBody #-}

type instance MemoHashIndex AlonzoTxBodyRaw = EraIndependentTxBody

instance HashAnnotated (TxBody AlonzoEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: TxBody AlonzoEra -> Set TxIn
collateral' :: TxBody AlonzoEra -> Set TxIn
outputs' :: TxBody AlonzoEra -> StrictSeq (TxOut AlonzoEra)
certs' :: TxBody AlonzoEra -> StrictSeq (TxCert AlonzoEra)
txfee' :: TxBody AlonzoEra -> Coin
withdrawals' :: TxBody AlonzoEra -> Withdrawals
vldt' :: TxBody AlonzoEra -> ValidityInterval
update' :: TxBody AlonzoEra -> StrictMaybe (Update AlonzoEra)
reqSignerHashes' :: TxBody AlonzoEra -> Set (KeyHash 'Guard)
adHash' :: TxBody AlonzoEra -> StrictMaybe TxAuxDataHash
mint' :: TxBody AlonzoEra -> MultiAsset
scriptIntegrityHash' :: TxBody AlonzoEra -> StrictMaybe ScriptIntegrityHash
txnetworkid' :: TxBody AlonzoEra -> StrictMaybe Network
inputs' = atbrInputs . getMemoRawType
{-# DEPRECATED inputs' "In favor of inputsTxBodyL" #-}

collateral' = atbrCollateral . getMemoRawType
{-# DEPRECATED collateral' "In favor of collateralInputsTxBodyL" #-}

outputs' = atbrOutputs . getMemoRawType
{-# DEPRECATED outputs' "In favor of outputsTxBodyL" #-}

certs' = atbrCerts . getMemoRawType
{-# DEPRECATED certs' "In favor of certsTxBodyL" #-}

withdrawals' = atbrWithdrawals . getMemoRawType
{-# DEPRECATED withdrawals' "In favor of withdrawalsTxBodyL" #-}

txfee' = atbrTxFee . getMemoRawType
{-# DEPRECATED txfee' "In favor of feeTxBodyL" #-}

vldt' = atbrValidityInterval . getMemoRawType
{-# DEPRECATED vldt' "In favor of vldtTxBodyL" #-}

update' = atbrUpdate . getMemoRawType
{-# DEPRECATED update' "In favor of updateTxBodyL" #-}

reqSignerHashes' = atbrReqSignerHashes . getMemoRawType
{-# DEPRECATED reqSignerHashes' "In favor of reqSignerHashesTxBodyL" #-}

adHash' = atbrAuxDataHash . getMemoRawType
{-# DEPRECATED adHash' "In favor of auxDataHashTxBodyL" #-}

mint' = atbrMint . getMemoRawType
{-# DEPRECATED mint' "In favor of mintTxBodyL" #-}

scriptIntegrityHash' = atbrScriptIntegrityHash . getMemoRawType
{-# DEPRECATED scriptIntegrityHash' "In favor of scriptIntegrityHashTxBodyL" #-}

txnetworkid' = atbrTxNetworkId . getMemoRawType
{-# DEPRECATED txnetworkid' "In favor of networkIdTxBodyL" #-}

instance EqRaw (TxBody AlonzoEra)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance EncCBOR (TxBody AlonzoEra)

instance EncCBOR AlonzoTxBodyRaw where
  encCBOR
    AlonzoTxBodyRaw
      { atbrInputs
      , atbrCollateral
      , atbrOutputs
      , atbrCerts
      , atbrWithdrawals
      , atbrTxFee
      , atbrValidityInterval = ValidityInterval bot top
      , atbrUpdate
      , atbrReqSignerHashes
      , atbrMint
      , atbrScriptIntegrityHash
      , atbrAuxDataHash
      , atbrTxNetworkId
      } =
      encode $
        Keyed
          ( \i ifee o f t c w u b rsh mi sh ah ni ->
              AlonzoTxBodyRaw i ifee o c w f (ValidityInterval b t) u rsh mi sh ah ni
          )
          !> Key 0 (To atbrInputs)
          !> Omit null (Key 13 (To atbrCollateral))
          !> Key 1 (To atbrOutputs)
          !> Key 2 (To atbrTxFee)
          !> encodeKeyedStrictMaybe 3 top
          !> Omit null (Key 4 (To atbrCerts))
          !> Omit (null . unWithdrawals) (Key 5 (To atbrWithdrawals))
          !> encodeKeyedStrictMaybe 6 atbrUpdate
          !> encodeKeyedStrictMaybe 8 bot
          !> Omit null (Key 14 (To atbrReqSignerHashes))
          !> Omit (== mempty) (Key 9 (To atbrMint))
          !> encodeKeyedStrictMaybe 11 atbrScriptIntegrityHash
          !> encodeKeyedStrictMaybe 7 atbrAuxDataHash
          !> encodeKeyedStrictMaybe 15 atbrTxNetworkId

instance DecCBOR AlonzoTxBodyRaw where
  decCBOR =
    decode $
      SparseKeyed
        "AlonzoTxBodyRaw"
        emptyAlonzoTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field AlonzoTxBodyRaw
      bodyFields 0 = field (\x tx -> tx {atbrInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {atbrOutputs = x}) From
      bodyFields 2 = field (\x tx -> tx {atbrTxFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {atbrValidityInterval = (atbrValidityInterval tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {atbrCerts = x}) From
      bodyFields 5 = field (\x tx -> tx {atbrWithdrawals = x}) From
      bodyFields 6 = ofield (\x tx -> tx {atbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {atbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {atbrValidityInterval = (atbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {atbrMint = x}) From
      bodyFields 11 = ofield (\x tx -> tx {atbrScriptIntegrityHash = x}) From
      bodyFields 13 = field (\x tx -> tx {atbrCollateral = x}) From
      bodyFields 14 = field (\x tx -> tx {atbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {atbrTxNetworkId = x}) From
      bodyFields n = invalidField n
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]

instance DecCBOR (Annotator AlonzoTxBodyRaw) where
  decCBOR = pure <$> decCBOR

emptyAlonzoTxBodyRaw :: AlonzoTxBodyRaw
emptyAlonzoTxBodyRaw =
  AlonzoTxBodyRaw
    mempty
    mempty
    StrictSeq.empty
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

alonzoRedeemerPointer ::
  forall era.
  MaryEraTxBody era =>
  TxBody era ->
  AlonzoPlutusPurpose AsItem era ->
  StrictMaybe (AlonzoPlutusPurpose AsIx era)
alonzoRedeemerPointer txBody = \case
  AlonzoSpending txIn ->
    AlonzoSpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  AlonzoMinting policyID ->
    AlonzoMinting <$> indexOf policyID (txBody ^. mintedTxBodyF :: Set PolicyID)
  AlonzoCertifying txCert ->
    AlonzoCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  AlonzoRewarding rewardAccount ->
    AlonzoRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))

alonzoRedeemerPointerInverse ::
  MaryEraTxBody era =>
  TxBody era ->
  AlonzoPlutusPurpose AsIx era ->
  StrictMaybe (AlonzoPlutusPurpose AsIxItem era)
alonzoRedeemerPointerInverse txBody = \case
  AlonzoSpending idx ->
    AlonzoSpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  AlonzoMinting idx ->
    AlonzoMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  AlonzoCertifying idx ->
    AlonzoCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  AlonzoRewarding idx ->
    AlonzoRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))

class Indexable elem container where
  indexOf :: AsItem Word32 elem -> container -> StrictMaybe (AsIx Word32 elem)
  fromIndex :: AsIx Word32 elem -> container -> StrictMaybe (AsIxItem Word32 elem)

instance Ord k => Indexable k (Set k) where
  indexOf (AsItem n) s = case Set.lookupIndex n s of
    Just x -> SJust (AsIx (fromIntegral @Int @Word32 x))
    Nothing -> SNothing
  fromIndex (AsIx w32) s =
    let i = fromIntegral @Word32 @Int w32
     in if i < Set.size s
          then SJust $ AsIxItem w32 (Set.elemAt i s)
          else SNothing

instance Eq k => Indexable k (StrictSeq k) where
  indexOf (AsItem n) seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> SJust (AsIx (fromIntegral @Int @Word32 m))
    Nothing -> SNothing
  fromIndex (AsIx w32) seqx =
    case StrictSeq.lookup (fromIntegral @Word32 @Int w32) seqx of
      Nothing -> SNothing
      Just x -> SJust $ AsIxItem w32 x

instance Ord k => Indexable k (Map.Map k v) where
  indexOf (AsItem n) mp = case Map.lookupIndex n mp of
    Just x -> SJust (AsIx (fromIntegral @Int @Word32 x))
    Nothing -> SNothing
  fromIndex (AsIx w32) mp =
    let i = fromIntegral @Word32 @Int w32
     in if i < fromIntegral (Map.size mp)
          then SJust . AsIxItem w32 . fst $ Map.elemAt i mp
          else SNothing

instance Ord k => Indexable k (OSet k) where
  indexOf asItem = indexOf asItem . OSet.toStrictSeq
  fromIndex asIndex = fromIndex asIndex . OSet.toStrictSeq
