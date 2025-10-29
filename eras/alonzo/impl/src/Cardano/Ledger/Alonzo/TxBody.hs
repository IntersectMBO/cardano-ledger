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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Cardano.Ledger.Mary.Value (MultiAsset (..), PolicyID (..))
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
import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.Map.Strict as Map
import Data.OSet.Strict (OSet)
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (InspectHeap (..), NoThunks (..))

type ScriptIntegrityHash = SafeHash EraIndependentScriptIntegrity

class (MaryEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody TopTx era) (Set TxIn)

  reqSignerHashesTxBodyL :: AtMostEra "Conway" era => Lens' (TxBody l era) (Set (KeyHash 'Guard))

  reqSignerHashesTxBodyG ::
    SimpleGetter (TxBody l era) (Set (KeyHash Guard))
  default reqSignerHashesTxBodyG ::
    AtMostEra "Conway" era => SimpleGetter (TxBody l era) (Set (KeyHash Guard))
  reqSignerHashesTxBodyG = reqSignerHashesTxBodyL

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody l era) (StrictMaybe ScriptIntegrityHash)

  networkIdTxBodyL :: Lens' (TxBody l era) (StrictMaybe Network)

  -- | This function is called @rdptr@ in the spec. Given a `TxBody` and a plutus
  -- purpose with an item, we should be able to find the plutus purpose as in index
  redeemerPointer ::
    TxBody l era ->
    PlutusPurpose AsItem era ->
    StrictMaybe (PlutusPurpose AsIx era)

  -- | This is an inverse of `redeemerPointer`. Given purpose as an index return it as an item.
  redeemerPointerInverse ::
    TxBody l era ->
    PlutusPurpose AsIx era ->
    StrictMaybe (PlutusPurpose AsIxItem era)

-- ======================================

data AlonzoTxBodyRaw l era where
  AlonzoTxBodyRaw ::
    { atbrInputs :: !(Set TxIn)
    , atbrCollateral :: !(Set TxIn)
    , atbrOutputs :: !(StrictSeq (TxOut era))
    , atbrCerts :: !(StrictSeq (TxCert era))
    , atbrWithdrawals :: !Withdrawals
    , atbrTxFee :: !Coin
    , atbrValidityInterval :: !ValidityInterval
    , atbrUpdate :: !(StrictMaybe (Update era))
    , atbrReqSignerHashes :: Set (KeyHash 'Guard)
    , atbrMint :: !MultiAsset
    , atbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , atbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , atbrTxNetworkId :: !(StrictMaybe Network)
    } ->
    AlonzoTxBodyRaw TopTx era

deriving instance Eq (AlonzoTxBodyRaw l AlonzoEra)

deriving via
  InspectHeap (AlonzoTxBodyRaw l AlonzoEra)
  instance
    Typeable l => NoThunks (AlonzoTxBodyRaw l AlonzoEra)

instance
  ( NFData (TxOut era)
  , NFData (TxCert era)
  , NFData (PParamsHKD StrictMaybe era)
  ) =>
  NFData (AlonzoTxBodyRaw l era)
  where
  rnf AlonzoTxBodyRaw {..} =
    atbrInputs `deepseq`
      atbrCollateral `deepseq`
        atbrOutputs `deepseq`
          atbrCerts `deepseq`
            atbrWithdrawals `deepseq`
              atbrTxFee `deepseq`
                atbrValidityInterval `deepseq`
                  atbrUpdate `deepseq`
                    atbrReqSignerHashes `deepseq`
                      atbrMint `deepseq`
                        atbrScriptIntegrityHash `deepseq`
                          atbrAuxDataHash `deepseq`
                            rnf atbrTxNetworkId

deriving instance Show (AlonzoTxBodyRaw l AlonzoEra)

instance Memoized (TxBody l AlonzoEra) where
  type RawType (TxBody l AlonzoEra) = AlonzoTxBodyRaw l AlonzoEra

instance HasEraTxLevel AlonzoTxBodyRaw AlonzoEra where
  toSTxLevel AlonzoTxBodyRaw {} = STopTxOnly

instance HasEraTxLevel TxBody AlonzoEra where
  toSTxLevel = toSTxLevel . getMemoRawType

instance EraTxBody AlonzoEra where
  newtype TxBody l AlonzoEra = MkAlonzoTxBody (MemoBytes (AlonzoTxBodyRaw l AlonzoEra))
    deriving (ToCBOR, Generic)
    deriving newtype (SafeToHash)

  mkBasicTxBody = emptyAlonzoTxBody

  inputsTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrInputs} -> atbrInputs) $
      \txBodyRaw inputs_ -> txBodyRaw {atbrInputs = inputs_}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrOutputs} -> atbrOutputs) $
      \txBodyRaw outputs_ -> txBodyRaw {atbrOutputs = outputs_}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrTxFee} -> atbrTxFee) $
      \txBodyRaw fee_ -> txBodyRaw {atbrTxFee = fee_}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrAuxDataHash} -> atbrAuxDataHash) $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = to (`withTopTxLevelOnly` (^. allInputsTxBodyF))
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrWithdrawals} -> atbrWithdrawals) $
      \txBodyRaw withdrawals_ -> txBodyRaw {atbrWithdrawals = withdrawals_}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrCerts} -> atbrCerts) $
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
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrValidityInterval} -> atbrValidityInterval) $
      \txBodyRaw vldt_ -> txBodyRaw {atbrValidityInterval = vldt_}
  {-# INLINEABLE vldtTxBodyL #-}

instance MaryEraTxBody AlonzoEra where
  mintTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrMint} -> atbrMint) $
      \txBodyRaw mint_ -> txBodyRaw {atbrMint = mint_}
  {-# INLINEABLE mintTxBodyL #-}

instance AlonzoEraTxBody AlonzoEra where
  collateralInputsTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrCollateral} -> atbrCollateral) $
      \txBodyRaw collateral_ -> txBodyRaw {atbrCollateral = collateral_}
  {-# INLINEABLE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrReqSignerHashes} -> atbrReqSignerHashes) $
      \txBodyRaw reqSignerHashes_ -> txBodyRaw {atbrReqSignerHashes = reqSignerHashes_}
  {-# INLINEABLE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrScriptIntegrityHash} -> atbrScriptIntegrityHash) $
      \txBodyRaw scriptIntegrityHash_ -> txBodyRaw {atbrScriptIntegrityHash = scriptIntegrityHash_}
  {-# INLINEABLE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL =
    lensMemoRawType @AlonzoEra (\AlonzoTxBodyRaw {atbrTxNetworkId} -> atbrTxNetworkId) $
      \txBodyRaw networkId -> txBodyRaw {atbrTxNetworkId = networkId}
  {-# INLINEABLE networkIdTxBodyL #-}

  redeemerPointer = alonzoRedeemerPointer

  redeemerPointerInverse = alonzoRedeemerPointerInverse

deriving newtype instance Eq (TxBody l AlonzoEra)

deriving instance Typeable l => NoThunks (TxBody l AlonzoEra)

deriving instance NFData (TxBody l AlonzoEra)

deriving instance Show (TxBody l AlonzoEra)

deriving via
  Mem (AlonzoTxBodyRaw l AlonzoEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l AlonzoEra))

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
  TxBody TopTx AlonzoEra
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

type instance MemoHashIndex (AlonzoTxBodyRaw l era) = EraIndependentTxBody

instance HashAnnotated (TxBody l AlonzoEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

instance EqRaw (TxBody l AlonzoEra)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
deriving newtype instance EncCBOR (TxBody l AlonzoEra)

instance EncCBOR (AlonzoTxBodyRaw l AlonzoEra) where
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

instance
  Typeable l =>
  DecCBOR (AlonzoTxBodyRaw l AlonzoEra)
  where
  decCBOR =
    fmap asSTxTopLevel . decode $
      SparseKeyed
        "AlonzoTxBodyRaw"
        (asSTxTopLevel emptyAlonzoTxBodyRaw)
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (AlonzoTxBodyRaw TopTx AlonzoEra)
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

instance Typeable l => DecCBOR (Annotator (AlonzoTxBodyRaw l AlonzoEra)) where
  decCBOR = pure <$> decCBOR

emptyAlonzoTxBodyRaw :: AlonzoTxBodyRaw TopTx era
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

emptyAlonzoTxBody :: Typeable l => TxBody l AlonzoEra
emptyAlonzoTxBody = asSTxTopLevel $ mkMemoizedEra @AlonzoEra emptyAlonzoTxBodyRaw

alonzoRedeemerPointer ::
  forall era l.
  MaryEraTxBody era =>
  TxBody l era ->
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
  TxBody l era ->
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
