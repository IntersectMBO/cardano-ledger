{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
    getBabbageTxOutEitherAddr,
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
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    TokenType (..),
    decodeAnnotator,
    decodeBreakOr,
    decodeListLenOrIndef,
    decodeNestedCborBytes,
    encodeNestedCbor,
    peekTokenType,
  )
import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data
  ( AuxiliaryDataHash (..),
    BinaryData,
    Data,
    Datum (..),
    binaryDataToData,
    dataToBinaryData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( Addr28Extra,
    AlonzoEraTxOut (..),
    DataHash32,
    decodeAddress28,
    decodeDataHash32,
    encodeAddress28,
    encodeDataHash32,
    getAdaOnly,
  )
import Cardano.Ledger.Alonzo.TxBody as AlonzoTxBodyReExports
  ( AlonzoEraTxBody (..),
    ShelleyEraTxBody (..),
    ShelleyMAEraTxBody (..),
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    compactAddr,
    decompactAddr,
    fromCborBackwardsBothAddr,
    fromCborBothAddr,
    fromCborRewardAcnt,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core (TxBody, TxOut)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset, policies, policyID)
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
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
  ( DecodeNonNegative (decodeNonNegative),
    Val (..),
    decodeMint,
    encodeMint,
  )
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as LBS
import Data.Coders hiding (to)
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (..), Interns, interns)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data BabbageTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
      !(DataHash (Crypto era))
  | TxOutCompactDatum
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
      !(Datum era)
      !(Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32

type TxOut era = BabbageTxOut era

{-# DEPRECATED TxOut "Use `BabbageTxOut` instead" #-}

instance CC.Crypto c => EraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance EraTxOut (BabbageEra CC.StandardCrypto) #-}

  type TxOut (BabbageEra c) = BabbageTxOut (BabbageEra c)

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

dataHashBabbageTxOutL ::
  EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (DataHash (Crypto era)))
dataHashBabbageTxOutL =
  lens
    getBabbageTxOutDataHash
    ( \(BabbageTxOut addr cv _ s) -> \case
        SNothing -> BabbageTxOut addr cv NoDatum s
        SJust dh -> BabbageTxOut addr cv (DatumHash dh) s
    )
{-# INLINE dataHashBabbageTxOutL #-}

instance CC.Crypto c => AlonzoEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (BabbageEra CC.StandardCrypto) #-}

  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINE dataHashTxOutL #-}

class (AlonzoEraTxOut era, EraScript era) => BabbageEraTxOut era where
  referenceScriptTxOutL :: Lens' (Core.TxOut era) (StrictMaybe (Script era))

  dataTxOutL :: Lens' (Core.TxOut era) (StrictMaybe (Data era))

  datumTxOutL :: Lens' (Core.TxOut era) (Datum era)

dataBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (Data era))
dataBabbageTxOutL =
  lens
    getBabbageTxOutData
    ( \(BabbageTxOut addr cv _ s) ->
        \case
          SNothing -> BabbageTxOut addr cv NoDatum s
          SJust d -> BabbageTxOut addr cv (Datum (dataToBinaryData d)) s
    )
{-# INLINE dataBabbageTxOutL #-}

datumBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (Datum era)
datumBabbageTxOutL =
  lens getBabbageTxOutDatum (\(BabbageTxOut addr cv _ s) d -> BabbageTxOut addr cv d s)
{-# INLINE datumBabbageTxOutL #-}

referenceScriptBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (Script era))
referenceScriptBabbageTxOutL =
  lens getBabbageTxOutScript (\(BabbageTxOut addr cv d _) s -> BabbageTxOut addr cv d s)
{-# INLINE referenceScriptBabbageTxOutL #-}

instance CC.Crypto c => BabbageEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (BabbageEra CC.StandardCrypto) #-}
  dataTxOutL = dataBabbageTxOutL
  {-# INLINE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINE referenceScriptTxOutL #-}

addrEitherBabbageTxOutL ::
  EraTxOut era =>
  Lens' (BabbageTxOut era) (Either (Addr (Crypto era)) (CompactAddr (Crypto era)))
addrEitherBabbageTxOutL =
  lens
    getBabbageTxOutEitherAddr
    ( \txOut eAddr ->
        let cVal = getTxOutCompactValue txOut
            (_, _, datum, mScript) = viewTxOut txOut
         in case eAddr of
              Left addr -> mkTxOutCompact addr (compactAddr addr) cVal datum mScript
              Right cAddr -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal datum mScript
    )
{-# INLINE addrEitherBabbageTxOutL #-}

valueEitherBabbageTxOutL ::
  forall era.
  EraTxOut era =>
  Lens' (BabbageTxOut era) (Either (Value era) (CompactForm (Value era)))
valueEitherBabbageTxOutL =
  lens
    (Right . getTxOutCompactValue)
    ( \txOut eVal ->
        let (cAddr, _, datum, mScript) = viewCompactTxOut txOut
         in case eVal of
              Left val -> mkTxOut (decompactAddr cAddr) cAddr val datum mScript
              Right cVal -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal datum mScript
    )
{-# INLINE valueEitherBabbageTxOutL #-}

deriving stock instance
  (Era era, Eq (Script era), Eq (CompactForm (Value era))) =>
  Eq (BabbageTxOut era)

-- | Already in NF
instance NFData (BabbageTxOut era) where
  rnf = rwhnf

viewCompactTxOut ::
  forall era.
  EraTxOut era =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Value era), Datum era, StrictMaybe (Script era))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, NoDatum, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, DatumHash dh, SNothing)
  TxOutCompactDatum addr val datum -> (addr, val, Datum datum, SNothing)
  TxOutCompactRefScript addr val datum rs -> (addr, val, datum, SJust rs)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal ->
    let (a, b, c) =
          Alonzo.viewCompactTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
     in (a, b, toDatum c, SNothing)
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 ->
    let (a, b, c) =
          Alonzo.viewCompactTxOut @era $
            Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32
     in (a, b, toDatum c, SNothing)
  where
    toDatum = \case
      SNothing -> NoDatum
      SJust dh -> DatumHash dh

viewTxOut ::
  forall era.
  (Era era, Val (Value era)) =>
  TxOut era ->
  (Addr (Crypto era), Value era, Datum era, StrictMaybe (Script era))
viewTxOut (TxOutCompact' bs c) = (addr, val, NoDatum, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, DatumHash dh, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDatum bs c d) = (addr, val, Datum d, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactRefScript bs c d rs) = (addr, val, d, SJust rs)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal) = (addr, val, NoDatum, SNothing)
  where
    (addr, val, _) =
      Alonzo.viewTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32) =
  case mDataHash of
    SNothing -> (addr, val, NoDatum, SNothing)
    SJust dh -> (addr, val, DatumHash dh, SNothing)
  where
    (addr, val, mDataHash) =
      Alonzo.viewTxOut @era $
        Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32

instance
  (Era era, Show (Value era), Show (Script era), Val (Value era)) =>
  Show (BabbageTxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "BabbageTxOut" (BabbageTxOut era) instance NoThunks (BabbageTxOut era)

pattern BabbageTxOut ::
  (Era era, Val (Value era), HasCallStack) =>
  Addr (Crypto era) ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
pattern BabbageTxOut addr vl datum refScript <-
  (viewTxOut -> (addr, vl, datum, refScript))
  where
    BabbageTxOut addr vl datum refScript = mkTxOut addr (compactAddr addr) vl datum refScript

{-# COMPLETE BabbageTxOut #-}

-- | Helper function for constructing a BabbageTxOut. Both compacted and uncompacted
-- address should be the exact same address in different forms.
mkTxOut ::
  forall era.
  (Era era, Val (Value era), HasCallStack) =>
  Addr (Crypto era) ->
  CompactAddr (Crypto era) ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOut addr _cAddr vl NoDatum SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
      TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
mkTxOut addr _cAddr vl (DatumHash dh) SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
    Just (Refl, dataHash32) <- encodeDataHash32 dh =
      TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
mkTxOut _addr cAddr vl d rs =
  let cVal = fromMaybe (error "Illegal value in txout") $ toCompact vl
   in case rs of
        SNothing -> case d of
          NoDatum -> TxOutCompact' cAddr cVal
          DatumHash dh -> TxOutCompactDH' cAddr cVal dh
          Datum binaryData -> TxOutCompactDatum cAddr cVal binaryData
        SJust rs' -> TxOutCompactRefScript cAddr cVal d rs'

-- TODO: Implement mkTxOut in terms of mkTxOutCompact, it will avoid unnecessary
-- MultiAsset serilization/deserialization
mkTxOutCompact ::
  (Era era, Val (Value era)) =>
  Addr (Crypto era) ->
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOutCompact addr cAddr cVal = mkTxOut addr cAddr (fromCompact cVal)

pattern TxOutCompact ::
  (EraTxOut era, HasCallStack) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  BabbageTxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, NoDatum, SNothing))
  where
    TxOutCompact cAddr cVal
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) NoDatum SNothing
      | otherwise = TxOutCompact' cAddr cVal

pattern TxOutCompactDH ::
  (EraTxOut era, HasCallStack) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  DataHash (Crypto era) ->
  BabbageTxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, DatumHash dh, SNothing))
  where
    TxOutCompactDH cAddr cVal dh
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) (DatumHash dh) SNothing
      | otherwise = TxOutCompactDH' cAddr cVal dh

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _spendInputs :: !(Set (TxIn (Crypto era))),
    _collateralInputs :: !(Set (TxIn (Crypto era))),
    _referenceInputs :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (Sized (BabbageTxOut era))),
    _collateralReturn :: !(StrictMaybe (Sized (BabbageTxOut era))),
    _totalCollateral :: !(StrictMaybe Coin),
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: !(Set (KeyHash 'Witness (Crypto era))),
    _mint :: !(MultiAsset (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  (Era era, Eq (Script era), Eq (PParamsUpdate era), Eq (CompactForm (Value era))) =>
  Eq (TxBodyRaw era)

instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (TxBodyRaw era)

instance (CC.Crypto (Crypto era), NFData (PParamsUpdate era)) => NFData (TxBodyRaw era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (TxBodyRaw era)

newtype BabbageTxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  (CC.Crypto (Crypto era), NFData (PParamsUpdate era)) =>
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
{-# INLINE lensTxBodyRaw #-}

inputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (Crypto era)))
inputsBabbageTxBodyL =
  lensTxBodyRaw _spendInputs (\txBodyRaw inputs_ -> txBodyRaw {_spendInputs = inputs_})
{-# INLINE inputsBabbageTxBodyL #-}

outputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (TxOut era))
outputsBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . _outputs)
    (\txBodyRaw outputs_ -> txBodyRaw {_outputs = mkSized <$> outputs_})
{-# INLINE outputsBabbageTxBodyL #-}

feeBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) Coin
feeBabbageTxBodyL =
  lensTxBodyRaw _txfee (\txBodyRaw fee_ -> txBodyRaw {_txfee = fee_})
{-# INLINE feeBabbageTxBodyL #-}

auxDataHashBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (AuxiliaryDataHash (Crypto era)))
auxDataHashBabbageTxBodyL =
  lensTxBodyRaw _adHash (\txBodyRaw auxDataHash -> txBodyRaw {_adHash = auxDataHash})
{-# INLINE auxDataHashBabbageTxBodyL #-}

allInputsBabbageTxBodyF ::
  BabbageEraTxBody era => SimpleGetter (BabbageTxBody era) (Set (TxIn (Crypto era)))
allInputsBabbageTxBodyF =
  to $ \txBody ->
    (txBody ^. inputsBabbageTxBodyL)
      `Set.union` (txBody ^. collateralInputsBabbageTxBodyL)
      `Set.union` (txBody ^. referenceInputsBabbageTxBodyL)
{-# INLINE allInputsBabbageTxBodyF #-}

mintedBabbageTxBodyF :: SimpleGetter (BabbageTxBody era) (Set (ScriptHash (Crypto era)))
mintedBabbageTxBodyF =
  to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (_mint txBodyRaw)))
{-# INLINE mintedBabbageTxBodyF #-}

wdrlsBabbbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (Wdrl (Crypto era))
wdrlsBabbbageTxBodyL =
  lensTxBodyRaw _wdrls (\txBodyRaw wdrls_ -> txBodyRaw {_wdrls = wdrls_})
{-# INLINE wdrlsBabbbageTxBodyL #-}

updateBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe (Update era))
updateBabbageTxBodyL =
  lensTxBodyRaw _update (\txBodyRaw update_ -> txBodyRaw {_update = update_})
{-# INLINE updateBabbageTxBodyL #-}

certsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictSeq (DCert (Crypto era)))
certsBabbageTxBodyL =
  lensTxBodyRaw _certs (\txBodyRaw certs_ -> txBodyRaw {_certs = certs_})
{-# INLINE certsBabbageTxBodyL #-}

vldtBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) ValidityInterval
vldtBabbageTxBodyL =
  lensTxBodyRaw _vldt (\txBodyRaw vldt_ -> txBodyRaw {_vldt = vldt_})
{-# INLINE vldtBabbageTxBodyL #-}

mintBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (MultiAsset (Crypto era))
mintBabbageTxBodyL =
  lensTxBodyRaw _mint (\txBodyRaw mint_ -> txBodyRaw {_mint = mint_})
{-# INLINE mintBabbageTxBodyL #-}

mintValueBabbageTxBodyF ::
  (BabbageEraTxBody era, Value era ~ MaryValue (Crypto era)) =>
  SimpleGetter (BabbageTxBody era) (Value era)
mintValueBabbageTxBodyF = mintBabbageTxBodyL . to (MaryValue 0)

collateralInputsBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (TxIn (Crypto era)))
collateralInputsBabbageTxBodyL =
  lensTxBodyRaw
    _collateralInputs
    (\txBodyRaw collateral_ -> txBodyRaw {_collateralInputs = collateral_})
{-# INLINE collateralInputsBabbageTxBodyL #-}

reqSignerHashesBabbageTxBodyL ::
  BabbageEraTxBody era => Lens' (BabbageTxBody era) (Set (KeyHash 'Witness (Crypto era)))
reqSignerHashesBabbageTxBodyL =
  lensTxBodyRaw
    _reqSignerHashes
    (\txBodyRaw reqSignerHashes_ -> txBodyRaw {_reqSignerHashes = reqSignerHashes_})
{-# INLINE reqSignerHashesBabbageTxBodyL #-}

scriptIntegrityHashBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (ScriptIntegrityHash (Crypto era)))
scriptIntegrityHashBabbageTxBodyL =
  lensTxBodyRaw
    _scriptIntegrityHash
    (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {_scriptIntegrityHash = scriptIntegrityHash_})
{-# INLINE scriptIntegrityHashBabbageTxBodyL #-}

networkIdBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Network)
networkIdBabbageTxBodyL =
  lensTxBodyRaw _txnetworkid (\txBodyRaw networkId -> txBodyRaw {_txnetworkid = networkId})
{-# INLINE networkIdBabbageTxBodyL #-}

sizedOutputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictSeq (Sized (BabbageTxOut era)))
sizedOutputsBabbageTxBodyL =
  lensTxBodyRaw _outputs (\txBodyRaw outputs_ -> txBodyRaw {_outputs = outputs_})
{-# INLINE sizedOutputsBabbageTxBodyL #-}

referenceInputsBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (Set (TxIn (Crypto era)))
referenceInputsBabbageTxBodyL =
  lensTxBodyRaw
    _referenceInputs
    (\txBodyRaw reference_ -> txBodyRaw {_referenceInputs = reference_})
{-# INLINE referenceInputsBabbageTxBodyL #-}

totalCollateralBabbageTxBodyL :: BabbageEraTxBody era => Lens' (BabbageTxBody era) (StrictMaybe Coin)
totalCollateralBabbageTxBodyL =
  lensTxBodyRaw
    _totalCollateral
    (\txBodyRaw totalCollateral_ -> txBodyRaw {_totalCollateral = totalCollateral_})
{-# INLINE totalCollateralBabbageTxBodyL #-}

collateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (BabbageTxOut era))
collateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    (fmap sizedValue . _collateralReturn)
    (\txBodyRaw collateralReturn_ -> txBodyRaw {_collateralReturn = mkSized <$> collateralReturn_})
{-# INLINE collateralReturnBabbageTxBodyL #-}

sizedCollateralReturnBabbageTxBodyL ::
  BabbageEraTxBody era =>
  Lens' (BabbageTxBody era) (StrictMaybe (Sized (BabbageTxOut era)))
sizedCollateralReturnBabbageTxBodyL =
  lensTxBodyRaw
    _collateralReturn
    (\txBodyRaw collateralReturn_ -> txBodyRaw {_collateralReturn = collateralReturn_})
{-# INLINE sizedCollateralReturnBabbageTxBodyL #-}

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

  referenceInputsTxBodyL :: Lens' (Core.TxBody era) (Set (TxIn (Crypto era)))

  totalCollateralTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe Coin)

  collateralReturnTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe (BabbageTxOut era))

  sizedCollateralReturnTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe (Sized (BabbageTxOut era)))

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

type TxBody era = BabbageTxBody era

{-# DEPRECATED TxBody "Use `BabbageTxBody` instead" #-}

deriving newtype instance CC.Crypto (Crypto era) => Eq (BabbageTxBody era)

deriving instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (BabbageTxBody era)

deriving instance
  (Era era, Val (Value era), Show (Value era), Show (Script era), Show (PParamsUpdate era)) =>
  Show (BabbageTxBody era)

deriving via
  (Mem (TxBodyRaw era))
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
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (Sized (BabbageTxOut era)) ->
  StrictMaybe (Sized (BabbageTxOut era)) ->
  StrictMaybe Coin ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (Crypto era)) ->
  MultiAsset (Crypto era) ->
  StrictMaybe (ScriptIntegrityHash (Crypto era)) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
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

instance (c ~ Crypto era) => HashAnnotated (BabbageTxBody era) EraIndependentTxBody c

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: TxBody era -> Set (TxIn (Crypto era))
collateralInputs' :: TxBody era -> Set (TxIn (Crypto era))
referenceInputs' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (BabbageTxOut era)
collateralReturn' :: TxBody era -> StrictMaybe (BabbageTxOut era)
totalCollateral' :: TxBody era -> StrictMaybe Coin
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> MultiAsset (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
spendInputs' (TxBodyConstr (Memo raw _)) = _spendInputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

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

{-# INLINE encodeTxOut #-}
encodeTxOut ::
  forall era.
  (Era era, ToCBOR (Value era), ToCBOR (Script era)) =>
  CompactAddr (Crypto era) ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  Encoding
encodeTxOut addr val datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To addr)
      !> Key 1 (To val)
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script

data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe (Addr (Crypto era), CompactAddr (Crypto era))),
    decodingTxOutVal :: !(Value era),
    decodingTxOutDatum :: !(Datum era),
    decodingTxOutScript :: !(StrictMaybe (Script era))
  }

{-# INLINE decodeTxOut #-}
decodeTxOut ::
  forall s era.
  (Era era, Val (Value era), DecodeNonNegative (Value era), FromCBOR (Annotator (Script era))) =>
  (forall s'. Decoder s' (Addr (Crypto era), CompactAddr (Crypto era))) ->
  Decoder s (BabbageTxOut era)
decodeTxOut decAddr = do
  dtxo <- decode $ SparseKeyed "TxOut" initial bodyFields requiredFields
  case dtxo of
    DecodingTxOut SNothing _ _ _ -> cborError $ DecoderErrorCustom "BabbageTxOut" "Impossible: no Addr"
    DecodingTxOut (SJust (addr, cAddr)) val d script -> pure $ mkTxOut addr cAddr val d script
  where
    initial :: DecodingTxOut era
    initial =
      DecodingTxOut SNothing mempty NoDatum SNothing
    bodyFields :: (Word -> Field (DecodingTxOut era))
    bodyFields 0 =
      field
        (\x txo -> txo {decodingTxOutAddr = SJust x})
        (D decAddr)
    bodyFields 1 =
      field
        (\x txo -> txo {decodingTxOutVal = x})
        (D decodeNonNegative)
    bodyFields 2 =
      field
        (\x txo -> txo {decodingTxOutDatum = x})
        (D fromCBOR)
    bodyFields 3 =
      ofield
        (\x txo -> txo {decodingTxOutScript = x})
        (D $ decodeCIC "Script")
    bodyFields n = field (\_ t -> t) (Invalid n)
    requiredFields =
      [ (0, "addr"),
        (1, "val")
      ]

decodeCIC :: (FromCBOR (Annotator b)) => T.Text -> Decoder s b
decodeCIC s = do
  lbs <- decodeNestedCborBytes
  case decodeAnnotator s fromCBOR (LBS.fromStrict lbs) of
    Left e -> fail $ T.unpack s <> ": " <> show e
    Right x -> pure x

instance
  (Era era, Val (Value era), ToCBOR (Value era), ToCBOR (Script era)) =>
  ToCBOR (BabbageTxOut era)
  where
  toCBOR (BabbageTxOut addr v d s) = encodeTxOut (compactAddr addr) v d s

-- FIXME: ^ Starting with Babbage we need to reserialize all Addresses.  It is
-- safe to reserialize an address, because we do not rely on this instance for
-- computing a hash of a transaction and it is only used in storing TxOuts in
-- the ledger state.
--
-- After Vasil Hardfork we can switch it back to a more efficient version below:
--
-- toCBOR (TxOutCompact addr cv) = encodeTxOut @era addr cv NoDatum SNothing
-- toCBOR (TxOutCompactDH addr cv dh) = encodeTxOut @era addr cv (DatumHash dh) SNothing
-- toCBOR (TxOutCompactDatum addr cv d) = encodeTxOut addr cv (Datum d) SNothing
-- toCBOR (TxOutCompactRefScript addr cv d rs) = encodeTxOut addr cv d (SJust rs)

instance
  ( Era era,
    Val (Value era),
    FromCBOR (Annotator (Script era)),
    DecodeNonNegative (Value era)
  ) =>
  FromCBOR (BabbageTxOut era)
  where
  fromCBOR = fromCborTxOutWithAddr fromCborBackwardsBothAddr

instance
  ( Era era,
    Val (Value era),
    FromCBOR (Annotator (Script era)),
    DecodeNonNegative (Value era)
  ) =>
  FromSharedCBOR (BabbageTxOut era)
  where
  type Share (BabbageTxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns =
    internTxOut <$!> fromCborTxOutWithAddr fromCborBackwardsBothAddr
    where
      internTxOut = \case
        TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
          TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
        TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
          TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
        txOut -> txOut

fromCborTxOutWithAddr ::
  (Era era, Val (Value era), FromCBOR (Annotator (Script era)), DecodeNonNegative (Value era)) =>
  (forall s'. Decoder s' (Addr (Crypto era), CompactAddr (Crypto era))) ->
  Decoder s (BabbageTxOut era)
fromCborTxOutWithAddr decAddr = do
  peekTokenType >>= \case
    TypeMapLenIndef -> decodeTxOut decAddr
    TypeMapLen -> decodeTxOut decAddr
    _ -> oldTxOut
  where
    oldTxOut = do
      lenOrIndef <- decodeListLenOrIndef
      case lenOrIndef of
        Nothing -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          decodeBreakOr >>= \case
            True -> pure $ mkTxOut a ca v NoDatum SNothing
            False -> do
              dh <- fromCBOR
              decodeBreakOr >>= \case
                True -> pure $ mkTxOut a ca v (DatumHash dh) SNothing
                False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
        Just 2 -> do
          (a, ca) <- decAddr
          v <- decodeNonNegative
          pure $ mkTxOut a ca v NoDatum SNothing
        Just 3 -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          dh <- fromCBOR
          pure $ mkTxOut a ca v (DatumHash dh) SNothing
        Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

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

getBabbageTxOutEitherAddr ::
  HashAlgorithm (CC.ADDRHASH (Crypto era)) =>
  BabbageTxOut era ->
  Either (Addr (Crypto era)) (CompactAddr (Crypto era))
getBabbageTxOutEitherAddr = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOutCompactRefScript cAddr _ _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address or a hash of non-standard size"

-- TODO: Switch to using `getBabbageTxOutDatum`
getBabbageTxOutData :: BabbageTxOut era -> StrictMaybe (Data era)
getBabbageTxOutData = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' {} -> SNothing
  TxOutCompactDatum _ _ binaryData -> SJust $ binaryDataToData binaryData
  TxOutCompactRefScript _ _ datum _
    | Datum binaryData <- datum -> SJust $ binaryDataToData binaryData
    | otherwise -> SNothing
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> SNothing

-- TODO: Switch to using `getBabbageTxOutDatum`

-- | Return the data hash of a given transaction output, if one is present.
--  Note that this function does *not* return the hash of an inline datum
--  if one is present.
getBabbageTxOutDataHash :: Era era => BabbageTxOut era -> StrictMaybe (DataHash (Crypto era))
getBabbageTxOutDataHash = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' _ _ dh -> SJust dh
  TxOutCompactDatum {} -> SNothing
  TxOutCompactRefScript _ _ datum _ ->
    case datum of
      NoDatum -> SNothing
      DatumHash dh -> SJust dh
      Datum _d -> SNothing
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 ->
    maybeToStrictMaybe $ decodeDataHash32 dataHash32

getBabbageTxOutScript :: BabbageTxOut era -> StrictMaybe (Script era)
getBabbageTxOutScript = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' {} -> SNothing
  TxOutCompactDatum {} -> SNothing
  TxOutCompactRefScript _ _ _ s -> SJust s
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> SNothing

getBabbageTxOutDatum :: Era era => BabbageTxOut era -> Datum era
getBabbageTxOutDatum = \case
  TxOutCompact' {} -> NoDatum
  TxOutCompactDH' _ _ dh -> DatumHash dh
  TxOutCompactDatum _ _ binaryData -> Datum binaryData
  TxOutCompactRefScript _ _ datum _ -> datum
  TxOut_AddrHash28_AdaOnly {} -> NoDatum
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32
    | Just dh <- decodeDataHash32 dataHash32 -> DatumHash dh
    | otherwise -> error "Impossible: Compacted a hash of non-standard size"

getTxOutCompactValue :: EraTxOut era => BabbageTxOut era -> CompactForm (Value era)
getTxOutCompactValue =
  \case
    TxOutCompact' _ cv -> cv
    TxOutCompactDH' _ cv _ -> cv
    TxOutCompactDatum _ cv _ -> cv
    TxOutCompactRefScript _ cv _ _ -> cv
    TxOut_AddrHash28_AdaOnly _ _ cc -> injectCompact cc
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ -> injectCompact cc

txOutData :: BabbageTxOut era -> Maybe (Data era)
txOutData = strictMaybeToMaybe . getBabbageTxOutData
{-# DEPRECATED txOutData "In favor of `dataTxOutL` or `getBabbageTxOutData`" #-}

txOutDataHash :: BabbageTxOut era -> Maybe (Data era)
txOutDataHash = strictMaybeToMaybe . getBabbageTxOutData
{-# DEPRECATED txOutDataHash "In favor of `dataHashTxOutL` or `getBabbageTxOutDataHash`" #-}

txOutScript :: BabbageTxOut era -> Maybe (Script era)
txOutScript = strictMaybeToMaybe . getBabbageTxOutScript
{-# DEPRECATED txOutScript "In favor of `dataTxOutL` or `getBabbageTxOutScript`" #-}
