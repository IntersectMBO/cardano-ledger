{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Babbage.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH, TxOutCompactDatum, TxOutCompactRefScript),
    utxoEntrySize,
    TxBody
      ( TxBody,
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
    Datum (..),
    datumDataHash,
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
    BabbageBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
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
    serializeEncoding,
  )
import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data
  ( AuxiliaryDataHash (..),
    BinaryData,
    Data,
    DataHash,
    binaryDataToData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( Addr28Extra,
    DataHash32,
    decodeAddress28,
    decodeDataHash32,
    encodeAddress28,
    encodeDataHash32,
    getAdaOnly,
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentScriptIntegrity,
    EraIndependentTxBody,
  )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (Value (..), policies, policyID)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    Val (..),
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero,
  )
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as LBS
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (..), Interns, fromNotSharedCBOR, interns)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data TxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      {-# UNPACK #-} !Word64
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(DataHash (Crypto era))
      {-# UNPACK #-} !Word64
  | TxOutCompactDatum
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
      {-# UNPACK #-} !Word64
  | TxOutCompactRefScript
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(Datum era)
      !(Core.Script era)
      {-# UNPACK #-} !Word64
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !Word64
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
      {-# UNPACK #-} !Word64

utxoEntrySize :: TxOut era -> Integer
utxoEntrySize (TxOutCompact' _ _ s) = fromIntegral s
utxoEntrySize (TxOutCompactDH' _ _ _ s) = fromIntegral s
utxoEntrySize (TxOutCompactDatum _ _ _ s) = fromIntegral s
utxoEntrySize (TxOutCompactRefScript _ _ _ _ s) = fromIntegral s
utxoEntrySize (TxOut_AddrHash28_AdaOnly _ _ _ s) = fromIntegral s
utxoEntrySize (TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ _ s) = fromIntegral s

deriving stock instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

-- | Already in NF
instance NFData (TxOut era) where
  rnf = rwhnf

viewCompactTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Core.Value era), Datum era, StrictMaybe (Core.Script era))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val _ -> (addr, val, NoDatum, SNothing)
  TxOutCompactDH' addr val dh _ -> (addr, val, DatumHash dh, SNothing)
  TxOutCompactDatum addr val datum _ -> (addr, val, Datum datum, SNothing)
  TxOutCompactRefScript addr val datum rs _ -> (addr, val, datum, SJust rs)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal _ ->
    let (a, b, c) = Alonzo.viewCompactTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
     in (a, b, toDatum c, SNothing)
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 _ ->
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
  Era era =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, Datum era, StrictMaybe (Core.Script era))
viewTxOut (TxOutCompact' bs c _) = (addr, val, NoDatum, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh _) = (addr, val, DatumHash dh, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDatum bs c d _) = (addr, val, Datum d, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactRefScript bs c d rs _) = (addr, val, d, SJust rs)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal _) = (addr, val, NoDatum, SNothing)
  where
    (addr, val, _) =
      Alonzo.viewTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 _) =
  case mDataHash of
    SNothing -> (addr, val, NoDatum, SNothing)
    SJust dh -> (addr, val, DatumHash dh, SNothing)
  where
    (addr, val, mDataHash) =
      Alonzo.viewTxOut @era $
        Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32

instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

data Datum era
  = NoDatum
  | DatumHash !(DataHash (Crypto era))
  | Datum !(BinaryData era)
  deriving (Eq, Ord, Show)

instance Era era => ToCBOR (Datum era) where
  toCBOR d = encode $ case d of
    NoDatum -> Sum NoDatum 0
    DatumHash dh -> Sum DatumHash 1 !> To dh
    Datum d' -> Sum Datum 2 !> To d'

instance Era era => FromCBOR (Datum era) where
  fromCBOR = decode (Summands "Datum" decodeDatum)
    where
      decodeDatum 0 = SumD NoDatum
      decodeDatum 1 = SumD DatumHash <! From
      decodeDatum 2 = SumD Datum <! From
      decodeDatum k = Invalid k

datumDataHash :: Datum era -> StrictMaybe (DataHash (Crypto era))
datumDataHash = \case
  NoDatum -> SNothing
  (DatumHash dh) -> SJust dh
  (Datum _) -> SNothing

pattern TxOut ::
  forall era.
  ( Era era,
    Compactible (Core.Value era),
    Val (Core.Value era),
    ToCBOR (Core.Script era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  TxOut era
pattern TxOut addr vl datum refScript <-
  (viewTxOut -> (addr, vl, datum, refScript))
  where
    TxOut a v d r = mkTxOut a v d r s
      where
        cv = fromMaybe (error "Illegal value in txout") $ toCompact v
        s = encodedTxOutSize (compactAddr a) cv d r

{-# COMPLETE TxOut #-}

pattern TxOutCompact ::
  forall era.
  ( Era era,
    ToCBOR (Core.Script era),
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  TxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, NoDatum, SNothing))
  where
    TxOutCompact cAddr cVal
      | isAdaOnlyCompact cVal = TxOut (decompactAddr cAddr) (fromCompact cVal) NoDatum SNothing
      | otherwise = TxOutCompact' cAddr cVal (encodedTxOutSize @era cAddr cVal NoDatum SNothing)

pattern TxOutCompactDH ::
  forall era.
  ( Era era,
    ToCBOR (Core.Script era),
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  DataHash (Crypto era) ->
  TxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, DatumHash dh, SNothing))
  where
    TxOutCompactDH cAddr cVal dh
      | isAdaOnlyCompact cVal = TxOut (decompactAddr cAddr) (fromCompact cVal) (DatumHash dh) SNothing
      | otherwise = TxOutCompactDH' cAddr cVal dh (encodedTxOutSize @era cAddr cVal (DatumHash dh) SNothing)

mkTxOut ::
  forall era.
  Era era =>
  Addr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  Word64 ->
  TxOut era
mkTxOut (Addr network paymentCred stakeRef) vl NoDatum SNothing osize
  | StakeRefBase stakeCred <- stakeRef,
    Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
      TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact osize
mkTxOut (Addr network paymentCred stakeRef) vl (DatumHash dh) SNothing osize
  | StakeRefBase stakeCred <- stakeRef,
    Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
    Just (Refl, dataHash32) <- encodeDataHash32 dh =
      TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32 osize
mkTxOut addr vl d rs osize =
  let v = fromMaybe (error "Illegal value in txout") $ toCompact vl
      a = compactAddr addr
   in case rs of
        SNothing -> case d of
          NoDatum -> TxOutCompact' a v osize
          DatumHash dh -> TxOutCompactDH' a v dh osize
          Datum binaryData -> TxOutCompactDatum a v binaryData osize
        SJust rs' -> TxOutCompactRefScript a v d rs' osize

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _spendInputs :: !(Set (TxIn (Crypto era))),
    _collateralInputs :: !(Set (TxIn (Crypto era))),
    _referenceInputs :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (TxOut era)),
    _collateralReturn :: !(StrictMaybe (TxOut era)),
    _totalCollateral :: !(StrictMaybe Coin),
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    _mint :: !(Value (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.Value, not a Core.Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    CC.Crypto (Crypto era),
    Compactible (Core.Value era),
    Eq (PParamsDelta era)
  ) =>
  Eq (TxBodyRaw era)

instance
  (Typeable era, NoThunks (Core.Value era), NoThunks (PParamsDelta era)) =>
  NoThunks (TxBodyRaw era)

deriving instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBodyRaw era)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  ( CC.Crypto (Crypto era)
  ) =>
  Eq (TxBody era)

deriving instance
  ( Typeable era,
    NoThunks (Core.Value era),
    NoThunks (PParamsDelta era)
  ) =>
  NoThunks (TxBody era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    ( Era era,
      Typeable (Core.Script era),
      Typeable (Core.AuxiliaryData era),
      Compactible (Core.Value era),
      Show (Core.Value era),
      DecodeNonNegative (Core.Value era),
      FromCBOR (Annotator (Core.Script era)),
      ToCBOR (Core.Script era),
      Core.SerialisableData (PParamsDelta era)
    ) =>
    FromCBOR (Annotator (TxBody era))

instance
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (Core.Script era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

-- The Set of constraints necessary to use the TxBody pattern
type BabbageBody era =
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era),
    Core.SerialisableData (PParamsDelta era)
  )

pattern TxBody ::
  BabbageBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictMaybe (TxOut era) ->
  StrictMaybe Coin ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Value (Crypto era) ->
  StrictMaybe (ScriptIntegrityHash (Crypto era)) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  StrictMaybe Network ->
  TxBody era
pattern TxBody
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
    TxBody
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
        TxBodyConstr $
          memoBytes
            ( encodeTxBodyRaw $
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
            )

{-# COMPLETE TxBody #-}

instance (c ~ Crypto era) => HashAnnotated (TxBody era) EraIndependentTxBody c

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: TxBody era -> Set (TxIn (Crypto era))
collateralInputs' :: TxBody era -> Set (TxIn (Crypto era))
referenceInputs' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
collateralReturn' :: TxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: TxBody era -> StrictMaybe Coin
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> Value (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
spendInputs' (TxBodyConstr (Memo raw _)) = _spendInputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

collateralInputs' (TxBodyConstr (Memo raw _)) = _collateralInputs raw

referenceInputs' (TxBodyConstr (Memo raw _)) = _referenceInputs raw

outputs' (TxBodyConstr (Memo raw _)) = _outputs raw

collateralReturn' (TxBodyConstr (Memo raw _)) = _collateralReturn raw

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
  (Era era, ToCBOR (Core.Script era)) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  Encoding
encodeTxOut addr val datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To addr)
      !> Key 1 (To val)
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script

encodedTxOutSize ::
  forall era.
  (Era era, ToCBOR (Core.Script era)) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  Word64
encodedTxOutSize addr val datum script =
  fromIntegral . LBS.length . serializeEncoding $ encodeTxOut addr val datum script

data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe (Addr (Crypto era))),
    decodingTxOutVal :: !(Core.Value era),
    decodingTxOutDatum :: !(Datum era),
    decodingTxOutScript :: !(StrictMaybe (Core.Script era))
  }
  deriving (Typeable)

{-# INLINE decodeTxOut #-}
decodeTxOut ::
  forall s era.
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Core.Script era),
    DecodeNonNegative (Core.Value era)
  ) =>
  Decoder s (TxOut era)
decodeTxOut = do
  dtxo <- decode $ SparseKeyed "TxOut" initial bodyFields requiredFields
  case dtxo of
    DecodingTxOut SNothing _ _ _ -> cborError $ DecoderErrorCustom "txout" "impossible: no addr"
    DecodingTxOut (SJust addr) val d script -> pure $ TxOut addr val d script
  where
    initial :: DecodingTxOut era
    initial =
      DecodingTxOut SNothing mempty NoDatum SNothing
    bodyFields :: (Word -> Field (DecodingTxOut era))
    bodyFields 0 =
      field
        (\x txo -> txo {decodingTxOutAddr = SJust x})
        (D fromCBOR)
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
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr cv) = encodeTxOut @era addr cv NoDatum SNothing
  toCBOR (TxOutCompactDatum addr cv d _) = encodeTxOut addr cv (Datum d) SNothing
  toCBOR (TxOutCompactRefScript addr cv d rs _) = encodeTxOut addr cv d (SJust rs)
  toCBOR (TxOutCompactDH addr cv dh) = encodeTxOut @era addr cv (DatumHash dh) SNothing

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = fromNotSharedCBOR

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  FromSharedCBOR (TxOut era)
  where
  type Share (TxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns = do
    peekTokenType >>= \case
      TypeMapLenIndef -> decodeTxOut
      TypeMapLen -> decodeTxOut
      _ -> oldTxOut
    where
      oldTxOut = do
        lenOrIndef <- decodeListLenOrIndef
        let internTxOut = \case
              TxOut_AddrHash28_AdaOnly cred addr28Extra ada osize ->
                TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada osize
              TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 osize ->
                TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32 osize
              txOut -> txOut
        internTxOut <$!> case lenOrIndef of
          Nothing -> do
            a <- fromCBOR
            cv <- decodeNonNegative
            decodeBreakOr >>= \case
              True -> pure $ TxOutCompact a cv
              False -> do
                dh <- fromCBOR
                decodeBreakOr >>= \case
                  True -> pure $ TxOutCompactDH a cv dh
                  False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
          Just 2 ->
            TxOutCompact
              <$> fromCBOR
              <*> decodeNonNegative
          Just 3 ->
            TxOutCompactDH
              <$> fromCBOR
              <*> decodeNonNegative
              <*> fromCBOR
          Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (PParamsDelta era),
    ToCBOR (Core.Script era)
  ) =>
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
      !> Key 13 (E encodeFoldable _collateralInputs)
      !> Key 18 (E encodeFoldable _referenceInputs)
      !> Key 1 (E encodeFoldable _outputs)
      !> encodeKeyedStrictMaybe 16 _collateralReturn
      !> encodeKeyedStrictMaybe 17 _totalCollateral
      !> Key 2 (To _txfee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (E encodeFoldable _certs))
      !> Omit (null . unWdrl) (Key 5 (To _wdrls))
      !> encodeKeyedStrictMaybe 6 _update
      !> encodeKeyedStrictMaybe 8 bot
      !> Key 14 (E encodeFoldable _reqSignerHashes)
      !> Omit isZero (Key 9 (E encodeMint _mint))
      !> encodeKeyedStrictMaybe 11 _scriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 _adHash
      !> encodeKeyedStrictMaybe 15 _txnetworkid

instance
  forall era.
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (Core.Script era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        initial
        bodyFields
        requiredFields
    where
      initial :: TxBodyRaw era
      initial =
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
          (D (decodeStrictSeq fromCBOR))
      bodyFields 16 =
        ofield
          (\x tx -> tx {_collateralReturn = x})
          From
      bodyFields 17 =
        field
          (\x tx -> tx {_totalCollateral = x})
          (D (SJust <$> fromCBOR))
      bodyFields 2 = field (\x tx -> tx {_txfee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        field
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 = field (\x tx -> tx {_wdrls = x}) From
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

-- ====================================================
-- HasField instances to be consistent with earlier Eras

instance (Crypto era ~ c) => HasField "inputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _spendInputs m

instance HasField "outputs" (TxBody era) (StrictSeq (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = _outputs m

instance Crypto era ~ crypto => HasField "certs" (TxBody era) (StrictSeq (DCert crypto)) where
  getField (TxBodyConstr (Memo m _)) = _certs m

instance Crypto era ~ crypto => HasField "wdrls" (TxBody era) (Wdrl crypto) where
  getField (TxBodyConstr (Memo m _)) = _wdrls m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = _txfee m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyConstr (Memo m _)) = _update m

instance
  (Crypto era ~ c) =>
  HasField "reqSignerHashes" (TxBody era) (Set (KeyHash 'Witness c))
  where
  getField (TxBodyConstr (Memo m _)) = _reqSignerHashes m

instance (Crypto era ~ c) => HasField "mint" (TxBody era) (Mary.Value c) where
  getField (TxBodyConstr (Memo m _)) = _mint m

instance (Crypto era ~ c) => HasField "collateral" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _collateralInputs m

instance (Crypto era ~ c) => HasField "referenceInputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _referenceInputs m

instance HasField "collateralReturn" (TxBody era) (StrictMaybe (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = _collateralReturn m

instance HasField "totalCollateral" (TxBody era) (StrictMaybe Coin) where
  getField (TxBodyConstr (Memo m _)) = _totalCollateral m

instance (Crypto era ~ c) => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField (TxBodyConstr (Memo m _)) = Set.map policyID (policies (_mint m))

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = _vldt m

instance
  c ~ Crypto era =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _adHash m

instance
  c ~ Crypto era =>
  HasField "scriptIntegrityHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _scriptIntegrityHash m

instance HasField "txnetworkid" (TxBody era) (StrictMaybe Network) where
  getField (TxBodyConstr (Memo m _)) = _txnetworkid m

instance (Era era, Core.Value era ~ val, Compactible val) => HasField "value" (TxOut era) val where
  getField = \case
    TxOutCompact' _ cv _ -> fromCompact cv
    TxOutCompactDH' _ cv _ _ -> fromCompact cv
    TxOutCompactDatum _ cv _ _ -> fromCompact cv
    TxOutCompactRefScript _ cv _ _ _ -> fromCompact cv
    TxOut_AddrHash28_AdaOnly _ _ cc _ -> inject (fromCompact cc)
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ _ -> inject (fromCompact cc)

instance (Era era, c ~ Crypto era) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField = maybeToStrictMaybe . txOutDataHash

instance (Era era) => HasField "datum" (TxOut era) (StrictMaybe (Data era)) where
  getField = maybeToStrictMaybe . txOutData

instance (Era era, s ~ Core.Script era) => HasField "referenceScript" (TxOut era) (StrictMaybe s) where
  getField = maybeToStrictMaybe . txOutScript

getBabbageTxOutEitherAddr ::
  HashAlgorithm (CC.ADDRHASH (Crypto era)) =>
  TxOut era ->
  Either (Addr (Crypto era)) (CompactAddr (Crypto era))
getBabbageTxOutEitherAddr = \case
  TxOutCompact' cAddr _ _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ _ -> Right cAddr
  TxOutCompactRefScript cAddr _ _ _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address or a hash of non-standard size"

txOutData :: TxOut era -> Maybe (Data era)
txOutData = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' {} -> Nothing
  TxOutCompactDatum _ _ binaryData _ -> Just $! binaryDataToData binaryData
  TxOutCompactRefScript _ _ (Datum binaryData) _ _ -> Just $! binaryDataToData binaryData
  TxOutCompactRefScript _ _ _ _ _ -> Nothing
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> Nothing

-- | Return the data hash of a given transaction output, if one is present.
--  Note that this function does *not* return the hash of any inline datums
--  that are present.
txOutDataHash :: Era era => TxOut era -> Maybe (DataHash (Crypto era))
txOutDataHash = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' _ _ dh _ -> Just dh
  TxOutCompactDatum _ _ _ _ -> Nothing
  TxOutCompactRefScript _ _ datum _ _ ->
    case datum of
      NoDatum -> Nothing
      DatumHash dh -> Just dh
      Datum _d -> Nothing
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 _ -> decodeDataHash32 dataHash32

txOutScript :: TxOut era -> Maybe (Core.Script era)
txOutScript = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' {} -> Nothing
  TxOutCompactDatum {} -> Nothing
  TxOutCompactRefScript _ _ _ s _ -> Just s
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> Nothing
