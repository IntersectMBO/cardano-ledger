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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Babbage.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH, TxOutCompactDatum),
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
    datumDatahash,
    inputs',
    collateral',
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
    BabbageBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeBreakOr,
    decodeListLenOrIndef,
    encodeListLen,
  )
import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..), Data, DataHash, hashData)
import Cardano.Ledger.Alonzo.TxBody (decodeAddress28, decodeDataHash32, encodeAddress28, encodeDataHash32, getAdaOnly)
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    isSNothing,
  )
import Cardano.Ledger.Coin (Coin (..))
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
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr, compactAddr, decompactAddr)
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
import GHC.TypeLits
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data TxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(DataHash (Crypto era))
  | TxOutCompactDatum'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(Data era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash

deriving stock instance
  ( Eq (Core.Value era),
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
  (CompactAddr (Crypto era), CompactForm (Core.Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, SJust dh)
  TxOutCompactDatum' addr val datum -> (addr, val, SJust $ hashData datum)
  TxOut_AddrHash28_AdaOnly stakeRef a b c d adaVal
    | Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28) ->
      (compactAddr (decodeAddress28 stakeRef a b c d), toCompactValue adaVal, SNothing)
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef a b c d adaVal e f g h
    | Just Refl <- sameNat (Proxy @(SizeHash (CC.HASH (Crypto era)))) (Proxy @32),
      Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28) ->
      ( compactAddr (decodeAddress28 stakeRef a b c d),
        toCompactValue adaVal,
        SJust (decodeDataHash32 e f g h)
      )
  TxOut_AddrHash28_AdaOnly {} -> error "Impossible: Compacted and address or hash of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> error "Impossible: Compacted and address or hash of non-standard size"
  where
    toCompactValue :: CompactForm Coin -> CompactForm (Core.Value era)
    toCompactValue ada =
      fromMaybe (error "Failed to compact a `Coin` as `CompactForm (Core.Value era)`")
        . toCompact
        . inject
        $ fromCompact ada

viewTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, Datum era)
viewTxOut (TxOutCompact' bs c) = (addr, val, NoDatum)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, DatumHash dh)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDatum' bs c d) = (addr, val, Datum d)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef a b c d adaVal)
  | Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28) =
    let addr = decodeAddress28 stakeRef a b c d
     in (addr, inject (fromCompact adaVal), NoDatum)
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef a b c d adaVal e f g h)
  | Just Refl <- sameNat (Proxy @(SizeHash (CC.HASH (Crypto era)))) (Proxy @32),
    Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28) =
    let addr = decodeAddress28 stakeRef a b c d
     in (addr, inject (fromCompact adaVal), DatumHash (decodeDataHash32 e f g h))
viewTxOut (TxOut_AddrHash28_AdaOnly {}) = error "Impossible: Compacted and address or hash of non-standard size"
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 {}) = error "Impossible: Compacted and address or hash of non-standard size"

instance
  ( Era era,
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

data Datum era
  = NoDatum
  | DatumHash !(DataHash (Crypto era))
  | Datum !(Data era)
  deriving (Eq, Ord, Show)

datumDatahash :: Era era => Datum era -> StrictMaybe (DataHash (Crypto era))
datumDatahash = \case
  NoDatum -> SNothing
  (DatumHash d) -> SJust d
  (Datum d) -> SJust $ hashData d

pattern TxOut ::
  forall era.
  ( Era era,
    Compactible (Core.Value era),
    Val (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  Datum (era) ->
  TxOut era
pattern TxOut addr vl dh <-
  (viewTxOut -> (addr, vl, dh))
  where
    TxOut (Addr network paymentCred stakeRef) vl NoDatum
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, a, b, c, d) <- encodeAddress28 network paymentCred =
        TxOut_AddrHash28_AdaOnly stakeCred a b c d adaCompact
    TxOut (Addr network paymentCred stakeRef) vl (DatumHash dh)
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, a, b, c, d) <- encodeAddress28 network paymentCred,
        Just (Refl, e, f, g, h) <- encodeDataHash32 dh =
        TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred a b c d adaCompact e f g h
    TxOut addr vl d =
      let v = fromMaybe (error "Illegal value in txout") $ toCompact vl
          a = compactAddr addr
       in case d of
            NoDatum -> TxOutCompact' a v
            DatumHash dh -> TxOutCompactDH' a v dh
            Datum d' -> TxOutCompactDatum' a v d'

{-# COMPLETE TxOut #-}

-- TODO deprecate
pattern TxOutCompact ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  TxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, SNothing))
  where
    TxOutCompact cAddr cVal = TxOut (decompactAddr cAddr) (fromCompact cVal) NoDatum

-- TODO deprecate
pattern TxOutCompactDH ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  DataHash (Crypto era) ->
  TxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, SJust dh))
  where
    TxOutCompactDH cAddr cVal = TxOut (decompactAddr cAddr) (fromCompact cVal) . DatumHash

pattern TxOutCompactDatum ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  Data era ->
  TxOut era
pattern TxOutCompactDatum addr vl d = TxOutCompactDatum' addr vl d

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _inputs :: !(Set (TxIn (Crypto era))),
    _collateral :: !(Set (TxIn (Crypto era))),
    _referenceInputs :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (TxOut era)),
    _collateralReturn :: !(StrictMaybe (TxOut era)),
    _totalCollateral :: !Coin,
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
    Show (PParamsDelta era)
  ) =>
  Show (TxBodyRaw era)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  ( Eq (Core.Value era),
    Compactible (Core.Value era),
    CC.Crypto (Crypto era),
    Eq (PParamsDelta era)
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
      Core.SerialisableData (PParamsDelta era)
    ) =>
    FromCBOR (Annotator (TxBody era))

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
  Coin ->
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
          { _inputs = inputs,
            _collateral = collateral,
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

inputs' :: TxBody era -> Set (TxIn (Crypto era))
collateral' :: TxBody era -> Set (TxIn (Crypto era))
referenceInputs' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
collateralReturn' :: TxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: TxBody era -> Coin
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> Value (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
inputs' (TxBodyConstr (Memo raw _)) = _inputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

collateral' (TxBodyConstr (Memo raw _)) = _collateral raw

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

instance
  ( Era era,
    Compactible (Core.Value era)
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr cv) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR cv
  toCBOR (TxOutCompactDatum addr cv d) =
    encodeListLen 4
      <> toCBOR True
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR d
  toCBOR (TxOutCompactDH addr cv dh) =
    encodeListLen 3
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR dh

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  FromCBOR (Annotator (TxOut era))
  where
  fromCBOR = fromNotSharedCBOR

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  FromSharedCBOR (Annotator (TxOut era))
  where
  type Share (Annotator (TxOut era)) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns = do
    lenOrIndef <- decodeListLenOrIndef
    let internTxOut :: TxOut era -> TxOut era
        internTxOut = \case
          TxOut_AddrHash28_AdaOnly cred a b c d ada ->
            TxOut_AddrHash28_AdaOnly (interns credsInterns cred) a b c d ada
          TxOut_AddrHash28_AdaOnly_DataHash32 cred a b c d ada e f g h ->
            TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) a b c d ada e f g h
          txOut -> txOut
    internTxOut <$$> case lenOrIndef of
      Nothing ->
        fmap pure $ do
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
        fmap pure $
          TxOutCompact
            <$> fromCBOR
            <*> decodeNonNegative
      Just 3 ->
        fmap pure $
          TxOutCompactDH
            <$> fromCBOR
            <*> decodeNonNegative
            <*> fromCBOR
      Just 4 -> do
        True <- fromCBOR @Bool
        a <- fromCBOR
        b <- decodeNonNegative
        c <- fromCBOR
        pure $ TxOutCompactDatum a b <$> c
      Just n -> cborError $ DecoderErrorCustom "txout" $ "wrong number of terms in txout: " <> T.pack (show n)

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (PParamsDelta era)
  ) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { _inputs,
      _collateral,
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
      !> Key 0 (E encodeFoldable _inputs)
      !> Key 13 (E encodeFoldable _collateral)
      !> Key 18 (E encodeFoldable _referenceInputs)
      !> Key 1 (E encodeFoldable _outputs)
      !> Key 16 (To _collateralReturn)
      !> Key 17 (To _totalCollateral)
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
    where
      encodeKeyedStrictMaybe key x =
        Omit isSNothing (Key key (E (toCBOR . fromSJust) x))

      fromSJust :: StrictMaybe a -> a
      fromSJust (SJust x) = x
      fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing"

doubleFmap, (<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
{-# INLINE doubleFmap #-}
doubleFmap = fmap . fmap
{-# INLINE (<$$>) #-}
(<$$>) = doubleFmap

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
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        (pure initial)
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
          mempty
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
      bodyFields :: (Word -> Field (Annotator (TxBodyRaw era)))
      bodyFields 0 =
        fieldA
          (\x tx -> tx {_inputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        fieldA
          (\x tx -> tx {_collateral = x})
          (D (decodeSet fromCBOR))
      bodyFields 18 =
        fieldA
          (\x tx -> tx {_referenceInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        fieldAA
          (\x tx -> tx {_outputs = x})
          (D (decodeAnnStrictSeq fromCBOR))
      bodyFields 16 =
        fieldAA
          (\x tx -> tx {_collateralReturn = x})
          (D (sequenceA <$> fromCBOR))
      bodyFields 17 =
        fieldA
          (\x tx -> tx {_totalCollateral = x})
          (D (fromCBOR))
      bodyFields 2 = fieldA (\x tx -> tx {_txfee = x}) From
      bodyFields 3 =
        fieldA
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 4 =
        fieldA
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 = fieldA (\x tx -> tx {_wdrls = x}) From
      bodyFields 6 = fieldA (\x tx -> tx {_update = x}) (D (SJust <$> fromCBOR))
      bodyFields 7 = fieldA (\x tx -> tx {_adHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 8 =
        fieldA
          (\x tx -> tx {_vldt = (_vldt tx) {invalidBefore = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 9 = fieldA (\x tx -> tx {_mint = x}) (D decodeMint)
      bodyFields 11 = fieldA (\x tx -> tx {_scriptIntegrityHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 14 = fieldA (\x tx -> tx {_reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = fieldA (\x tx -> tx {_txnetworkid = x}) (D (SJust <$> fromCBOR))
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

-- ====================================================
-- HasField instances to be consistent with earlier Eras

instance (Crypto era ~ c) => HasField "inputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _inputs m

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
  getField (TxBodyConstr (Memo m _)) = _collateral m

instance (Crypto era ~ c) => HasField "referenceInputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _referenceInputs m

instance HasField "collateralReturn" (TxBody era) (StrictMaybe (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = _collateralReturn m

instance HasField "totalCollateral" (TxBody era) (Coin) where
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

instance (Era era, Crypto era ~ c) => HasField "compactAddress" (TxOut era) (CompactAddr c) where
  getField (TxOutCompact a _) = a
  getField (TxOutCompactDH a _ _) = a

instance (Era era, CC.Crypto c, Crypto era ~ c) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOutCompact a _) = decompactAddr a
  getField (TxOutCompactDH a _ _) = decompactAddr a

instance (Era era, Core.Value era ~ val, Compactible val) => HasField "value" (TxOut era) val where
  getField (TxOutCompact _ v) = fromCompact v
  getField (TxOutCompactDH _ v _) = fromCompact v

instance (Era era, c ~ Crypto era) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField (TxOutCompact _ _) = SNothing
  getField (TxOutCompactDH _ _ d) = SJust d

instance (Era era) => HasField "datum" (TxOut era) (StrictMaybe (Data era)) where
  getField (TxOutCompact _ _) = SNothing
  getField (TxOutCompactDatum _ _ d) = SJust d
  getField (TxOutCompactDH _ _ _) = SNothing
