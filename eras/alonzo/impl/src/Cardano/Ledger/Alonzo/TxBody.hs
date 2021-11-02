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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Alonzo.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH),
    TxBody
      ( TxBody,
        inputs,
        collateral,
        outputs,
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
    inputs',
    collateral',
    outputs',
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
    AlonzoBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,

    -- * deprecated
    WitnessPPDataHash,
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
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..), DataHash)
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    isSNothing,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), PaymentCredential, StakeReference)
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
    extractHash,
    unsafeMakeSafeHash,
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
    adaOnly,
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero,
  )
import Data.Bits
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
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
  | SizeHash (CC.ADDRHASH (Crypto era)) ~ 28 =>
    TxOut_AddrHash28_AdaOnly
      !(StakeReference (Crypto era))
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !Word64 -- Ada value
  | ( SizeHash (CC.ADDRHASH (Crypto era)) ~ 28,
      SizeHash (CC.HASH (Crypto era)) ~ 32
    ) =>
    TxOut_AddrHash28_AdaOnly_DataHash32
      !(StakeReference (Crypto era))
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
      {-# UNPACK #-} !Word64 -- Ada value
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash

deriving stock instance
  ( Eq (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

decodeAddress28 ::
  forall crypto.
  ( SizeHash (CC.ADDRHASH crypto) ~ 28,
    HashAlgorithm (CC.ADDRHASH crypto)
  ) =>
  StakeReference crypto ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  Addr crypto
decodeAddress28 stakeRef a b c d =
  Addr network paymentCred stakeRef
  where
    network = if d `testBit` 1 then Mainnet else Testnet
    paymentCred =
      if d `testBit` 0
        then KeyHashObj (KeyHash addrHash)
        else ScriptHashObj (ScriptHash addrHash)
    addrHash :: Hash (CC.ADDRHASH crypto) a
    addrHash =
      hashFromPackedBytes $
        PackedBytes28 a b c (fromIntegral (d `shiftR` 32))

encodeAddress28 ::
  forall crypto.
  ( SizeHash (CC.ADDRHASH crypto) ~ 28,
    HashAlgorithm (CC.ADDRHASH crypto)
  ) =>
  Network ->
  (PaymentCredential crypto) ->
  ( Word64,
    Word64,
    Word64,
    Word64
  )
encodeAddress28 network paymentCred = case paymentCred of
  KeyHashObj (KeyHash addrHash) -> go addrHash
  ScriptHashObj (ScriptHash addrHash) -> go addrHash
  where
    networkBit = case network of
      Mainnet -> 0 `setBit` 1
      Testnet -> 0

    payCredTypeBit = case paymentCred of
      KeyHashObj {} -> 0 `setBit` 0
      ScriptHashObj {} -> 0

    go :: Hash (CC.ADDRHASH crypto) a -> (Word64, Word64, Word64, Word64)
    go h = case hashToPackedBytes h of
      PackedBytes28 a b c d ->
        ( a,
          b,
          c,
          ((fromIntegral d) `shiftL` 32) .|. networkBit .|. payCredTypeBit
        )
      _ -> error "encodeAddress28: unexpected 28 byte PackedBytes that does NOT use the PackedBytes28 constructor!"

decodeDataHash32 ::
  forall crypto.
  ( SizeHash (CC.HASH crypto) ~ 32,
    HashAlgorithm (CC.HASH crypto)
  ) =>
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  DataHash crypto
decodeDataHash32 a b c d =
  unsafeMakeSafeHash $
    hashFromPackedBytes $
      PackedBytes32 a b c d

encodeDataHash32 ::
  forall crypto.
  ( SizeHash (CC.HASH crypto) ~ 32,
    HashAlgorithm (CC.HASH crypto)
  ) =>
  DataHash crypto ->
  ( Word64,
    Word64,
    Word64,
    Word64
  )
encodeDataHash32 dataHash = case hashToPackedBytes (extractHash dataHash) of
  PackedBytes32 a b c d -> (a, b, c, d)
  _ -> error "encodeAddress28: unexpected 32 byte PackedBytes that does NOT use the PackedBytes32 constructor!"

viewCompactTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Core.Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, SJust dh)
  TxOut_AddrHash28_AdaOnly stakeRef a b c d adaVal ->
    (compactAddr addr, valueToCompactErr (Proxy @era) val, SNothing)
    where
      (addr, val) = viewAddrHash28_AdaOnly (Proxy @era) stakeRef a b c d adaVal
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef a b c d adaVal e f g h ->
    (compactAddr addr, valueToCompactErr (Proxy @era) val, SJust (decodeDataHash32 e f g h))
    where
      (addr, val) = viewAddrHash28_AdaOnly (Proxy @era) stakeRef a b c d adaVal
  where
    valueToCompactErr _ = fromMaybe (error "Failed to compact a `Value era`") . toCompact

viewTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, StrictMaybe (DataHash (Crypto era)))
viewTxOut (TxOutCompact' bs c) = (addr, val, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, SJust dh)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef a b c d adaVal) =
  (addr, val, SNothing)
  where
    (addr, val) = viewAddrHash28_AdaOnly (Proxy @era) stakeRef a b c d adaVal
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef a b c d adaVal e f g h) =
  (addr, val, SJust (decodeDataHash32 e f g h))
  where
    (addr, val) = viewAddrHash28_AdaOnly (Proxy @era) stakeRef a b c d adaVal

viewAddrHash28_AdaOnly ::
  ( Era era,
    SizeHash (CC.ADDRHASH (Crypto era)) ~ 28,
    HashAlgorithm (CC.ADDRHASH (Crypto era))
  ) =>
  Proxy era ->
  StakeReference (Crypto era) ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  (Addr (Crypto era), Core.Value era)
viewAddrHash28_AdaOnly _ stakeRef a b c d adaVal =
  ( decodeAddress28 stakeRef a b c d,
    inject (Coin (fromIntegral adaVal))
  )

instance
  ( Era era,
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern TxOut ::
  forall era.
  ( Era era,
    Compactible (Core.Value era),
    Val (Core.Value era),
    Show (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  TxOut era
pattern TxOut addr vl dh <-
  (viewTxOut -> (addr, vl, dh))
  where
    TxOut (Addr network paymentCred stakeRef) vl SNothing
      | adaOnly vl,
        Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28) =
        let (a, b, c, d) = encodeAddress28 network paymentCred
         in TxOut_AddrHash28_AdaOnly stakeRef a b c d (fromIntegral (unCoin (coin vl)))
    TxOut (Addr network paymentCred stakeRef) vl (SJust dh)
      | adaOnly vl,
        Just Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28),
        Just Refl <- sameNat (Proxy @(SizeHash (CC.HASH (Crypto era)))) (Proxy @32) =
        let (a, b, c, d) = encodeAddress28 network paymentCred
            (e, f, g, h) = encodeDataHash32 dh
         in TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef a b c d (fromIntegral (unCoin (coin vl))) e f g h
    TxOut addr vl mdh =
      let v = fromMaybe (error $ "Illegal value in txout: " <> show vl) $ toCompact vl
          a = compactAddr addr
       in case mdh of
            SNothing -> TxOutCompact' a v
            SJust dh -> TxOutCompactDH' a v dh

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
    TxOutCompact = TxOutCompact'

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
    TxOutCompactDH = TxOutCompactDH'

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

{-# DEPRECATED WitnessPPDataHash "Use ScriptIntegrityHash instead" #-}

type WitnessPPDataHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _inputs :: !(Set (TxIn (Crypto era))),
    _collateral :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (TxOut era)),
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    _mint :: !(Value (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.Value, not a Core.Value.
    -- Operations on the TxBody in the AlonzoEra depend upon this.
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
type AlonzoBody era =
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era),
    Core.SerialisableData (PParamsDelta era)
  )

pattern TxBody ::
  AlonzoBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
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
    outputs,
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
            _outputs = outputs,
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
      outputsX
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
                  outputsX
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
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: TxBody era -> Set (TxIn (Crypto era))
collateral' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
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

outputs' (TxBodyConstr (Memo raw _)) = _outputs raw

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
  FromCBOR (TxOut era)
  where
  fromCBOR = do
    lenOrIndef <- decodeListLenOrIndef
    case lenOrIndef of
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
    ToCBOR (PParamsDelta era)
  ) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { _inputs,
      _collateral,
      _outputs,
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
      ( \i ifee o f t c w u b rsh mi sh ah ni ->
          TxBodyRaw i ifee o c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (E encodeFoldable _inputs)
      !> Key 13 (E encodeFoldable _collateral)
      !> Key 1 (E encodeFoldable _outputs)
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
          StrictSeq.empty
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
          (\x tx -> tx {_inputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {_collateral = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {_outputs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 2 = field (\x tx -> tx {_txfee = x}) From
      bodyFields 3 =
        field
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 4 =
        field
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 = field (\x tx -> tx {_wdrls = x}) From
      bodyFields 6 = field (\x tx -> tx {_update = x}) (D (SJust <$> fromCBOR))
      bodyFields 7 = field (\x tx -> tx {_adHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 8 =
        field
          (\x tx -> tx {_vldt = (_vldt tx) {invalidBefore = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 9 = field (\x tx -> tx {_mint = x}) (D decodeMint)
      bodyFields 11 = field (\x tx -> tx {_scriptIntegrityHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 14 = field (\x tx -> tx {_reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = field (\x tx -> tx {_txnetworkid = x}) (D (SJust <$> fromCBOR))
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

instance
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
  fromCBOR = pure <$> fromCBOR

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

instance (Crypto era ~ c) => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField (TxBodyConstr (Memo m _)) = Set.map policyID (policies (_mint m))

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = _vldt m

instance
  c ~ Crypto era =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _adHash m

-- | TODO deprecated
instance
  c ~ Crypto era =>
  HasField "wppHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _scriptIntegrityHash m

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
