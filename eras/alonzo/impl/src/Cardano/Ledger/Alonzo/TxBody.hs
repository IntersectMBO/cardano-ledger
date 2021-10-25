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
import Cardano.Crypto.Hash (hashToBytes, HashAlgorithm (SizeHash), hashFromBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..), DataHash)
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    isSNothing,
  )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentScriptIntegrity,
    EraIndependentTxBody, ScriptHash (ScriptHash)
  )
import Cardano.Ledger.Keys (KeyHash (KeyHash), KeyRole (..))
import Cardano.Ledger.Mary.Value (Value (..), policies, policyID)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash
  )
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr (..), compactAddr, decompactAddr)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero, adaOnly, Val (..)
  )
import qualified Data.ByteString as BS
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable,type  (:~:) (..))
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)
import qualified Cardano.Crypto.Hash as HS
import Data.Proxy
import GHC.TypeLits
import Data.Word
import Cardano.Ledger.Credential (Credential (..), StakeReference, PaymentCredential, StakeCredential)
import Data.Bits ((.&.), Bits (setBit, testBit, shiftR), shiftL)
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Data.ByteString.Short as SBS

-- We are trying to make some special cases, Since this is already a
-- multi-constructor type, we can make more specialized constructors for free.
-- What can we specialize?
--
-- * Shelley address VS byron address?
--   * Why not just specialize over the size of the ShortByteString?
-- * Currency
--   * Just Ada VS multi asset
-- * Datum VS no datum
--
-- Note that we don't unpack stake reference as we want to take advantage of
-- sharing.
data TxOut era
  -- -- This is here for reference. Everything else is a special case. TODO we can
  -- -- probably remove this.
  -- = TxOut_Canonical
  --     {-# UNPACK #-} !(CompactAddr (Crypto era))
  --     !(CompactForm (Core.Value era))
  --     !(Maybe (DataHash (Crypto era)))

  -- Special cases Notation: TxOut_<C>_<A>_<D>
  --  <C> is MultiAsset or JustAda
  --  <A> is the size in bytes of the address (N is arbitrary)
  --  <D> "NoDH" or "DH" in bytes of the datum hash (N is arbitrary >0)

  -- | The old TxOutCompact TODO remove?
  = TxOut_MultiAsset
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
  -- | The old TxOutCompactDH TODO remove?
  | TxOut_MultiAsset_DH
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(DataHash (Crypto era))

  | SizeHash (ADDRHASH (Crypto era)) ~ 28
    => TxOut_JustAda_ShelleyAddress
      {-# UNPACK #-}   !(CompactForm Coin) -- 1 word
      -- ^ The amount of lovelace.
      {-# UNPACK #-}   !PackedNetworkAndPaymentCredentials -- 4 word
      {-# NOUNPACK #-} !(StakeReference (Crypto era)) -- 1 word

  | SizeHash (ADDRHASH (Crypto era)) ~ 28
    => TxOut_JustAda_ShelleyAddress_DH
      {-# UNPACK #-}   !(CompactForm Coin) -- 1 word
      -- ^ The amount of lovelace.
      {-# UNPACK #-}   !PackedNetworkAndPaymentCredentials -- 4 word
      {-# NOUNPACK #-} !(StakeReference (Crypto era)) -- 1 word
      !(DataHash (Crypto era)) -- 4 word

-- | A shelley address packed into 4 words
data PackedNetworkAndPaymentCredentials = PackedNetworkAndPaymentCredentials
      {-# UNPACK #-}   !Word64 -- ^ Address bits
      {-# UNPACK #-}   !Word64 -- ^ Address bits
      {-# UNPACK #-}   !Word64 -- ^ Address bits
      {-# UNPACK #-}   !Word64
      -- ^ bits from high to low:
      --    (32 bits: address bits)
      --    ... unused
      --    (1 bit: 0 for Testnet 1 for Mainnet)
      --    (1 bit: 0 for Script hash 1 for key hash w.r.t addresss type)
      deriving Eq


unpackPackedShelleyAddressWith
  :: ( HashAlgorithm (ADDRHASH (Crypto era))
     , SizeHash (ADDRHASH (Crypto era)) ~ 28
     )
  => proxy era
  -> PackedNetworkAndPaymentCredentials
  -> StakeReference (Crypto era)
  -> Addr (Crypto era)
unpackPackedShelleyAddressWith era packed stakeRef
  = Addr network paymentCred stakeRef
  where
    (network, paymentCred) = unpackPackedShelleyAddress era packed

unpackPackedShelleyAddress
  :: ( HashAlgorithm (ADDRHASH (Crypto era))
     , SizeHash (ADDRHASH (Crypto era)) ~ 28
     )
  => proxy era
  -> PackedNetworkAndPaymentCredentials
  -> (Network, PaymentCredential (Crypto era))
unpackPackedShelleyAddress _ (PackedNetworkAndPaymentCredentials addr_a addr_b addr_c addr_d) =
  ( if testBit addr_d 1
    then Mainnet
    else Testnet
  , if testBit addr_d 0
    then ScriptHashObj (ScriptHash hash)
    else KeyHashObj (KeyHash hash)
  )
  where
    hash :: HashAlgorithm h => HS.Hash h a
    hash = fromMaybe (error "Internal error: incorrect hash size")
      $ hashFromBytes
      $ BS.pack
        [ fromIntegral (word `shiftR` (byteIx * 8))
        | (word, nBytes) <-
            [ (addr_a, 8)
            , (addr_b, 8)
            , (addr_c, 8)
            , (addr_d, 4)
            ]
        , byteIx <- [0..nBytes-1]
        ]

mkPackedShelleyAddress
  :: SizeHash (ADDRHASH (Crypto era)) ~ 28
  => proxy era
  -> Network
  -> PaymentCredential (Crypto era)
  -> PackedNetworkAndPaymentCredentials
mkPackedShelleyAddress _ network paymentCredential
  = PackedNetworkAndPaymentCredentials
                addr_a
                addr_b
                addr_c
                (networkBit
                  .&. addrTypeBit
                  .&. addr_d
                )
  where
    networkBit :: Word64
    networkBit = case network of
      Testnet -> 0
      Mainnet -> 0 `setBit` 1

    addrTypeBit :: Word64
    addrTypeBit = case paymentCredential of
      ScriptHashObj{} -> 0
      KeyHashObj{} -> 0 `setBit` 0

    credByteString :: BS.ByteString
    credByteString = case paymentCredential of
      ScriptHashObj (ScriptHash ha) -> hashToBytes ha
      KeyHashObj (KeyHash ha) -> hashToBytes ha

    extract :: Int -> Int -> Word64
    extract offset bytes = foldr (.&.) 0 [(fromIntegral $ credByteString `BS.index` (i + offset)) `shiftL` (i * bytes) | i <- [0..bytes - 1]]

    addr_a = extract 0  8
    addr_b = extract 8  8
    addr_c = extract 16 8
    addr_d = extract 16 4

deriving stock instance
  ( Eq (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

viewCompactTxOut'
  :: forall era
  . ( HashAlgorithm (ADDRHASH (Crypto era))
    , Val (Core.Value era)
    )
  => TxOut era
  -> ( Either (Addr (Crypto era)) (CompactAddr (Crypto era))
     , Either (Core.Value era) (CompactForm (Core.Value era))
     , StrictMaybe (DataHash (Crypto era))
     )
viewCompactTxOut' txOut = case txOut of
  TxOut_MultiAsset cAddr cVal -> (Right cAddr, Right cVal, SNothing)
  TxOut_MultiAsset_DH cAddr cVal dh -> (Right cAddr, Right cVal, SJust dh)
  TxOut_JustAda_ShelleyAddress cCoin pNetPayCred stakeRef
    -> (Left addr, Left val, SNothing)
    where
      addr = unpackPackedShelleyAddressWith (Proxy @era) pNetPayCred stakeRef
      val = inject (fromCompact cCoin)
  TxOut_JustAda_ShelleyAddress_DH cCoin pNetPayCred stakeRef dh
    -> (Left addr, Left val, SJust dh)
    where
      addr = unpackPackedShelleyAddressWith (Proxy @era) pNetPayCred stakeRef
      val = inject (fromCompact cCoin)

viewCompactTxOut ::
  forall era.
  ( Compactible (Core.Value era)
  , HashAlgorithm (ADDRHASH (Crypto era))
  , Val (Core.Value era)
  ) =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Core.Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut (viewCompactTxOut' -> (cAddr, cValue, dhMay))
  = ( either compactAddr id cAddr
    , either (fromMaybe (error "Invalid ada value") {- TODO is this error ok? -} . toCompact) id cValue
    , dhMay
    )

viewTxOut ::
  forall era.
  ( Era era
  , Val (Core.Value era)
  ) =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, StrictMaybe (DataHash (Crypto era)))
viewTxOut (viewCompactTxOut' -> (cAddr, cValue, dhMay))
  = (either id decompactAddr cAddr, either id fromCompact cValue, dhMay)

instance
  ( Era era,
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern TxOut :: forall era.
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  TxOut era
pattern TxOut addr val dh <-
  (viewTxOut -> (addr, val, dh))
  where
    TxOut addr val mdh
      -- The standard use case
      | Just Refl <- sameNat (Proxy @(HS.SizeHash (CC.ADDRHASH (Crypto era)))) (Proxy @28)
      = case (addr, mdh, adaOnly val) of
        -- Shelley address without datum and just ada
        (Addr network paymentCredential stakeReference, SNothing, True)
          -> TxOut_JustAda_ShelleyAddress
              (fromMaybe (error "Invalid ada value") (toCompact (coin val))) -- TODO is this error correct?
              (mkPackedShelleyAddress (Proxy @era) network paymentCredential)
              stakeReference
        -- Shelley address with datum and just ada
        (Addr network paymentCredential stakeReference, SJust dh, True)
          -> TxOut_JustAda_ShelleyAddress_DH
              (fromMaybe (error "Invalid ada value") (toCompact (coin val))) -- TODO is this error correct?
              (mkPackedShelleyAddress (Proxy @era) network paymentCredential)
              stakeReference
              dh
        _ -> canonical
      -- Non-standard address hash size.
      | otherwise = canonical
      where
        canonical = case mdh of
              SNothing -> TxOutCompact cAddr cVal
              SJust dh -> TxOutCompactDH cAddr cVal dh
        cVal = fromMaybe (error $ "Illegal value in txout: " <> show val) $ toCompact val
        cAddr = compactAddr addr
    

{-# COMPLETE TxOut #-}

pattern TxOutCompact
  :: ( Compactible (Core.Value era)
     , HashAlgorithm (ADDRHASH (Crypto era))
     , Val (Core.Value era)
     )
  => CompactAddr (Crypto era)
  -> CompactForm (Core.Value era)
  -> TxOut era
pattern TxOutCompact addr val <- (viewCompactTxOut -> (addr, val, SNothing)) where
  TxOutCompact addr val = TxOut_MultiAsset addr val

pattern TxOutCompactDH
  :: ( Compactible (Core.Value era)
     , HashAlgorithm (ADDRHASH (Crypto era))
     , Val (Core.Value era)
     )
  => CompactAddr (Crypto era)
  -> CompactForm (Core.Value era)
  -> DataHash (Crypto era)
  -> TxOut era
pattern TxOutCompactDH addr val dh <- (viewCompactTxOut -> (addr, val, SJust dh)) where
  TxOutCompactDH addr val dh = TxOut_MultiAsset_DH addr val dh

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
{-# LANGUAGE GADTs #-}

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

instance (Crypto era ~ c, Compactible (Core.Value era), Val (Core.Value era), HashAlgorithm (ADDRHASH c)) => HasField "compactAddress" (TxOut era) (CompactAddr c) where
  getField (TxOutCompact a _) = a
  getField (TxOutCompactDH a _ _) = a

instance (CC.Crypto c, Crypto era ~ c, Compactible (Core.Value era), Val (Core.Value era), HashAlgorithm (ADDRHASH c)) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOutCompact a _) = decompactAddr a
  getField (TxOutCompactDH a _ _) = decompactAddr a

instance (Core.Value era ~ val, Compactible val, Val val, HashAlgorithm (ADDRHASH (Crypto era))) => HasField "value" (TxOut era) val where
  getField (TxOutCompact _ v) = fromCompact v
  getField (TxOutCompactDH _ v _) = fromCompact v

instance (c ~ Crypto era, Compactible (Core.Value era), Val (Core.Value era), HashAlgorithm (ADDRHASH (Crypto era))) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField (TxOutCompact _ _) = SNothing
  getField (TxOutCompactDH _ _ d) = SJust d
