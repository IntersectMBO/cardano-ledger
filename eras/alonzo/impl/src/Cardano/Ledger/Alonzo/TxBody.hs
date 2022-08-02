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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxBody
  ( AlonzoTxOut (.., AlonzoTxOut, TxOutCompact, TxOutCompactDH),
    AlonzoEraTxOut (..),
    -- Constructors are not exported for safety:
    Addr28Extra,
    DataHash32,
    AlonzoTxBody
      ( AlonzoTxBody,
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
    AlonzoEraTxBody (..),
    ShelleyEraTxBody (..),
    ShelleyMAEraTxBody (..),
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

    -- * Deprecated
    TxOut,
    TxBody,
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
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Scripts ()
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    compactAddr,
    decompactAddr,
    fromCborBackwardsBothAddr,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), PaymentCredential, StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset (..), policies, policyID)
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash,
    extractHash,
    unsafeMakeSafeHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative (decodeNonNegative),
    Val (..),
    decodeMint,
    encodeMint,
  )
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (guard, (<$!>))
import Data.Bits
import Data.Coders hiding (to)
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing
import Data.Typeable (Proxy (..), Typeable, (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data Addr28Extra
  = Addr28Extra
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr
      {-# UNPACK #-} !Word64 -- Payment Addr (32bits) + ... +  0/1 for Testnet/Mainnet + 0/1 Script/Pubkey
  deriving (Eq)

data DataHash32
  = DataHash32
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
      {-# UNPACK #-} !Word64 -- DataHash
  deriving (Eq)

data AlonzoTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value era))
      !(DataHash (Crypto era))
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32

type TxOut era = AlonzoTxOut era

{-# DEPRECATED TxOut "Use `AlonzoTxOut` instead" #-}

deriving stock instance
  ( Eq (Value era),
    Compactible (Value era)
  ) =>
  Eq (AlonzoTxOut era)

-- | Already in NF
instance NFData (AlonzoTxOut era) where
  rnf = rwhnf

getAdaOnly ::
  forall era.
  Val (Value era) =>
  Proxy era ->
  Value era ->
  Maybe (CompactForm Coin)
getAdaOnly _ v = do
  guard $ isAdaOnly v
  toCompact $ coin v

decodeAddress28 ::
  forall c.
  HashAlgorithm (CC.ADDRHASH c) =>
  Credential 'Staking c ->
  Addr28Extra ->
  Maybe (Addr c)
decodeAddress28 stakeRef (Addr28Extra a b c d) = do
  Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH c))) (Proxy @28)
  let network = if d `testBit` 1 then Mainnet else Testnet
      paymentCred =
        if d `testBit` 0
          then KeyHashObj (KeyHash addrHash)
          else ScriptHashObj (ScriptHash addrHash)
      addrHash :: Hash (CC.ADDRHASH c) a
      addrHash =
        hashFromPackedBytes $
          PackedBytes28 a b c (fromIntegral (d `shiftR` 32))
  pure $! Addr network paymentCred (StakeRefBase stakeRef)

encodeAddress28 ::
  forall c.
  HashAlgorithm (CC.ADDRHASH c) =>
  Network ->
  PaymentCredential c ->
  Maybe (SizeHash (CC.ADDRHASH c) :~: 28, Addr28Extra)
encodeAddress28 network paymentCred = do
  let networkBit, payCredTypeBit :: Word64
      networkBit =
        case network of
          Mainnet -> 0 `setBit` 1
          Testnet -> 0
      payCredTypeBit =
        case paymentCred of
          KeyHashObj {} -> 0 `setBit` 0
          ScriptHashObj {} -> 0
      encodeAddr ::
        Hash (CC.ADDRHASH c) a ->
        Maybe (SizeHash (CC.ADDRHASH c) :~: 28, Addr28Extra)
      encodeAddr h = do
        refl@Refl <- sameNat (Proxy @(SizeHash (CC.ADDRHASH c))) (Proxy @28)
        case hashToPackedBytes h of
          PackedBytes28 a b c d ->
            let d' = (fromIntegral d `shiftL` 32) .|. networkBit .|. payCredTypeBit
             in Just (refl, Addr28Extra a b c d')
          _ -> Nothing
  case paymentCred of
    KeyHashObj (KeyHash addrHash) -> encodeAddr addrHash
    ScriptHashObj (ScriptHash addrHash) -> encodeAddr addrHash

decodeDataHash32 ::
  forall c.
  HashAlgorithm (CC.HASH c) =>
  DataHash32 ->
  Maybe (DataHash c)
decodeDataHash32 (DataHash32 a b c d) = do
  Refl <- sameNat (Proxy @(SizeHash (CC.HASH c))) (Proxy @32)
  Just $! unsafeMakeSafeHash $ hashFromPackedBytes $ PackedBytes32 a b c d

encodeDataHash32 ::
  forall c.
  (HashAlgorithm (CC.HASH c)) =>
  DataHash c ->
  Maybe (SizeHash (CC.HASH c) :~: 32, DataHash32)
encodeDataHash32 dataHash = do
  refl@Refl <- sameNat (Proxy @(SizeHash (CC.HASH c))) (Proxy @32)
  case hashToPackedBytes (extractHash dataHash) of
    PackedBytes32 a b c d -> Just (refl, DataHash32 a b c d)
    _ -> Nothing

viewCompactTxOut ::
  (Era era, Val (Value era)) =>
  AlonzoTxOut era ->
  (CompactAddr (Crypto era), CompactForm (Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, SJust dh)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
    | Just addr <- decodeAddress28 stakeRef addr28Extra ->
        (compactAddr addr, injectCompact adaVal, SNothing)
    | otherwise -> error addressErrorMsg
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32
    | Just addr <- decodeAddress28 stakeRef addr28Extra,
      Just dh <- decodeDataHash32 dataHash32 ->
        (compactAddr addr, injectCompact adaVal, SJust dh)
    | otherwise -> error addressErrorMsg

viewTxOut ::
  (Era era, Val (Value era)) =>
  AlonzoTxOut era ->
  (Addr (Crypto era), Value era, StrictMaybe (DataHash (Crypto era)))
viewTxOut (TxOutCompact' bs c) = (addr, val, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, SJust dh)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal)
  | Just addr <- decodeAddress28 stakeRef addr28Extra =
      (addr, inject (fromCompact adaVal), SNothing)
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32)
  | Just addr <- decodeAddress28 stakeRef addr28Extra,
    Just dh <- decodeDataHash32 dataHash32 =
      (addr, inject (fromCompact adaVal), SJust dh)
viewTxOut TxOut_AddrHash28_AdaOnly {} = error addressErrorMsg
viewTxOut TxOut_AddrHash28_AdaOnly_DataHash32 {} = error addressErrorMsg

instance (Era era, Val (Value era), Show (Value era)) => Show (AlonzoTxOut era) where
  show = show . viewTxOut -- FIXME: showing tuple is ugly

deriving via InspectHeapNamed "AlonzoTxOut" (AlonzoTxOut era) instance NoThunks (AlonzoTxOut era)

pattern AlonzoTxOut ::
  forall era.
  (Era era, Val (Value era), HasCallStack) =>
  Addr (Crypto era) ->
  Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  AlonzoTxOut era
pattern AlonzoTxOut addr vl dh <-
  (viewTxOut -> (addr, vl, dh))
  where
    AlonzoTxOut (Addr network paymentCred stakeRef) vl SNothing
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
          TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
    AlonzoTxOut (Addr network paymentCred stakeRef) vl (SJust dh)
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
        Just (Refl, dataHash32) <- encodeDataHash32 dh =
          TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
    AlonzoTxOut addr vl mdh =
      let v = fromMaybe (error "Illegal value in txout") $ toCompact vl
          a = compactAddr addr
       in case mdh of
            SNothing -> TxOutCompact' a v
            SJust dh -> TxOutCompactDH' a v dh

{-# COMPLETE AlonzoTxOut #-}

instance CC.Crypto c => EraTxOut (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxOut (AlonzoEra CC.StandardCrypto) #-}

  type TxOut (AlonzoEra c) = AlonzoTxOut (AlonzoEra c)

  mkBasicTxOut addr vl = AlonzoTxOut addr vl SNothing

  addrEitherTxOutL =
    lens
      getAlonzoTxOutEitherAddr
      ( \txOut eAddr ->
          let cVal = getTxOutCompactValue txOut
              (_, _, dh) = viewTxOut txOut
           in case eAddr of
                Left addr -> mkTxOutCompact addr (compactAddr addr) cVal dh
                Right cAddr -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal dh
      )
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL =
    lens
      (Right . getTxOutCompactValue)
      ( \txOut eVal ->
          case eVal of
            Left val ->
              let (addr, _, dh) = viewTxOut txOut
               in AlonzoTxOut addr val dh
            Right cVal ->
              let dh = getAlonzoTxOutDataHash txOut
               in case getAlonzoTxOutEitherAddr txOut of
                    Left addr -> mkTxOutCompact addr (compactAddr addr) cVal dh
                    Right cAddr -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal dh
      )
  {-# INLINE valueEitherTxOutL #-}

class EraTxOut era => AlonzoEraTxOut era where
  dataHashTxOutL :: Lens' (Core.TxOut era) (StrictMaybe (DataHash (Crypto era)))

instance CC.Crypto c => AlonzoEraTxOut (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (AlonzoEra CC.StandardCrypto) #-}

  dataHashTxOutL =
    lens getAlonzoTxOutDataHash (\(AlonzoTxOut addr cv _) dh -> AlonzoTxOut addr cv dh)
  {-# INLINE dataHashTxOutL #-}

getTxOutCompactValue :: EraTxOut era => AlonzoTxOut era -> CompactForm (Value era)
getTxOutCompactValue =
  \case
    TxOutCompact' _ cv -> cv
    TxOutCompactDH' _ cv _ -> cv
    TxOut_AddrHash28_AdaOnly _ _ cc -> injectCompact cc
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ -> injectCompact cc

-- ======================================

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _inputs :: !(Set (TxIn (Crypto era))),
    _collateral :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (AlonzoTxOut era)),
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    _mint :: !(MultiAsset (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Cardano.Ledger.Core.Value.
    -- Operations on the TxBody in the AlonzoEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (Value era), Compactible (Value era)) =>
  Eq (TxBodyRaw era)

instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (TxBodyRaw era)

instance (Era era, NFData (PParamsUpdate era)) => NFData (TxBodyRaw era)

deriving instance
  (Era era, Show (PParamsUpdate era), Show (Value era), Val (Value era)) =>
  Show (TxBodyRaw era)

newtype AlonzoTxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

type TxBody era = AlonzoTxBody era

{-# DEPRECATED TxBody "Use `AlonzoTxBody` instead" #-}

lensTxBodyRaw ::
  (Era era, Val (Value era), ToCBOR (PParamsUpdate era), DecodeNonNegative (Value era)) =>
  (TxBodyRaw era -> a) ->
  (TxBodyRaw era -> t -> TxBodyRaw era) ->
  Lens (AlonzoTxBody era) (AlonzoTxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkAlonzoTxBody $ setter txBodyRaw val)
{-# INLINE lensTxBodyRaw #-}

instance CC.Crypto c => EraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxBody (AlonzoEra CC.StandardCrypto) #-}

  type TxBody (AlonzoEra c) = AlonzoTxBody (AlonzoEra c)

  mkBasicTxBody = mkAlonzoTxBody initial

  inputsTxBodyL =
    lensTxBodyRaw _inputs (\txBodyRaw inputs_ -> txBodyRaw {_inputs = inputs_})
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensTxBodyRaw _outputs (\txBodyRaw outputs_ -> txBodyRaw {_outputs = outputs_})
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL =
    lensTxBodyRaw _txfee (\txBodyRaw fee_ -> txBodyRaw {_txfee = fee_})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensTxBodyRaw _adHash (\txBodyRaw auxDataHash -> txBodyRaw {_adHash = auxDataHash})
  {-# INLINE auxDataHashTxBodyL #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINE allInputsTxBodyF #-}

  mintedTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (_mint txBodyRaw)))
  {-# INLINE mintedTxBodyF #-}

instance CC.Crypto c => ShelleyEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  wdrlsTxBodyL =
    lensTxBodyRaw _wdrls (\txBodyRaw wdrls_ -> txBodyRaw {_wdrls = wdrls_})
  {-# INLINE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL =
    lensTxBodyRaw _update (\txBodyRaw update_ -> txBodyRaw {_update = update_})
  {-# INLINE updateTxBodyL #-}

  certsTxBodyL =
    lensTxBodyRaw _certs (\txBodyRaw certs_ -> txBodyRaw {_certs = certs_})
  {-# INLINE certsTxBodyL #-}

instance CC.Crypto c => ShelleyMAEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  vldtTxBodyL =
    lensTxBodyRaw _vldt (\txBodyRaw vldt_ -> txBodyRaw {_vldt = vldt_})
  {-# INLINE vldtTxBodyL #-}

  mintTxBodyL =
    lensTxBodyRaw _mint (\txBodyRaw mint_ -> txBodyRaw {_mint = mint_})
  {-# INLINE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue 0)

class (ShelleyMAEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (Core.TxBody era) (Set (TxIn (Crypto era)))

  reqSignerHashesTxBodyL :: Lens' (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era)))

  scriptIntegrityHashTxBodyL ::
    Lens' (Core.TxBody era) (StrictMaybe (ScriptIntegrityHash (Crypto era)))

  networkIdTxBodyL :: Lens' (Core.TxBody era) (StrictMaybe Network)

instance CC.Crypto c => AlonzoEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL =
    lensTxBodyRaw _collateral (\txBodyRaw collateral_ -> txBodyRaw {_collateral = collateral_})
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensTxBodyRaw
      _reqSignerHashes
      (\txBodyRaw reqSignerHashes_ -> txBodyRaw {_reqSignerHashes = reqSignerHashes_})
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensTxBodyRaw
      _scriptIntegrityHash
      (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {_scriptIntegrityHash = scriptIntegrityHash_})
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL =
    lensTxBodyRaw _txnetworkid (\txBodyRaw networkId -> txBodyRaw {_txnetworkid = networkId})
  {-# INLINE networkIdTxBodyL #-}

deriving newtype instance CC.Crypto (Crypto era) => Eq (AlonzoTxBody era)

deriving instance (Era era, NoThunks (PParamsUpdate era)) => NoThunks (AlonzoTxBody era)

deriving instance (Era era, NFData (PParamsUpdate era)) => NFData (AlonzoTxBody era)

deriving instance
  (Era era, Show (PParamsUpdate era), Show (Value era), Val (Value era)) =>
  Show (AlonzoTxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    ( Era era,
      FromCBOR (PParamsUpdate era),
      Val (Value era),
      Show (Value era),
      DecodeNonNegative (Value era)
    ) =>
    FromCBOR (Annotator (AlonzoTxBody era))

pattern AlonzoTxBody ::
  EraTxBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (AlonzoTxOut era) ->
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
  AlonzoTxBody era
pattern AlonzoTxBody
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
    AlonzoTxBody
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
        mkAlonzoTxBody $
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

{-# COMPLETE AlonzoTxBody #-}

mkAlonzoTxBody ::
  (Era era, Val (Value era), ToCBOR (PParamsUpdate era), DecodeNonNegative (Value era)) =>
  TxBodyRaw era ->
  AlonzoTxBody era
mkAlonzoTxBody = TxBodyConstr . memoBytes . encodeTxBodyRaw

instance (c ~ Crypto era) => HashAnnotated (AlonzoTxBody era) EraIndependentTxBody c

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: AlonzoTxBody era -> Set (TxIn (Crypto era))
collateral' :: AlonzoTxBody era -> Set (TxIn (Crypto era))
outputs' :: AlonzoTxBody era -> StrictSeq (AlonzoTxOut era)
certs' :: AlonzoTxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: AlonzoTxBody era -> Coin
wdrls' :: AlonzoTxBody era -> Wdrl (Crypto era)
vldt' :: AlonzoTxBody era -> ValidityInterval
update' :: AlonzoTxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: AlonzoTxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: AlonzoTxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: AlonzoTxBody era -> MultiAsset (Crypto era)
scriptIntegrityHash' :: AlonzoTxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
txnetworkid' :: AlonzoTxBody era -> StrictMaybe Network
inputs' (TxBodyConstr (Memo raw _)) = _inputs raw

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
  (Era era, Val (Value era), DecodeNonNegative (Value era), ToCBOR (CompactForm (Value era))) =>
  ToCBOR (AlonzoTxOut era)
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
  (Era era, Show (Value era), Val (Value era), DecodeNonNegative (Value era)) =>
  FromCBOR (AlonzoTxOut era)
  where
  fromCBOR = fromNotSharedCBOR
  {-# INLINE fromCBOR #-}

instance
  (Era era, Val (Value era), DecodeNonNegative (Value era), Show (Value era)) =>
  FromSharedCBOR (AlonzoTxOut era)
  where
  type Share (AlonzoTxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns = do
    lenOrIndef <- decodeListLenOrIndef
    let internTxOut = \case
          TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
            TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
          TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
            TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
          txOut -> txOut
    internTxOut <$!> case lenOrIndef of
      Nothing -> do
        (a, ca) <- fromCborBackwardsBothAddr
        cv <- decodeNonNegative
        decodeBreakOr >>= \case
          True -> pure $ mkTxOutCompact a ca cv SNothing
          False -> do
            dh <- fromCBOR
            decodeBreakOr >>= \case
              True -> pure $ mkTxOutCompact a ca cv (SJust dh)
              False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
      Just 2 -> do
        (a, ca) <- fromCborBackwardsBothAddr
        cv <- decodeNonNegative
        pure $ mkTxOutCompact a ca cv SNothing
      Just 3 -> do
        (a, ca) <- fromCborBackwardsBothAddr
        cv <- decodeNonNegative
        mkTxOutCompact a ca cv . SJust <$> fromCBOR
      Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"
  {-# INLINE fromSharedCBOR #-}

pattern TxOutCompact ::
  (Era era, Val (Value era), HasCallStack) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  AlonzoTxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, SNothing))
  where
    TxOutCompact cAddr cVal = mkTxOutCompact (decompactAddr cAddr) cAddr cVal SNothing

pattern TxOutCompactDH ::
  (Era era, Val (Value era), HasCallStack) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  DataHash (Crypto era) ->
  AlonzoTxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, SJust dh))
  where
    TxOutCompactDH cAddr cVal dh = mkTxOutCompact (decompactAddr cAddr) cAddr cVal (SJust dh)

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

mkTxOutCompact ::
  (Era era, HasCallStack, Val (Value era)) =>
  Addr (Crypto era) ->
  CompactAddr (Crypto era) ->
  CompactForm (Value era) ->
  StrictMaybe (DataHash (Crypto era)) ->
  AlonzoTxOut era
mkTxOutCompact addr cAddr cVal mdh
  | isAdaOnlyCompact cVal = AlonzoTxOut addr (fromCompact cVal) mdh
  | SJust dh <- mdh = TxOutCompactDH' cAddr cVal dh
  | otherwise = TxOutCompact' cAddr cVal

encodeTxBodyRaw ::
  (Era era, ToCBOR (PParamsUpdate era), Val (Value era), DecodeNonNegative (Value era)) =>
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
      !> Omit null (Key 13 (E encodeFoldable _collateral))
      !> Key 1 (E encodeFoldable _outputs)
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
    FromCBOR (PParamsUpdate era),
    Show (Value era),
    Val (Value era),
    DecodeNonNegative (Value era)
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

instance
  ( Era era,
    FromCBOR (PParamsUpdate era),
    Val (Value era),
    Show (Value era),
    DecodeNonNegative (Value era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

getAlonzoTxOutDataHash ::
  forall era.
  HashAlgorithm (CC.HASH (Crypto era)) =>
  AlonzoTxOut era ->
  StrictMaybe (DataHash (Crypto era))
getAlonzoTxOutDataHash = \case
  TxOutCompactDH' _ _ dh -> SJust dh
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dh -> maybeToStrictMaybe $ decodeDataHash32 dh
  _ -> SNothing

getAlonzoTxOutEitherAddr ::
  HashAlgorithm (CC.ADDRHASH (Crypto era)) =>
  AlonzoTxOut era ->
  Either (Addr (Crypto era)) (CompactAddr (Crypto era))
getAlonzoTxOutEitherAddr = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error addressErrorMsg
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error addressErrorMsg

addressErrorMsg :: String
addressErrorMsg = "Impossible: Compacted an address of non-standard size"
{-# NOINLINE addressErrorMsg #-}
