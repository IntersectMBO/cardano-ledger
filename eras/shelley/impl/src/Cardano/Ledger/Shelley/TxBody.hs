{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
    PoolMetadata (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    StakePoolRelay (..),
    TxBody
      ( TxBody,
        TxBodyConstr,
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash
      ),
    TxBodyRaw (..),
    EraIndependentTxBody,
    TxOut (TxOut, TxOutCompact),
    Url,
    Wdrl (..),
    --
    module Cardano.Ledger.Keys.WitVKey,
    witKeyHash,
    wvkBytes,
    --
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
    --
    TransTxId,
    TransTxOut,
    TransTxBody,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), Url)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Keys.WitVKey
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization
  ( decodeRecordNamed,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.Constraints (TransValue)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.PoolParams
import Cardano.Ledger.Slot (SlotNo (..))
import qualified Cardano.Ledger.TxIn as Core
import Cardano.Ledger.Val (DecodeNonNegative (..))
import Cardano.HeapWords (HeapWords (..))
import Control.DeepSeq (NFData (rnf))
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (ShortByteString, pack)
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    encode,
    encodeKeyedStrictMaybe,
    field,
    invalidField,
    ofield,
    (!>),
  )
import Data.Constraint (Constraint)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

-- ========================================================================

newtype Wdrl crypto = Wdrl {unWdrl :: Map (RewardAcnt crypto) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData)

instance CC.Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = mapToCBOR . unWdrl

instance CC.Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl <$> mapFromCBOR

type TransTxId (c :: Type -> Constraint) era =
  -- Transaction Ids are the hash of a transaction body, which contains
  -- a Core.TxBody and Core.TxOut, hence the need for the ToCBOR instances
  -- in order to hash them.
  ( HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.TxOut era),
    TransValue ToCBOR era,
    TransValue c era
  )

-- | The output of a UTxO.
data TxOut era
  = TxOutCompact
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))

type TransTxOut (c :: Type -> Constraint) era =
  ( c (Core.Value era),
    Compactible (Core.Value era)
  )

-- assume Shelley+ type address : payment addr, staking addr (same length as payment), plus 1 word overhead
instance
  ( CC.Crypto (Crypto era),
    HeapWords (CompactForm (Core.Value era))
  ) =>
  HeapWords (TxOut era)
  where
  heapWords (TxOutCompact _ vl) =
    3
      + heapWords (packedADDRHASH (Proxy :: Proxy era))
      + heapWords vl

-- a ShortByteString of the same length as the ADDRHASH
-- used to calculate heapWords
packedADDRHASH :: forall proxy era. (CC.Crypto (Crypto era)) => proxy era -> ShortByteString
packedADDRHASH _ = pack (replicate (fromIntegral (1 + 2 * HS.sizeHash (Proxy :: Proxy (CC.ADDRHASH (Crypto era))))) (1 :: Word8))

instance
  (TransTxOut Show era, Era era) => -- Use the weakest constraint possible here
  Show (TxOut era)
  where
  show = show . viewCompactTxOut

deriving stock instance
  -- weakest constraint
  TransTxOut Eq era => Eq (TxOut era)

instance NFData (TxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern TxOut ::
  (Era era, Show (Core.Value era), Compactible (Core.Value era)) =>
  Addr (Crypto era) ->
  Core.Value era ->
  TxOut era
pattern TxOut addr vl <-
  (viewCompactTxOut -> (addr, vl))
  where
    TxOut addr vl =
      TxOutCompact
        (compactAddr addr)
        (fromMaybe (error $ "illegal value in txout: " <> show vl) $ toCompact vl)

{-# COMPLETE TxOut #-}

viewCompactTxOut ::
  forall era.
  (Era era) => -- Use the weakest constraint possible here
  TxOut era ->
  (Addr (Crypto era), Core.Value era)
viewCompactTxOut (TxOutCompact bs c) = (addr, val)
  where
    addr = decompactAddr bs
    val = fromCompact c

-- ---------------------------
-- WellFormed instances

instance (Compactible v, v ~ Core.Value era) => HasField "value" (TxOut era) v where
  getField (TxOutCompact _ v) = fromCompact v

-- ==============================
-- The underlying type for TxBody

data TxBodyRaw era = TxBodyRaw
  { _inputsX :: !(Set (Core.TxIn (Crypto era))),
    _outputsX :: !(StrictSeq (Core.TxOut era)),
    _certsX :: !(StrictSeq (DCert (Crypto era))),
    _wdrlsX :: !(Wdrl (Crypto era)),
    _txfeeX :: !Coin,
    _ttlX :: !SlotNo,
    _txUpdateX :: !(StrictMaybe (Update era)),
    _mdHashX :: !(StrictMaybe (AuxiliaryDataHash (Crypto era)))
  }
  deriving (Generic, Typeable)

deriving instance TransTxBody NoThunks era => NoThunks (TxBodyRaw era)

type TransTxBody (c :: Type -> Constraint) era =
  ( c (Core.TxOut era),
    c (Core.PParamsDelta era),
    HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era)
  )

deriving instance
  (NFData (Core.TxOut era), CC.Crypto (Crypto era), NFData (Core.PParamsDelta era)) =>
  NFData (TxBodyRaw era)

deriving instance (Era era, TransTxBody Eq era) => Eq (TxBodyRaw era)

deriving instance (Era era, TransTxBody Show era) => Show (TxBodyRaw era)

instance
  ( FromCBOR (Core.TxOut era),
    Era era,
    FromCBOR (Core.PParamsDelta era),
    ToCBOR (Core.PParamsDelta era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode
      ( SparseKeyed
          "TxBody"
          baseTxBodyRaw
          boxBody
          [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")]
      )

instance
  (TransTxBody FromCBOR era, ToCBOR (Core.PParamsDelta era), Era era) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

-- =================================================================
-- Composable components for building TxBody optional sparse serialisers.
-- The order of serializing optional fields, and their key values is
-- demanded by backward compatibility concerns.

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody ::
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.PParamsDelta era)
  ) =>
  Word ->
  Field (TxBodyRaw era)
boxBody 0 = field (\x tx -> tx {_inputsX = x}) (D (decodeSet fromCBOR))
boxBody 1 = field (\x tx -> tx {_outputsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 4 = field (\x tx -> tx {_certsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 5 = field (\x tx -> tx {_wdrlsX = x}) From
boxBody 2 = field (\x tx -> tx {_txfeeX = x}) From
boxBody 3 = field (\x tx -> tx {_ttlX = x}) From
boxBody 6 = ofield (\x tx -> tx {_txUpdateX = x}) From
boxBody 7 = ofield (\x tx -> tx {_mdHashX = x}) From
boxBody n = invalidField n

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (TransTxBody ToCBOR era, Era era) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
txSparse (TxBodyRaw input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> TxBodyRaw i o c w f t u h)
    !> Key 0 (E encodeFoldable input) -- We don't have to send these in TxBodyRaw order
    !> Key 1 (E encodeFoldable output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> encodeKeyedStrictMaybe 6 update
    !> encodeKeyedStrictMaybe 7 hash

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
baseTxBodyRaw :: TxBodyRaw era
baseTxBodyRaw =
  TxBodyRaw
    { _inputsX = Set.empty,
      _outputsX = StrictSeq.empty,
      _txfeeX = Coin 0,
      _ttlX = SlotNo 0,
      _certsX = StrictSeq.empty,
      _wdrlsX = Wdrl Map.empty,
      _txUpdateX = SNothing,
      _mdHashX = SNothing
    }

instance
  ( Era era,
    FromCBOR (Core.PParamsDelta era),
    TransTxBody ToCBOR era
  ) =>
  ToCBOR (TxBodyRaw era)
  where
  toCBOR x = encode (txSparse x)

-- ====================================================
-- Introduce TxBody as a newtype around a MemoBytes

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (Generic, Typeable)
  deriving newtype (SafeToHash)

deriving newtype instance
  (TransTxBody NoThunks era, Typeable era) => NoThunks (TxBody era)

deriving newtype instance
  (NFData (Core.TxOut era), CC.Crypto (Crypto era), NFData (Core.PParamsDelta era)) =>
  NFData (TxBody era)

deriving instance (Era era, TransTxBody Show era) => Show (TxBody era)

deriving instance (Era era, TransTxBody Eq era) => Eq (TxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    ( Era era,
      FromCBOR (Core.TxOut era),
      FromCBOR (Core.PParamsDelta era),
      ToCBOR (Core.PParamsDelta era)
    ) =>
    FromCBOR (Annotator (TxBody era))

-- | Pattern for use by external users
pattern TxBody ::
  (Era era, FromCBOR (Core.PParamsDelta era), TransTxBody ToCBOR era) =>
  Set (Core.TxIn (Crypto era)) ->
  StrictSeq (Core.TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  TxBody era
pattern TxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { _inputsX = _inputs,
            _outputsX = _outputs,
            _certsX = _certs,
            _wdrlsX = _wdrls,
            _txfeeX = _txfee,
            _ttlX = _ttl,
            _txUpdateX = _txUpdate,
            _mdHashX = _mdHash
          }
        _
      )
  where
    TxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      TxBodyConstr $ memoBytes (txSparse (TxBodyRaw _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash))

{-# COMPLETE TxBody #-}

-- =========================================
-- WellFormed era   instances

instance (Era era, c ~ Crypto era) => HashAnnotated (TxBody era) EraIndependentTxBody c

instance (Era era) => ToCBOR (TxBody era) where
  toCBOR (TxBodyConstr memo) = toCBOR memo

instance Crypto era ~ crypto => HasField "inputs" (TxBody era) (Set (Core.TxIn crypto)) where
  getField (TxBodyConstr (Memo m _)) = getField @"_inputsX" m

instance Core.TxOut era ~ out => HasField "outputs" (TxBody era) (StrictSeq out) where
  getField (TxBodyConstr (Memo m _)) = getField @"_outputsX" m

instance Crypto era ~ crypto => HasField "certs" (TxBody era) (StrictSeq (DCert crypto)) where
  getField (TxBodyConstr (Memo m _)) = getField @"_certsX" m

instance Crypto era ~ crypto => HasField "wdrls" (TxBody era) (Wdrl crypto) where
  getField (TxBodyConstr (Memo m _)) = getField @"_wdrlsX" m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = getField @"_txfeeX" m

instance HasField "ttl" (TxBody era) SlotNo where
  getField (TxBodyConstr (Memo m _)) = getField @"_ttlX" m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyConstr (Memo m _)) = getField @"_txUpdateX" m

instance
  Crypto era ~ crypto =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash crypto))
  where
  getField (TxBodyConstr (Memo m _)) = getField @"_mdHashX" m

instance c ~ Crypto era => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField _ = Set.empty

instance
  c ~ Crypto era =>
  HasField "txinputs_fee" (TxBody era) (Set (Core.TxIn c))
  where
  getField (TxBodyConstr (Memo m _)) = getField @"_inputsX" m

-- ===============================================================

instance-- use the weakest constraint necessary

  (Era era, TransTxOut ToCBOR era) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  (Era era, TransTxOut DecodeNonNegative era, Show (Core.Value era)) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = fromNotSharedCBOR

-- This instance does not do any sharing and is isomorphic to FromCBOR
-- use the weakest constraint necessary
instance
  (Era era, TransTxOut DecodeNonNegative era, Show (Core.Value era)) =>
  FromSharedCBOR (TxOut era)
  where
  type Share (TxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR _ =
    decodeRecordNamed "TxOut" (const 2) $ do
      cAddr <- fromCBOR
      coin <- decodeNonNegative
      pure $ TxOutCompact cAddr coin

witKeyHash :: WitVKey kr crypto -> KeyHash 'Witness crypto
witKeyHash = witVKeyHash
{-# DEPRECATED witKeyHash "In favor of `witVKeyHash`" #-}

wvkBytes :: WitVKey kr crypto -> BSL.ByteString
wvkBytes = witVKeyBytes
{-# DEPRECATED wvkBytes "In favor of `witVKeyBytes`" #-}
