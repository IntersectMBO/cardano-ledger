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
{-# LANGUAGE LambdaCase #-}
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
    Ix,
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    PoolCert (..),
    PoolMetadata (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    StakeCreds (..),
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
    -- eraIndTxBodyHash,
    TxOut (TxOut, TxOutCompact),
    Url,
    Wdrl (..),
    WitVKey (WitVKey, wvkBytes),
    --
    witKeyHash,
    --
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
    --
    TransTxId,
    TransTxOut,
    TransTxBody,

    -- * Deprecated
    TxId,
    TxIn,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
    FromCBOR (fromCBOR),
    Size,
    ToCBOR (..),
    TokenType (TypeMapLen, TypeMapLen64, TypeMapLenIndef),
    annotatorSlice,
    decodeWord,
    encodeListLen,
    encodePreEncoded,
    peekTokenType,
    serializeEncoding,
    szCases,
  )
import qualified Cardano.Crypto.Hash.Class as HS
import Cardano.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
  )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes
  ( DnsName,
    Port,
    StrictMaybe (..),
    UnitInterval,
    Url,
    invalidKey,
    isSNothing,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
  ( Credential (..),
    Ix,
    Ptr (..),
    StakeCredential,
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era
import Cardano.Ledger.Hashes (EraIndependentTxBody, ScriptHash)
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey,
    VerKeyVRF,
    asWitness,
    decodeSignedDSIGN,
    encodeSignedDSIGN,
    hashKey,
    hashSignature,
  )
import Cardano.Ledger.SafeHash (HashAnnotated, SafeToHash)
import Cardano.Ledger.Serialization
  ( CBORGroup (..),
    CborSeq (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeNullMaybe,
    decodeRecordNamed,
    decodeRecordSum,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    encodeNullMaybe,
    ipv4FromCBOR,
    ipv4ToCBOR,
    ipv6FromCBOR,
    ipv6ToCBOR,
    listLenInt,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.CompactAddr
  ( CompactAddr,
    compactAddr,
    decompactAddr,
  )
import Cardano.Ledger.Shelley.Constraints (TransValue)
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import qualified Cardano.Ledger.TxIn as Core
import Cardano.Ledger.Val (DecodeNonNegative (..))
import Cardano.Prelude (HeapWords (..), panic)
import Control.DeepSeq (NFData (rnf))
import Control.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (ShortByteString, pack)
import Data.Coders
  ( Decode (..),
    Density (..),
    Dual (..),
    Encode (..),
    Field,
    Wrapped (..),
    decode,
    encode,
    field,
    (!>),
  )
import Data.Constraint (Constraint)
import Data.Foldable (asum)
import Data.IP (IPv4, IPv6)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class
  ( AllowThunksIn (..),
    InspectHeapNamed (..),
    NoThunks (..),
  )
import Quiet

-- ========================================================================

instance HasExp (StakeCreds era) (Map (Credential 'Staking era) SlotNo) where
  toExp (StakeCreds x) = Base MapR x

instance Embed (StakeCreds era) (Map (Credential 'Staking era) SlotNo) where
  toBase (StakeCreds x) = x
  fromBase x = StakeCreds x

-- | The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: !(StakeCredential crypto),
    _delegatee :: !(KeyHash 'StakePool crypto)
  }
  deriving (Eq, Generic, Show, NFData)

instance NoThunks (Delegation crypto)

data PoolMetadata = PoolMetadata
  { _poolMDUrl :: !Url,
    _poolMDHash :: !ByteString
  }
  deriving (Eq, Ord, Generic, Show)

deriving instance NFData PoolMetadata

instance ToJSON PoolMetadata where
  toJSON pmd =
    Aeson.object
      [ "url" .= _poolMDUrl pmd,
        "hash" .= (Text.decodeLatin1 . B16.encode) (_poolMDHash pmd)
      ]

instance FromJSON PoolMetadata where
  parseJSON =
    Aeson.withObject "PoolMetadata" $ \obj -> do
      url <- obj .: "url"
      hash <- explicitParseField parseJsonBase16 obj "hash"
      return $ PoolMetadata url hash

parseJsonBase16 :: Value -> Parser ByteString
parseJsonBase16 v = do
  s <- parseJSON v
  case B16.decode (Char8.pack s) of
    Right bs -> return bs
    Left msg -> fail msg

instance NoThunks PoolMetadata

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    SingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
  | -- | An @A@ or @AAAA@ DNS record
    SingleHostName !(StrictMaybe Port) !DnsName
  | -- | A @SRV@ DNS record
    MultiHostName !DnsName
  deriving (Eq, Ord, Generic, Show)

instance FromJSON StakePoolRelay where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum
        [ explicitParseField parser1 obj "single host address",
          explicitParseField parser2 obj "single host name",
          explicitParseField parser3 obj "multi host name"
        ]
    where
      parser1 = Aeson.withObject "SingleHostAddr" $ \obj ->
        SingleHostAddr
          <$> obj .:? "port" .!= SNothing
          <*> obj .:? "IPv4" .!= SNothing
          <*> obj .:? "IPv6" .!= SNothing
      parser2 = Aeson.withObject "SingleHostName" $ \obj ->
        SingleHostName
          <$> obj .:? "port" .!= SNothing
          <*> obj .: "dnsName"
      parser3 = Aeson.withObject "MultiHostName" $ \obj ->
        MultiHostName
          <$> obj .: "dnsName"

instance ToJSON StakePoolRelay where
  toJSON (SingleHostAddr port ipv4 ipv6) =
    Aeson.object
      [ "single host address"
          .= Aeson.object
            [ "port" .= port,
              "IPv4" .= ipv4,
              "IPv6" .= ipv6
            ]
      ]
  toJSON (SingleHostName port dnsName) =
    Aeson.object
      [ "single host name"
          .= Aeson.object
            [ "port" .= port,
              "dnsName" .= dnsName
            ]
      ]
  toJSON (MultiHostName dnsName) =
    Aeson.object
      [ "multi host name"
          .= Aeson.object
            [ "dnsName" .= dnsName
            ]
      ]

instance NoThunks StakePoolRelay

instance NFData StakePoolRelay

instance ToCBOR StakePoolRelay where
  toCBOR (SingleHostAddr p ipv4 ipv6) =
    encodeListLen 4
      <> toCBOR (0 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> encodeNullMaybe ipv4ToCBOR (strictMaybeToMaybe ipv4)
      <> encodeNullMaybe ipv6ToCBOR (strictMaybeToMaybe ipv6)
  toCBOR (SingleHostName p n) =
    encodeListLen 3
      <> toCBOR (1 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> toCBOR n
  toCBOR (MultiHostName n) =
    encodeListLen 2
      <> toCBOR (2 :: Word8)
      <> toCBOR n

instance FromCBOR StakePoolRelay where
  fromCBOR = decodeRecordSum "StakePoolRelay" $
    \case
      0 ->
        (\x y z -> (4, SingleHostAddr x y z))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv4FromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv6FromCBOR)
      1 ->
        (\x y -> (3, SingleHostName x y))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> fromCBOR
      2 -> do
        x <- fromCBOR
        pure (2, MultiHostName x)
      k -> invalidKey k

-- | A stake pool.
data PoolParams crypto = PoolParams
  { _poolId :: !(KeyHash 'StakePool crypto),
    _poolVrf :: !(Hash crypto (VerKeyVRF crypto)),
    _poolPledge :: !Coin,
    _poolCost :: !Coin,
    _poolMargin :: !UnitInterval,
    _poolRAcnt :: !(RewardAcnt crypto),
    _poolOwners :: !(Set (KeyHash 'Staking crypto)),
    _poolRelays :: !(StrictSeq StakePoolRelay),
    _poolMD :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (ToCBOR) via CBORGroup (PoolParams crypto)
  deriving (FromCBOR) via CBORGroup (PoolParams crypto)

instance NoThunks (PoolParams crypto)

deriving instance NFData (PoolParams crypto)

newtype Wdrl crypto = Wdrl {unWdrl :: Map (RewardAcnt crypto) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData)

instance CC.Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = mapToCBOR . unWdrl

instance CC.Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl <$> mapFromCBOR

instance CC.Crypto crypto => ToJSON (PoolParams crypto) where
  toJSON pp =
    Aeson.object
      [ "publicKey" .= _poolId pp, -- TODO publicKey is an unfortunate name, should be poolId
        "vrf" .= _poolVrf pp,
        "pledge" .= _poolPledge pp,
        "cost" .= _poolCost pp,
        "margin" .= _poolMargin pp,
        "rewardAccount" .= _poolRAcnt pp,
        "owners" .= _poolOwners pp,
        "relays" .= _poolRelays pp,
        "metadata" .= _poolMD pp
      ]

instance CC.Crypto crypto => FromJSON (PoolParams crypto) where
  parseJSON =
    Aeson.withObject "PoolParams" $ \obj ->
      PoolParams
        <$> obj .: "publicKey" -- TODO publicKey is an unfortunate name, should be poolId
        <*> obj .: "vrf"
        <*> obj .: "pledge"
        <*> obj .: "cost"
        <*> obj .: "margin"
        <*> obj .: "rewardAccount"
        <*> obj .: "owners"
        <*> obj .: "relays"
        <*> obj .: "metadata"

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

instance
  ( Crypto era ~ c,
    Era era,
    TransValue Show era
  ) =>
  HasField "compactAddress" (TxOut era) (CompactAddr c)
  where
  getField (TxOutCompact a _) = a

-- ---------------------------
-- WellFormed instances

instance (Compactible v, v ~ Core.Value era) => HasField "value" (TxOut era) v where
  getField (TxOutCompact _ v) = fromCompact v

instance (CC.Crypto c, c ~ Crypto era) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOutCompact a _) = decompactAddr a

data DelegCert crypto
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential crypto)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential crypto)
  | -- | A stake delegation certificate.
    Delegate !(Delegation crypto)
  deriving (Show, Generic, Eq, NFData)

data PoolCert crypto
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams crypto)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool crypto) !EpochNo
  deriving (Show, Generic, Eq, NFData)

-- | Genesis key delegation certificate
data GenesisDelegCert crypto
  = GenesisDelegCert
      !(KeyHash 'Genesis crypto)
      !(KeyHash 'GenesisDelegate crypto)
      !(Hash crypto (VerKeyVRF crypto))
  deriving (Show, Generic, Eq, NFData)

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData)

deriving instance NoThunks MIRPot

instance ToCBOR MIRPot where
  toCBOR ReservesMIR = toCBOR (0 :: Word8)
  toCBOR TreasuryMIR = toCBOR (1 :: Word8)

instance FromCBOR MIRPot where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure ReservesMIR
      1 -> pure TreasuryMIR
      k -> invalidKey k

-- | MIRTarget specifies if funds from either the reserves
-- or the treasury are to be handed out to a collection of
-- reward accounts or instead transfered to the opposite pot.
data MIRTarget crypto
  = StakeAddressesMIR (Map (Credential 'Staking crypto) DeltaCoin)
  | SendToOppositePotMIR Coin
  deriving (Show, Generic, Eq, NFData)

deriving instance NoThunks (MIRTarget crypto)

instance
  CC.Crypto crypto =>
  FromCBOR (MIRTarget crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeMapLen -> StakeAddressesMIR <$> mapFromCBOR
      TypeMapLen64 -> StakeAddressesMIR <$> mapFromCBOR
      TypeMapLenIndef -> StakeAddressesMIR <$> mapFromCBOR
      _ -> SendToOppositePotMIR <$> fromCBOR

instance
  CC.Crypto crypto =>
  ToCBOR (MIRTarget crypto)
  where
  toCBOR (StakeAddressesMIR m) = mapToCBOR m
  toCBOR (SendToOppositePotMIR c) = toCBOR c

-- | Move instantaneous rewards certificate
data MIRCert crypto = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: MIRTarget crypto
  }
  deriving (Show, Generic, Eq, NFData)

instance
  CC.Crypto crypto =>
  FromCBOR (MIRCert crypto)
  where
  fromCBOR =
    decodeRecordNamed "MIRCert" (const 2) (MIRCert <$> fromCBOR <*> fromCBOR)

instance
  CC.Crypto crypto =>
  ToCBOR (MIRCert crypto)
  where
  toCBOR (MIRCert pot targets) =
    encodeListLen 2
      <> toCBOR pot
      <> toCBOR targets

-- | A heavyweight certificate.
data DCert crypto
  = DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  | DCertMir !(MIRCert crypto)
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (DelegCert crypto)

instance NoThunks (PoolCert crypto)

instance NoThunks (GenesisDelegCert crypto)

instance NoThunks (MIRCert crypto)

instance NoThunks (DCert crypto)

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
  (CC.Crypto (Crypto era), NFData (Core.PParamsDelta era)) =>
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

-- | This Dual follows strategy of the the old code, for backward compatibility,
--   of serializing StrictMaybe values. The strategy is to serialise only the
--   value: 'x' in a (SJust x). The SNothing and the SJust part are never
--   written to the serialised bytes but are supplied by the Omit capability.
--   Be sure and wrap a (Omit isNothing (Key v _)) around use of this Dual.
--   Like this: (Omit isNothing (Key v (ED omitStrictNothingDual x))).
--   Neither the Omit or the key is needed for Decoders.
omitStrictNothingDual :: (FromCBOR t, ToCBOR t) => Dual (StrictMaybe t)
omitStrictNothingDual = Dual (toCBOR . fromJust . strictMaybeToMaybe) (SJust <$> fromCBOR)

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody ::
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (Core.PParamsDelta era),
    ToCBOR (Core.PParamsDelta era)
  ) =>
  Word ->
  Field (TxBodyRaw era)
boxBody 0 = field (\x tx -> tx {_inputsX = x}) (D (decodeSet fromCBOR))
boxBody 1 = field (\x tx -> tx {_outputsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 4 = field (\x tx -> tx {_certsX = x}) (D (decodeStrictSeq fromCBOR))
boxBody 5 = field (\x tx -> tx {_wdrlsX = x}) From
boxBody 2 = field (\x tx -> tx {_txfeeX = x}) From
boxBody 3 = field (\x tx -> tx {_ttlX = x}) From
boxBody 6 = field (\x tx -> tx {_txUpdateX = x}) (DD omitStrictNothingDual)
boxBody 7 = field (\x tx -> tx {_mdHashX = x}) (DD omitStrictNothingDual)
boxBody n = field (\_ t -> t) (Invalid n)

-- | Tells how to serialise each field, and what tag to label it with in the
--   serialisation. boxBody and txSparse should be Duals, visually inspect
--   The key order looks strange but was choosen for backward compatibility.
txSparse ::
  (TransTxBody ToCBOR era, FromCBOR (Core.PParamsDelta era), Era era) =>
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
    !> Omit isSNothing (Key 6 (ED omitStrictNothingDual update))
    !> Omit isSNothing (Key 7 (ED omitStrictNothingDual hash))

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
  (CC.Crypto (Crypto era), NFData (Core.PParamsDelta era)) =>
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

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey kr crypto = WitVKey'
  { wvkKey' :: !(VKey kr crypto),
    wvkSig' :: !(SignedDSIGN crypto (Hash crypto EraIndependentTxBody)),
    -- | Hash of the witness vkey. We store this here to avoid repeated hashing
    --   when used in ordering.
    wvkKeyHash :: !(KeyHash 'Witness crypto),
    wvkBytes :: BSL.ByteString
  }
  deriving (Generic)

deriving instance CC.Crypto crypto => Show (WitVKey kr crypto)

deriving instance CC.Crypto crypto => Eq (WitVKey kr crypto)

deriving via
  (AllowThunksIn '["wvkBytes"] (WitVKey kr crypto))
  instance
    (CC.Crypto crypto, Typeable kr) => NoThunks (WitVKey kr crypto)

pattern WitVKey ::
  (Typeable kr, CC.Crypto crypto) =>
  VKey kr crypto ->
  SignedDSIGN crypto (Hash crypto EraIndependentTxBody) ->
  WitVKey kr crypto
pattern WitVKey k s <-
  WitVKey' k s _ _
  where
    WitVKey k s =
      let bytes =
            serializeEncoding $
              encodeListLen 2
                <> toCBOR k
                <> encodeSignedDSIGN s
          hash = asWitness $ hashKey k
       in WitVKey' k s hash bytes

{-# COMPLETE WitVKey #-}

witKeyHash ::
  WitVKey kr crypto ->
  KeyHash 'Witness crypto
witKeyHash (WitVKey' _ _ kh _) = kh

instance (Typeable kr, CC.Crypto crypto) => Ord (WitVKey kr crypto) where
  compare x y =
    -- It is advised against comparison on keys and signatures directly,
    -- therefore we use hashes of verification keys and signatures for
    -- implementing this Ord instance. Note that we do not need to memoize the
    -- hash of a signature, like it is done with the hash of a key, because Ord
    -- instance is only used for Sets of WitVKeys and it would be a mistake to
    -- have two WitVKeys in a same Set for different transactions. Therefore
    -- comparison on signatures is unlikely to happen and is only needed for
    -- compliance with Ord laws.
    comparing wvkKeyHash x y <> comparing (hashSignature @crypto . wvkSig') x y

newtype StakeCreds crypto = StakeCreds
  { unStakeCreds :: Map (Credential 'Staking crypto) SlotNo
  }
  deriving (Eq, Generic)
  deriving (Show) via (Quiet (StakeCreds crypto))
  deriving newtype (NFData, NoThunks, ToJSON, FromJSON)

deriving newtype instance
  CC.Crypto crypto =>
  FromCBOR (StakeCreds crypto)

deriving newtype instance
  CC.Crypto crypto =>
  ToCBOR (StakeCreds crypto)

-- CBOR

instance
  CC.Crypto crypto =>
  ToCBOR (DCert crypto)
  where
  toCBOR = \case
    -- DCertDeleg
    DCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR cred
    DCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR cred
    DCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR cred
        <> toCBOR poolkh
    -- DCertPool
    DCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> toCBOR (3 :: Word8)
        <> toCBORGroup poolParams
    DCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch
    -- DCertGenesis
    DCertGenesis (GenesisDelegCert gk kh vrf) ->
      encodeListLen 4
        <> toCBOR (5 :: Word8)
        <> toCBOR gk
        <> toCBOR kh
        <> toCBOR vrf
    -- DCertMIR
    DCertMir mir ->
      encodeListLen 2
        <> toCBOR (6 :: Word8)
        <> toCBOR mir

instance
  CC.Crypto crypto =>
  FromCBOR (DCert crypto)
  where
  fromCBOR = decodeRecordSum "DCert crypto" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . RegKey $ x)
      1 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . DeRegKey $ x)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- fromCBORGroup
        pure (fromIntegral (1 + listLenInt group), DCertPool (RegPool group))
      4 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, DCertGenesis $ GenesisDelegCert a b c)
      6 -> do
        x <- fromCBOR
        pure (2, DCertMir x)
      k -> invalidKey k

instance-- use the weakest constraint necessary

  (Era era, TransTxOut ToCBOR era) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance-- use the weakest constraint necessary

  (Era era, TransTxOut DecodeNonNegative era, Show (Core.Value era)) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = decodeRecordNamed "TxOut" (const 2) $ do
    cAddr <- fromCBOR
    coin <- decodeNonNegative
    pure $ TxOutCompact cAddr coin

instance
  (Typeable kr, CC.Crypto crypto) =>
  ToCBOR (WitVKey kr crypto)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . wvkBytes

instance
  (Typeable kr, CC.Crypto crypto) =>
  FromCBOR (Annotator (WitVKey kr crypto))
  where
  fromCBOR =
    annotatorSlice $
      decodeRecordNamed "WitVKey" (const 2) $
        fmap pure $
          mkWitVKey <$> fromCBOR <*> decodeSignedDSIGN
    where
      mkWitVKey k sig = WitVKey' k sig (asWitness $ hashKey k)

instance ToCBOR PoolMetadata where
  toCBOR (PoolMetadata u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetadata where
  fromCBOR = do
    decodeRecordNamed "PoolMetadata" (const 2) (PoolMetadata <$> fromCBOR <*> fromCBOR)

-- | The size of the '_poolOwners' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolOwners = SizeOfPoolOwners

instance ToCBOR SizeOfPoolOwners where
  toCBOR = panic "The `SizeOfPoolOwners` type cannot be encoded!"

-- | The size of the '_poolRelays' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolRelays = SizeOfPoolRelays

instance ToCBOR SizeOfPoolRelays where
  toCBOR = panic "The `SizeOfPoolRelays` type cannot be encoded!"

instance
  CC.Crypto crypto =>
  ToCBORGroup (PoolParams crypto)
  where
  toCBORGroup poolParams =
    toCBOR (_poolId poolParams)
      <> toCBOR (_poolVrf poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> encodeFoldable (_poolOwners poolParams)
      <> toCBOR (CborSeq (StrictSeq.fromStrict (_poolRelays poolParams)))
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe (_poolMD poolParams))

  encodedGroupSizeExpr size' proxy =
    encodedSizeExpr size' (_poolId <$> proxy)
      + encodedSizeExpr size' (_poolVrf <$> proxy)
      + encodedSizeExpr size' (_poolPledge <$> proxy)
      + encodedSizeExpr size' (_poolCost <$> proxy)
      + encodedSizeExpr size' (_poolMargin <$> proxy)
      + encodedSizeExpr size' (_poolRAcnt <$> proxy)
      + 2
      + poolSize * encodedSizeExpr size' (elementProxy (_poolOwners <$> proxy))
      + 2
      + relaySize * encodedSizeExpr size' (elementProxy (_poolRelays <$> proxy))
      + szCases
        [ Case "Nothing" 1,
          Case "Just" $ encodedSizeExpr size' (elementProxy (_poolMD <$> proxy))
        ]
    where
      poolSize, relaySize :: Size
      poolSize = size' (Proxy @SizeOfPoolOwners)
      relaySize = size' (Proxy @SizeOfPoolRelays)
      elementProxy :: Proxy (f a) -> Proxy a
      elementProxy _ = Proxy

  listLen _ = 9
  listLenBound _ = 9

instance
  CC.Crypto crypto =>
  FromCBORGroup (PoolParams crypto)
  where
  fromCBORGroup = do
    hk <- fromCBOR
    vrf <- fromCBOR
    pledge <- fromCBOR
    cost <- fromCBOR
    margin <- fromCBOR
    ra <- fromCBOR
    owners <- decodeSet fromCBOR
    relays <- decodeStrictSeq fromCBOR
    md <- decodeNullMaybe fromCBOR
    pure $
      PoolParams
        { _poolId = hk,
          _poolVrf = vrf,
          _poolPledge = pledge,
          _poolCost = cost,
          _poolMargin = margin,
          _poolRAcnt = ra,
          _poolOwners = owners,
          _poolRelays = relays,
          _poolMD = maybeToStrictMaybe md
        }

-- DEPRECATED

{-# DEPRECATED TxId "Import from Cardano.Ledger.TxIn instead" #-}

type TxId = Core.TxId

{-# DEPRECATED TxIn "Import from Cardano.Ledger.TxIn instead" #-}

type TxIn = Core.TxIn
