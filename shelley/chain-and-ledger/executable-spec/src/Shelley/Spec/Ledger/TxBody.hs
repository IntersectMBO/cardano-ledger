{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    Ix,
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolMetaData (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    StakeCreds (..),
    StakePoolRelay (..),
    TxBody
      ( TxBody,
        TxBodyY,
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash
      ),
    TxBodyX (..),
    TxId (..),
    TxIn (TxIn, ..),
    EraIndependentTxBody,
    eraIndTxBodyHash,
    TxOut (TxOut, TxOutCompact),
    Url,
    Wdrl (..),
    WitVKey (WitVKey, wvkBytes),
    --
    witKeyHash,
    --
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
    FromCBOR (fromCBOR),
    Size,
    ToCBOR (..),
    annotatorSlice,
    decodeWord,
    encodeListLen,
    encodePreEncoded,
    serializeEncoding,
    szCases,
  )
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Cardano.Ledger.Val (Val)
import Cardano.Prelude
  ( decodeEitherBase16,
    panic,
  )
import Control.DeepSeq (NFData (rnf))
import Control.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
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
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.IP (IPv4, IPv6)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (AllowThunksIn (..), InspectHeapNamed (..), NoThunks (..))
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
  )
import Shelley.Spec.Ledger.BaseTypes
  ( DnsName,
    Port,
    StrictMaybe (..),
    UnitInterval,
    Url,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.CompactAddr
  ( CompactAddr,
    compactAddr,
    decompactAddr,
  )
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ix,
    Ptr (..),
    StakeCredential,
  )
import Shelley.Spec.Ledger.Hashing
import Shelley.Spec.Ledger.Keys
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
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Serialization
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
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

-- ========================================================================

instance HasExp (StakeCreds era) (Map (Credential 'Staking era) SlotNo) where
  toExp (StakeCreds x) = Base MapR x

instance Embed (StakeCreds era) (Map (Credential 'Staking era) SlotNo) where
  toBase (StakeCreds x) = x
  fromBase x = StakeCreds x

-- | The delegation of one stake key to another.
data Delegation era = Delegation
  { _delegator :: !(StakeCredential era),
    _delegatee :: !(KeyHash 'StakePool (Crypto era))
  }
  deriving (Eq, Generic, Show)

instance NoThunks (Delegation era)

data PoolMetaData = PoolMetaData
  { _poolMDUrl :: !Url,
    _poolMDHash :: !ByteString
  }
  deriving (Eq, Ord, Generic, Show)

deriving instance NFData PoolMetaData

instance ToJSON PoolMetaData where
  toJSON pmd =
    Aeson.object
      [ "url" .= _poolMDUrl pmd,
        "hash" .= (Text.decodeLatin1 . Base16.encode) (_poolMDHash pmd)
      ]

instance FromJSON PoolMetaData where
  parseJSON =
    Aeson.withObject "PoolMetaData" $ \obj -> do
      url <- obj .: "url"
      hash <- explicitParseField parseJsonBase16 obj "hash"
      return $ PoolMetaData url hash

parseJsonBase16 :: Value -> Parser ByteString
parseJsonBase16 v = do
  s <- parseJSON v
  case decodeEitherBase16 (Char8.pack s) of
    Right bs -> return bs
    Left msg -> fail msg

instance NoThunks PoolMetaData

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
data PoolParams era = PoolParams
  { _poolId :: !(KeyHash 'StakePool (Crypto era)),
    _poolVrf :: !(Hash (Crypto era) (VerKeyVRF (Crypto era))),
    _poolPledge :: !Coin,
    _poolCost :: !Coin,
    _poolMargin :: !UnitInterval,
    _poolRAcnt :: !(RewardAcnt era),
    _poolOwners :: !(Set (KeyHash 'Staking (Crypto era))),
    _poolRelays :: !(StrictSeq StakePoolRelay),
    _poolMD :: !(StrictMaybe PoolMetaData)
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (ToCBOR) via CBORGroup (PoolParams era)
  deriving (FromCBOR) via CBORGroup (PoolParams era)

instance NoThunks (PoolParams era)

deriving instance NFData (PoolParams era)

newtype Wdrl era = Wdrl {unWdrl :: Map (RewardAcnt era) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData)

instance Era era => ToCBOR (Wdrl era) where
  toCBOR = mapToCBOR . unWdrl

instance Era era => FromCBOR (Wdrl era) where
  fromCBOR = Wdrl <$> mapFromCBOR

instance Era era => ToJSON (PoolParams era) where
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

instance Era era => FromJSON (PoolParams era) where
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

-- ===================================================================================
-- Because we expect other Era's to import and use TxId, TxIn, TxOut, we use the weakest
-- constraint possible when deriving their instances. A Stronger constraint, Gathering
-- many constraints together, like:  type Strong = (C1 x, C2 x, ..., Cn x)
-- may make this file look systematic by having things like:
-- derving instance (Strong x) => Foo x,  for many Foo (Eq, Show, NfData, etc) BUT this
-- forces unnecessary requirements on any new Era which tries to embed one of these
-- types in their own datatypes, if they then try and derive (Foo TheirDataType).
-- ====================================================================================

-- | A unique ID of a transaction, which is computable from the transaction.
newtype TxId era = TxId {_unTxId :: Hash (Crypto era) EraIndependentTxBody}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks)

deriving newtype instance
  (Era era) => -- weakest constraint
  ToCBOR (TxId era)

deriving newtype instance
  (Era era) => -- weakest constraint
  FromCBOR (TxId era)

deriving newtype instance (Era era) => NFData (TxId era)

-- | The input of a UTxO.
data TxIn era = TxInCompact {-# UNPACK #-} !(TxId era) {-# UNPACK #-} !Word64
  deriving (Generic)

-- TODO: We will also want to have the TxId be compact, but the representation
-- depends on the era. NOT SURE ABOUT this. The TxId is always a Hash, Can't get more compact than that.

pattern TxIn ::
  Era era =>
  TxId era ->
  Natural -> -- TODO We might want to change this to Word64 generally
  TxIn era
pattern TxIn addr index <-
  TxInCompact addr (fromIntegral -> index)
  where
    TxIn addr index =
      TxInCompact addr (fromIntegral index)

{-# COMPLETE TxIn #-}

deriving instance Ord (TxIn era)

deriving instance Eq (TxIn era)

deriving instance Show (TxIn era)

deriving instance Era era => NFData (TxIn era)

instance NoThunks (TxIn era)

-- | The output of a UTxO.
data TxOut era
  = TxOutCompact
      {-# UNPACK #-} !(CompactAddr era)
      !(CompactForm (Core.Value era))

instance
  (Show (Core.Value era), Era era, Compactible (Core.Value era)) => -- Use the weakest constraint possible here
  Show (TxOut era)
  where
  show = show . viewCompactTxOut

deriving stock instance
  -- weakest constraint
  (Eq (Core.Value era), Compactible (Core.Value era)) =>
  Eq (TxOut era)

instance NFData (TxOut era) where
  rnf = (`seq` ())

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern TxOut ::
  ShelleyBased era =>
  Addr era ->
  Core.Value era ->
  TxOut era
pattern TxOut addr vl <-
  (viewCompactTxOut -> (addr, vl))
  where
    TxOut addr vl =
      TxOutCompact (compactAddr addr) (toCompact vl)

{-# COMPLETE TxOut #-}

viewCompactTxOut ::
  forall era.
  (Era era, Compactible (Core.Value era)) => -- Use the weakest constraint possible here
  TxOut era ->
  (Addr era, Core.Value era)
viewCompactTxOut (TxOutCompact bs c) = (addr, val)
  where
    addr = decompactAddr bs
    val = fromCompact c

data DelegCert era
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential era)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential era)
  | -- | A stake delegation certificate.
    Delegate !(Delegation era)
  deriving (Show, Generic, Eq)

data PoolCert era
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams era)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool (Crypto era)) !EpochNo
  deriving (Show, Generic, Eq)

-- | Genesis key delegation certificate
data GenesisDelegCert era
  = GenesisDelegCert
      !(KeyHash 'Genesis (Crypto era))
      !(KeyHash 'GenesisDelegate (Crypto era))
      !(Hash (Crypto era) (VerKeyVRF (Crypto era)))
  deriving (Show, Generic, Eq)

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq)

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

-- | Move instantaneous rewards certificate
data MIRCert era = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: (Map (Credential 'Staking era) Coin)
  }
  deriving (Show, Generic, Eq)

instance
  (Era era, Typeable (Core.Script era), FromCBOR (Annotator (Core.Script era))) =>
  FromCBOR (MIRCert era)
  where
  fromCBOR = decodeRecordNamed "SingleHostAddr" (const 2) $ do
    pot <- fromCBOR
    values <- mapFromCBOR
    pure $ MIRCert pot values

instance
  (Era era, ToCBOR (Core.Script era)) =>
  ToCBOR (MIRCert era)
  where
  toCBOR (MIRCert pot values) =
    encodeListLen 2
      <> toCBOR pot
      <> mapToCBOR values

-- | A heavyweight certificate.
data DCert era
  = DCertDeleg !(DelegCert era)
  | DCertPool !(PoolCert era)
  | DCertGenesis !(GenesisDelegCert era)
  | DCertMir !(MIRCert era)
  deriving (Show, Generic, Eq)

instance NoThunks (DelegCert era)

instance NoThunks (PoolCert era)

instance NoThunks (GenesisDelegCert era)

instance NoThunks (MIRCert era)

instance NoThunks (DCert era)

-- ===========================================================================
-- Since TxBody has fees (which are Values) and Scripts (inside hashes),
-- and both are type families, we need to ensure that these families have
-- the minimum amount of properties, to make the correct instances for TxBody

-- | Needed for Show, Eq etc instances
type ProperVal era =
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Value era),
    Eq (Core.Value era),
    Val (Core.Value era)
  )

-- | Needed for FromCBOR instances
type ProperFrom era =
  ( Era era,
    Typeable era,
    FromCBOR (Core.Value era),
    Typeable (Core.Script era),
    FromCBOR (CompactForm (Core.Value era)),
    FromCBOR (Annotator (Core.Script era))
  )

-- | Needed for ToCBOR instances
type ProperTo era =
  ( Era era,
    ToCBOR (Core.Value era),
    ToCBOR (Core.Script era),
    ToCBOR (CompactForm (Core.Value era))
  )

-- ==============================
-- The underlying type for TxBody

data TxBodyX era = TxBodyX
  { _inputsX :: !(Set (TxIn era)),
    _outputsX :: !(StrictSeq (TxOut era)),
    _certsX :: !(StrictSeq (DCert era)),
    _wdrlsX :: !(Wdrl era),
    _txfeeX :: !Coin,
    _ttlX :: !SlotNo,
    _txUpdateX :: !(StrictMaybe (Update era)),
    _mdHashX :: !(StrictMaybe (MetaDataHash era))
  }
  deriving (Generic, NoThunks, Typeable, NFData)

deriving instance (Era era, ProperVal era) => Eq (TxBodyX era)

deriving instance (Era era, ProperVal era) => Show (TxBodyX era)

instance ProperFrom era => FromCBOR (TxBodyX era) where
  fromCBOR = decode (SparseKeyed "TxBody" baseTxBodyX boxBody [(0, "inputs"), (1, "outputs"), (2, "fee"), (3, "ttl")])

instance ProperFrom era => FromCBOR (Annotator (TxBodyX era)) where
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

isSNothing :: StrictMaybe a -> Bool
isSNothing SNothing = True
isSNothing _ = False

-- | Choose a de-serialiser when given the key (of type Word).
--   Wrap it in a Field which pairs it with its update function which
--   changes only the field being deserialised.
boxBody :: ProperFrom era => Word -> Field (TxBodyX era)
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
txSparse :: ProperTo era => TxBodyX era -> Encode ( 'Closed 'Sparse) (TxBodyX era)
txSparse (TxBodyX input output cert wdrl fee ttl update hash) =
  Keyed (\i o f t c w u h -> TxBodyX i o c w f t u h)
    !> Key 0 (E encodeFoldable input) -- We don't have to send these in TxBodyX order
    !> Key 1 (E encodeFoldable output) -- Just hack up a fake constructor with the lambda.
    !> Key 2 (To fee)
    !> Key 3 (To ttl)
    !> Omit null (Key 4 (E encodeFoldable cert))
    !> Omit (null . unWdrl) (Key 5 (To wdrl))
    !> Omit isSNothing (Key 6 (ED omitStrictNothingDual update))
    !> Omit isSNothing (Key 7 (ED omitStrictNothingDual hash))

-- The initial TxBody. We will overide some of these fields as we build a TxBody,
-- adding one field at a time, using optional serialisers, inside the Pattern.
baseTxBodyX :: TxBodyX era
baseTxBodyX =
  TxBodyX
    { _inputsX = Set.empty,
      _outputsX = StrictSeq.empty,
      _txfeeX = Coin 0,
      _ttlX = SlotNo 0,
      _certsX = StrictSeq.empty,
      _wdrlsX = Wdrl Map.empty,
      _txUpdateX = SNothing,
      _mdHashX = SNothing
    }

instance ProperTo era => ToCBOR (TxBodyX era) where
  toCBOR x = encode (txSparse x)

-- ====================================================
-- Introduce TxBody as a newtype around a MemoBytes

newtype TxBody era = TxBodyY (MemoBytes (TxBodyX era))
  deriving (Generic, Typeable)
  deriving newtype (NoThunks, NFData)

deriving instance ProperVal era => Show (TxBody era)

deriving instance ProperVal era => Eq (TxBody era)

deriving via
  (Mem (TxBodyX era))
  instance
    (ProperFrom era) =>
    FromCBOR (Annotator (TxBody era))

-- | Pattern for use by external users
pattern TxBody ::
  ProperTo era =>
  Set (TxIn era) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Wdrl era ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (MetaDataHash era) ->
  TxBody era
pattern TxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBodyY
    ( Memo
        ( TxBodyX
            { _inputsX = _inputs,
              _outputsX = _outputs,
              _certsX = _certs,
              _wdrlsX = _wdrls,
              _txfeeX = _txfee,
              _ttlX = _ttl,
              _txUpdateX = _txUpdate,
              _mdHashX = _mdHash
            }
          )
        _
      )
  where
    TxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      TxBodyY $ memoBytes (txSparse (TxBodyX _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash))

{-# COMPLETE TxBody #-}

instance Era era => HashAnnotated (TxBody era) era where
  type HashIndex (TxBody era) = EraIndependentTxBody

instance (Era era) => ToCBOR (TxBody era) where
  toCBOR (TxBodyY memo) = toCBOR memo

-- ==========================================================================
-- Here is where we declare that in the (ShelleyEra c) The abstract type family
-- Core.TxBody is set to THIS TxBody,The one we defined a few lines above.

type instance Core.TxBody (ShelleyEra c) = TxBody (ShelleyEra c)

-- ===========================================================================

instance HasField "inputs" (TxBody e) (Set (TxIn e)) where
  getField (TxBodyY (Memo m _)) = getField @"_inputsX" m

instance HasField "outputs" (TxBody era) (StrictSeq (TxOut era)) where
  getField (TxBodyY (Memo m _)) = getField @"_outputsX" m

instance HasField "certs" (TxBody era) (StrictSeq (DCert era)) where
  getField (TxBodyY (Memo m _)) = getField @"_certsX" m

instance HasField "wdrls" (TxBody era) (Wdrl era) where
  getField (TxBodyY (Memo m _)) = getField @"_wdrlsX" m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyY (Memo m _)) = getField @"_txfeeX" m

instance HasField "ttl" (TxBody era) SlotNo where
  getField (TxBodyY (Memo m _)) = getField @"_ttlX" m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyY (Memo m _)) = getField @"_txUpdateX" m

instance HasField "mdHash" (TxBody era) (StrictMaybe (MetaDataHash era)) where
  getField (TxBodyY (Memo m _)) = getField @"_mdHashX" m

-- ===============================================================

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey kr era = WitVKey'
  { wvkKey' :: !(VKey kr (Crypto era)),
    wvkSig' :: !(SignedDSIGN (Crypto era) (Hash (Crypto era) EraIndependentTxBody)),
    -- | Hash of the witness vkey. We store this here to avoid repeated hashing
    --   when used in ordering.
    wvkKeyHash :: !(KeyHash 'Witness (Crypto era)),
    wvkBytes :: BSL.ByteString
  }
  deriving (Generic)

deriving instance (Era era) => Show (WitVKey kr era)

deriving instance (Era era) => Eq (WitVKey kr era)

deriving via
  (AllowThunksIn '["wvkBytes"] (WitVKey kr era))
  instance
    (Era era, Typeable kr) => NoThunks (WitVKey kr era)

instance (Era era, Typeable kr) => HashAnnotated (WitVKey kr era) era where
  type HashIndex (WitVKey kr era) = EraIndependentWitVKey

pattern WitVKey ::
  (Typeable kr, Era era) =>
  VKey kr (Crypto era) ->
  SignedDSIGN (Crypto era) (Hash (Crypto era) EraIndependentTxBody) ->
  WitVKey kr era
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

-- | Compute an era-independent transaction body hash
eraIndTxBodyHash ::
  forall era.
  (Era era) =>
  TxBody era ->
  Hash (Crypto era) EraIndependentTxBody
eraIndTxBodyHash = coerce . hashAnnotated

{-# COMPLETE WitVKey #-}

witKeyHash ::
  WitVKey kr era ->
  KeyHash 'Witness (Crypto era)
witKeyHash (WitVKey' _ _ kh _) = kh

instance
  forall era kr.
  (Typeable kr, Era era) =>
  Ord (WitVKey kr era)
  where
  compare = comparing wvkKeyHash

newtype StakeCreds era = StakeCreds
  { unStakeCreds :: Map (Credential 'Staking era) SlotNo
  }
  deriving (Eq, Generic)
  deriving (Show) via (Quiet (StakeCreds era))
  deriving newtype (NFData, NoThunks, ToJSON, FromJSON)

deriving newtype instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (StakeCreds era)

deriving newtype instance
  (Era era, ToCBOR (Core.Script era)) =>
  ToCBOR (StakeCreds era)

-- CBOR

instance-- use the weakest predicate

  (Era era, ToCBOR (Core.Script era)) =>
  ToCBOR (DCert era)
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
  (Era era, Typeable (Core.Script era), FromCBOR (Annotator (Core.Script era))) =>
  FromCBOR (DCert era)
  where
  fromCBOR = decodeRecordSum "DCert era" $
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

instance
  (Era era) =>
  ToCBOR (TxIn era)
  where
  toCBOR (TxInCompact txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance
  (Era era) =>
  FromCBOR (TxIn era)
  where
  fromCBOR = do
    decodeRecordNamed "TxIn" (const 2) $ do
      a <- fromCBOR
      b <- fromCBOR
      pure $ TxInCompact a b

instance-- use the weakest constraint necessary

  (Era era, ToCBOR (Core.Value era), ToCBOR (CompactForm (Core.Value era))) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance-- use the weakest constraint necessary

  (Era era, FromCBOR (Core.Value era), FromCBOR (CompactForm (Core.Value era))) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = decodeRecordNamed "TxOut" (const 2) $ do
    cAddr <- fromCBOR
    coin <- fromCBOR
    pure $ TxOutCompact cAddr coin

instance
  (Typeable kr, Era era) =>
  ToCBOR (WitVKey kr era)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . wvkBytes

instance
  (Typeable kr, Era era) =>
  FromCBOR (Annotator (WitVKey kr era))
  where
  fromCBOR =
    annotatorSlice $
      decodeRecordNamed "WitVKey" (const 2) $
        fmap pure $
          mkWitVKey <$> fromCBOR <*> decodeSignedDSIGN
    where
      mkWitVKey k sig = WitVKey' k sig (asWitness $ hashKey k)

instance ToCBOR PoolMetaData where
  toCBOR (PoolMetaData u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetaData where
  fromCBOR = do
    decodeRecordNamed "PoolMetaData" (const 2) $ do
      u <- fromCBOR
      h <- fromCBOR
      pure $ PoolMetaData u h

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
  (Era era) =>
  ToCBORGroup (PoolParams era)
  where
  toCBORGroup poolParams =
    toCBOR (_poolId poolParams)
      <> toCBOR (_poolVrf poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> encodeFoldable (_poolOwners poolParams)
      <> toCBOR (CborSeq (StrictSeq.getSeq (_poolRelays poolParams)))
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
  (Era era) =>
  FromCBORGroup (PoolParams era)
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
