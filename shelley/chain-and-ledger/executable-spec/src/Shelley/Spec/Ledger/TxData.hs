{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.TxData
  where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeMapLen, encodeWord, enforceSize, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..), Word64, catMaybes)
import           Control.Monad (unless)

import           Data.ByteString (ByteString)
import           Data.Foldable (fold)
import           Data.Map.Strict (Map, filterWithKey)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ledger.Core (Relation (..))
import           Shelley.Spec.Ledger.BaseTypes (Text64, UnitInterval, invalidKey)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (AnyKeyHash, GenKeyHash, Hash,
                     KeyHash, pattern KeyHash, Sig, VKey, VerKeyVRF, hashAnyKey)
import           Shelley.Spec.Ledger.MetaData (MetaDataHash)
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Updates (Update, emptyUpdate, updateNull)

import           Shelley.Spec.Ledger.Serialization (CBORGroup (..), CBORMap (..), CborSeq (..),
                     FromCBORGroup (..), ToCBORGroup (..), decodeMapContents)

import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.Value

-- |The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: Credential crypto
  , _delegatee :: KeyHash crypto
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (Delegation crypto)

newtype Url = Url { unUrl :: Text64 }
  deriving (Eq, Generic, Show, ToCBOR, FromCBOR, NoUnexpectedThunks)

data PoolMetaData = PoolMetaData
  { _poolMDUrl  :: Url
  , _poolMDHash :: ByteString
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks PoolMetaData

-- |A stake pool.
data PoolParams crypto =
  PoolParams
    { _poolPubKey  :: KeyHash crypto
    , _poolVrf     :: Hash (HASH crypto) (VerKeyVRF (VRF crypto))
    , _poolPledge  :: Coin
    , _poolCost    :: Coin
    , _poolMargin  :: UnitInterval
    , _poolRAcnt   :: RewardAcnt crypto
    , _poolOwners  :: Set (KeyHash crypto)
    , _poolRelays  :: Seq Url
    , _poolMD      :: Maybe PoolMetaData
    } deriving (Show, Generic, Eq)
      deriving ToCBOR via CBORGroup (PoolParams crypto)
      deriving FromCBOR via CBORGroup (PoolParams crypto)

instance NoUnexpectedThunks (PoolParams crypto)

-- |An account based address for rewards
newtype RewardAcnt crypto = RewardAcnt
  { getRwdCred :: Credential crypto
  } deriving (Show, Eq, NoUnexpectedThunks, Ord, FromCBOR, ToCBOR)

-- | Script hash or key hash for a payment or a staking object.
data Credential crypto =
    ScriptHashObj (ScriptHash crypto)
  | KeyHashObj    (KeyHash crypto)
    deriving (Show, Eq, Generic, Ord)
    deriving ToCBOR via (CBORGroup (Credential crypto))

newtype GenesisCredential crypto = GenesisCredential (GenKeyHash crypto)
  deriving (Show, Generic)

instance Ord (GenesisCredential crypto)
  where compare (GenesisCredential gh) (GenesisCredential gh')  = compare gh gh'

instance Eq (GenesisCredential crypto)
  where (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance NoUnexpectedThunks (Credential crypto)


-- |An address for UTxO.
data Addr crypto
  = AddrBase (Credential crypto) (Credential crypto)
  | AddrEnterprise (Credential crypto)
  | AddrPtr (Credential crypto) Ptr
  | AddrBootstrap (KeyHash crypto)
  deriving (Show, Eq, Ord, Generic)
  deriving (ToCBOR, FromCBOR) via (CBORGroup (Addr crypto))

instance NoUnexpectedThunks (Addr crypto)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr SlotNo Ix Ix
  deriving (Show, Eq, Ord, Generic)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance NoUnexpectedThunks Ptr

newtype Wdrl crypto = Wdrl { unWdrl :: Map (RewardAcnt crypto) Coin }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = toCBOR . CBORMap . unWdrl

instance Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl . unwrapCBORMap <$> fromCBOR

-- | get value from UTxO output
getValue :: forall crypto. (Crypto crypto) => UTxOOut crypto -> Value crypto
getValue (UTxOOut _ v) = compactValueToValue v

-- | get address from UTxO output
getAddress :: UTxOOut crypto -> Addr crypto
getAddress (UTxOOut a _) = a

-- | get value from Tx output
getValueTx :: TxOut crypto -> Value crypto
getValueTx (TxOut _ v) = v

-- | get address from Tx output
getAddressTx :: TxOut crypto -> Addr crypto
getAddressTx (TxOut a _) = a

-- | get coin amount from UTxO output
getCoin :: Crypto crypto => UTxOOut crypto -> Coin
getCoin (UTxOOut _ v) =
  getAdaAmount $ Value $ filterWithKey (\k _ -> k==adaID) v'
  where
    Value v' = compactValueToValue v

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId crypto
  = TxId { _TxId :: Hash (HASH crypto) (TxBody crypto) }
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (TxId crypto)
deriving instance Crypto crypto => FromCBOR (TxId crypto)

-- |The input of a UTxO.
data TxIn crypto
  = TxIn (TxId crypto) Natural -- TODO use our own Natural type
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxIn crypto)

-- |The output of a Tx.
data TxOut crypto
  = TxOut (Addr crypto) (Value crypto)
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxOut crypto)


-- |The output of a UTxO.
data UTxOOut crypto
  = UTxOOut (Addr crypto) (CompactValue crypto)
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (UTxOOut crypto)

data DelegCert crypto =
    -- | A stake key registration certificate.
    RegKey (Credential crypto)
    -- | A stake key deregistration certificate.
  | DeRegKey (Credential crypto)
    -- | A stake delegation certificate.
  | Delegate (Delegation crypto)
  deriving (Show, Generic, Eq)

data PoolCert crypto =
    -- | A stake pool registration certificate.
    RegPool (PoolParams crypto)
    -- | A stake pool retirement certificate.
  | RetirePool (KeyHash crypto) EpochNo
  deriving (Show, Generic, Eq)

-- | Genesis key delegation certificate
newtype GenesisDelegate crypto = GenesisDelegate (GenKeyHash crypto, KeyHash crypto)
  deriving (Show, Generic, Eq)

-- | Move instantaneous rewards certificate
newtype MIRCert crypto = MIRCert (Map (Credential crypto) Coin)
  deriving (Show, Generic, Eq)

instance Crypto crypto => FromCBOR (MIRCert crypto) where
  fromCBOR = MIRCert . unwrapCBORMap <$> fromCBOR

instance Crypto crypto => ToCBOR (MIRCert crypto) where
  toCBOR (MIRCert c) = toCBOR (CBORMap c)

-- | A heavyweight certificate.
data DCert crypto =
    DCertDeleg (DelegCert crypto)
  | DCertPool (PoolCert crypto)
  | DCertGenesis (GenesisDelegate crypto)
  | DCertMir (MIRCert crypto)
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (DelegCert crypto)
instance NoUnexpectedThunks (PoolCert crypto)
instance NoUnexpectedThunks (GenesisDelegate crypto)
instance NoUnexpectedThunks (MIRCert crypto)
instance NoUnexpectedThunks (DCert crypto)

-- |A raw transaction
data TxBody crypto
  = TxBody
      { _inputs   :: !(Set (TxIn crypto))
      , _outputs  :: Seq (TxOut crypto)
      , _certs    :: Seq (DCert crypto)
      , _forge    :: Value crypto
      , _wdrls    :: Wdrl crypto
      , _txfee    :: Coin
      , _ttl      :: SlotNo
      , _txUpdate :: Update crypto
      , _mdHash   :: Maybe (MetaDataHash crypto)
      } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (TxBody crypto)

-- |Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto
  = WitVKey (VKey crypto) !(Sig crypto (TxBody crypto))
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (WitVKey crypto)

witKeyHash
  :: forall crypto. ( Crypto crypto)
  => WitVKey crypto
  -> AnyKeyHash crypto
witKeyHash (WitVKey key _) = hashAnyKey key

instance forall crypto
  . ( Crypto crypto)
  => Ord (WitVKey crypto) where
    compare = comparing witKeyHash

newtype StakeCreds crypto =
  StakeCreds (Map (Credential crypto) SlotNo)
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)

addStakeCreds :: (Credential crypto) -> SlotNo -> (StakeCreds crypto) -> StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

newtype StakePools crypto =
  StakePools { unStakePools :: (Map (KeyHash crypto) SlotNo) }
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)


-- CBOR

instance
  (Crypto crypto)
  => ToCBOR (DCert crypto)
 where
   toCBOR = \case
           -- DCertDeleg
     DCertDeleg (RegKey (KeyHashObj h)) ->
           encodeListLen 2
           <> toCBOR (0 :: Word8)
           <> toCBOR h
     DCertDeleg (RegKey (ScriptHashObj h)) ->
           encodeListLen 2
           <> toCBOR (1 :: Word8)
           <> toCBOR h
     DCertDeleg (DeRegKey (KeyHashObj h)) ->
           encodeListLen 2
           <> toCBOR (2 :: Word8)
           <> toCBOR h
     DCertDeleg (DeRegKey (ScriptHashObj h)) ->
           encodeListLen 2
           <> toCBOR (3 :: Word8)
           <> toCBOR h
     DCertDeleg (Delegate (Delegation (KeyHashObj h) poolkh)) ->
           encodeListLen 3
           <> toCBOR (4 :: Word8)
           <> toCBOR h
           <> toCBOR poolkh
     DCertDeleg (Delegate (Delegation (ScriptHashObj h) poolkh)) ->
           encodeListLen 3
           <> toCBOR (5 :: Word8)
           <> toCBOR h
           <> toCBOR poolkh

           -- DCertPool
     DCertPool (RegPool poolParams) ->
           encodeListLen (1 + listLen poolParams)
           <> toCBOR (6 :: Word8)
           <> toCBORGroup poolParams
     DCertPool (RetirePool vk epoch) ->
           encodeListLen 3
           <> toCBOR (7 :: Word8)
           <> toCBOR vk
           <> toCBOR epoch

           -- DCertGenesis
     DCertGenesis (GenesisDelegate (gk, kh)) ->
           encodeListLen 3
           <> toCBOR (8 :: Word8)
           <> toCBOR gk
           <> toCBOR kh

           -- DCertMIR
     DCertMir mir ->
           encodeListLen 2
           <> toCBOR (9 :: Word8)
           <> toCBOR mir

instance
  (Crypto crypto)
  => FromCBOR (DCert crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "RegKey" 2 n >> (DCertDeleg . RegKey . KeyHashObj) <$> fromCBOR
      1 -> matchSize "RegKey" 2 n >> (DCertDeleg . RegKey . ScriptHashObj) <$> fromCBOR
      2 -> matchSize "DeRegKey" 2 n >> (DCertDeleg . DeRegKey . KeyHashObj) <$> fromCBOR
      3 -> matchSize "DeRegKey" 2 n >> (DCertDeleg . DeRegKey . ScriptHashObj) <$> fromCBOR
      4 -> do
        matchSize "Delegate" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertDeleg $ Delegate (Delegation (KeyHashObj a) b)
      5 -> do
        matchSize "Delegate" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertDeleg $ Delegate (Delegation (ScriptHashObj a) b)
      6 -> do
        group <- fromCBORGroup
        matchSize "RegPool" (fromIntegral $ 1 + listLen group) n
        pure $ DCertPool $ RegPool group
      7 -> do
        matchSize "RetirePool" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertPool $ RetirePool a (EpochNo b)
      8 -> do
        matchSize "GenesisDelegate" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertGenesis $ GenesisDelegate (a, b)
      9 -> matchSize "MIRCert" 2 n >> DCertMir <$> fromCBOR
      k -> invalidKey k

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxIn crypto)
 where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR (fromIntegral index :: Word64)

instance (Crypto crypto) =>
  FromCBOR (TxIn crypto) where
  fromCBOR = do
    enforceSize "TxIn" 2
    a <- fromCBOR
    (b :: Word64) <- fromCBOR
    pure $ TxIn a (fromInteger $ toInteger b)

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (UTxOOut crypto)
 where
  toCBOR (UTxOOut addr value) =
    encodeListLen (listLen addr + 1)
      <> toCBORGroup addr
      <> toCBOR value

instance (Crypto crypto) =>
  FromCBOR (UTxOOut crypto) where
  fromCBOR = do
    n <- decodeListLen
    addr <- fromCBORGroup
    b <- fromCBOR
    matchSize "TxOut" ((fromIntegral . toInteger . listLen) addr + 1) n
    pure $ UTxOOut addr b

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxOut crypto)
 where
  toCBOR (TxOut addr value) =
    encodeListLen (listLen addr + 1)
      <> toCBORGroup addr
      <> toCBOR (valueToCompactValue value)

instance (Crypto crypto) =>
  FromCBOR (TxOut crypto) where
  fromCBOR = do
    n <- decodeListLen
    addr <- fromCBORGroup
    b <- fromCBOR
    matchSize "TxOut" ((fromIntegral . toInteger . listLen) addr + 1) n
    pure $ TxOut addr (compactValueToValue b)

instance
  Crypto crypto
  => ToCBOR (WitVKey crypto)
 where
  toCBOR (WitVKey vk sig) =
    encodeListLen 2
      <> toCBOR vk
      <> toCBOR sig

instance
  Crypto crypto
  => FromCBOR (WitVKey crypto)
 where
  fromCBOR = do
    enforceSize "WitVKey" 2
    a <- fromCBOR
    b <- fromCBOR
    pure $ WitVKey a b

instance
  (Crypto crypto)
  => ToCBOR (TxBody crypto)
 where
  toCBOR txbody =
    let l = catMaybes
          [ encodeMapElement 0 $ _inputs txbody
          , encodeMapElement 1 $ CborSeq $ _outputs txbody
          , encodeMapElement 2 $ _txfee txbody
          , encodeMapElement 3 $ _ttl txbody
          , encodeMapElementUnless null 4 $ CborSeq $ _certs txbody
          , encodeMapElementUnless (null . val) 5 $ _forge txbody
          , encodeMapElementUnless (null . unWdrl) 6 $ _wdrls txbody
          , encodeMapElementUnless (updateNull) 7 $ _txUpdate txbody
          , encodeMapElement 8 =<< _mdHash txbody
          ]
        n = fromIntegral $ length l
    in encodeMapLen n <> fold l
    where
      encodeMapElement ix x = Just (encodeWord ix <> toCBOR x)
      encodeMapElementUnless condition ix x =
        if condition x
          then Nothing
          else encodeMapElement ix x

instance
  (Crypto crypto)
  => FromCBOR (TxBody crypto)
  where
   fromCBOR = do
     mapParts <- decodeMapContents $
       decodeWord >>= \case
         0 -> fromCBOR                     >>= \x -> pure (0, \t -> t { _inputs   = x })
         1 -> (unwrapCborSeq <$> fromCBOR) >>= \x -> pure (1, \t -> t { _outputs  = x })
         2 -> fromCBOR                     >>= \x -> pure (2, \t -> t { _txfee    = x })
         3 -> fromCBOR                     >>= \x -> pure (3, \t -> t { _ttl      = x })
         4 -> (unwrapCborSeq <$> fromCBOR) >>= \x -> pure (4, \t -> t { _certs    = x })
         5 -> fromCBOR                      >>= \x -> pure (3, \t -> t { _forge    = x })
         6 -> fromCBOR                     >>= \x -> pure (5, \t -> t { _wdrls    = x })
         7 -> fromCBOR                     >>= \x -> pure (6, \t -> t { _txUpdate = x })
         8 -> fromCBOR                     >>= \x -> pure (7, \t -> t { _mdHash   = Just x })
         k -> invalidKey k
     let requiredFields :: Map Int String
         requiredFields = Map.fromList $
           [ (0, "inputs")
           , (1, "outputs")
           , (2, "fee")
           , (3, "ttl")
           ]
         fields = fst <$> mapParts
         missingFields = Map.filterWithKey (\k _ -> notElem k fields) requiredFields
     unless (null missingFields)
       (fail $ "missing required transaction component(s): " <> show missingFields)
     pure $ foldr ($) basebody (snd <$> mapParts)
     where
       basebody = TxBody
          { _inputs   = Set.empty
          , _outputs  = Seq.empty
          , _txfee    = Coin 0
          , _ttl      = SlotNo 0
          , _certs    = Seq.empty
          , _forge    = Value Map.empty
          , _wdrls    = Wdrl Map.empty
          , _txUpdate = emptyUpdate
          , _mdHash   = Nothing
          }


instance (Typeable crypto, Crypto crypto)
  => ToCBORGroup (Credential crypto) where
  listLen _ = 2
  toCBORGroup = \case
    KeyHashObj     kh -> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj  hs -> toCBOR (1 :: Word8) <> toCBOR hs

instance (Typeable crypto, Crypto crypto)
  => ToCBOR (GenesisCredential crypto)
  where toCBOR (GenesisCredential kh) =
          toCBOR kh

instance (Crypto crypto) =>
  FromCBOR (Credential crypto) where
  fromCBOR = do
    enforceSize "Credential" 2
    decodeWord >>= \case
      0 -> KeyHashObj <$> fromCBOR
      1 -> ScriptHashObj <$> fromCBOR
      k -> invalidKey k

instance
  (Typeable crypto, Crypto crypto)
  => ToCBORGroup (Addr crypto)
 where
  listLen (AddrBase _ _) = 3
  listLen (AddrPtr _ pointer) = 2 + listLen pointer
  listLen (AddrEnterprise _) = 2
  listLen (AddrBootstrap  _) = 2

  toCBORGroup (AddrBase (KeyHashObj a) (KeyHashObj b)) =
    toCBOR (0 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (AddrBase (KeyHashObj a) (ScriptHashObj b)) =
    toCBOR (1 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (AddrBase (ScriptHashObj a) (KeyHashObj b)) =
    toCBOR (2 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (AddrBase (ScriptHashObj a) (ScriptHashObj b)) =
    toCBOR (3 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (AddrPtr (KeyHashObj a) pointer) =
    toCBOR (4 :: Word8) <> toCBOR a <> toCBORGroup pointer
  toCBORGroup (AddrPtr (ScriptHashObj a) pointer) =
    toCBOR (5 :: Word8) <> toCBOR a <> toCBORGroup pointer
  toCBORGroup (AddrEnterprise (KeyHashObj a)) =
    toCBOR (6 :: Word8) <> toCBOR a
  toCBORGroup (AddrEnterprise (ScriptHashObj a)) =
    toCBOR (7 :: Word8) <> toCBOR a
  toCBORGroup (AddrBootstrap a) =
    toCBOR (8 :: Word8) <> toCBOR a

instance (Crypto crypto) =>
  FromCBORGroup (Addr crypto) where
  fromCBORGroup = do
    decodeWord >>= \case
      0 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ AddrBase (KeyHashObj a) (KeyHashObj b)
      1 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ AddrBase (KeyHashObj a) (ScriptHashObj b)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ AddrBase (ScriptHashObj a) (KeyHashObj b)
      3 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ AddrBase (ScriptHashObj a) (ScriptHashObj b)
      4 -> do
        a <- fromCBOR
        x <- fromCBOR
        y <- fromCBOR
        z <- fromCBOR
        pure $ AddrPtr (KeyHashObj a) (Ptr x y z)
      5 -> do
        a <- fromCBOR
        x <- fromCBOR
        y <- fromCBOR
        z <- fromCBOR
        pure $ AddrPtr (ScriptHashObj a) (Ptr x y z)
      6 -> do
        a <- fromCBOR
        pure $ AddrEnterprise (KeyHashObj a)
      7 -> do
        a <- fromCBOR
        pure $ AddrEnterprise (ScriptHashObj a)
      8 -> do
        a <- fromCBOR
        pure $ AddrBootstrap (KeyHash a)
      k -> invalidKey k

instance ToCBORGroup Ptr where
  toCBORGroup (Ptr sl txIx certIx) =
         toCBOR sl
      <> toCBOR (fromInteger (toInteger txIx) :: Word)
      <> toCBOR (fromInteger (toInteger certIx) :: Word)
  listLen _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = Ptr <$> fromCBOR <*> fromCBOR <*> fromCBOR

instance ToCBOR PoolMetaData
 where
  toCBOR (PoolMetaData u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetaData
  where
  fromCBOR = do
    enforceSize "PoolMetaData" 2
    u <- fromCBOR
    h <- fromCBOR
    pure $ PoolMetaData u h

instance
  (Crypto crypto)
  => ToCBORGroup (PoolParams crypto)
 where
  toCBORGroup poolParams =
         toCBOR (_poolPubKey poolParams)
      <> toCBOR (_poolVrf poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> toCBOR (_poolOwners poolParams)
      <> toCBOR (CborSeq (_poolRelays poolParams))
      <> toCBOR (_poolMD poolParams)
  listLen _ = 9

instance
  (Crypto crypto)
  => FromCBORGroup (PoolParams crypto)
 where
  fromCBORGroup = do
    vk <- fromCBOR
    vrf <- fromCBOR
    pledge <- fromCBOR
    cost <- fromCBOR
    margin <- fromCBOR
    ra <- fromCBOR
    owners <- fromCBOR
    relays <- fromCBOR
    md <- fromCBOR
    pure $ PoolParams
            { _poolPubKey = vk
            , _poolVrf    = vrf
            , _poolPledge = pledge
            , _poolCost   = cost
            , _poolMargin = margin
            , _poolRAcnt  = ra
            , _poolOwners = owners
            , _poolRelays = (unwrapCborSeq relays)
            , _poolMD     = md
            }

instance Relation (StakeCreds crypto) where
  type Domain (StakeCreds crypto) = Credential crypto
  type Range (StakeCreds crypto)  = SlotNo

  singleton k v = StakeCreds $ Map.singleton k v

  dom (StakeCreds stkCreds) = dom stkCreds

  range (StakeCreds stkCreds) = range stkCreds

  s ◁ (StakeCreds stkCreds) = StakeCreds $ s ◁ stkCreds

  s ⋪ (StakeCreds stkCreds) = StakeCreds $ s ⋪ stkCreds

  (StakeCreds stkCreds) ▷ s = StakeCreds $ stkCreds ▷ s

  (StakeCreds stkCreds) ⋫ s = StakeCreds $ stkCreds ⋫ s

  (StakeCreds a) ∪ (StakeCreds b) = StakeCreds $ a ∪ b

  (StakeCreds a) ⨃ b = StakeCreds $ a ⨃ b

  vmax <=◁ (StakeCreds stkCreds) = StakeCreds $ vmax <=◁ stkCreds

  (StakeCreds stkCreds) ▷<= vmax = StakeCreds $ stkCreds ▷<= vmax

  (StakeCreds stkCreds) ▷>= vmin = StakeCreds $ stkCreds ▷>= vmin

  size (StakeCreds stkCreds) = size stkCreds
