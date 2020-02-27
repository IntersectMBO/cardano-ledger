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

module TxData
  where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeMapLen, encodeWord, enforceSize, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..), Word64, catMaybes)
import           Control.Monad (unless)
import           Lens.Micro.TH (makeLenses)

import           Data.Foldable (fold, foldMap)
import qualified Data.Map as DM
import           Data.Map.Strict (Map)
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

import           BaseTypes (UnitInterval, invalidKey)
import           Coin (Coin (..))
import           Keys (AnyKeyHash, pattern AnyKeyHash, GenKeyHash, Hash, KeyHash, pattern KeyHash,
                     Sig, VKey, VKeyGenesis, VerKeyVRF, hashAnyKey, hash)
import           Ledger.Core (Relation (..))

import           Scripts
import           Value
import           CostModel
import           PParams (PlutusPP)

import           PParams (emptyPlutusPP, PlutusPP)




-- |The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: Credential crypto
  , _delegatee :: KeyHash crypto
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (Delegation crypto)

-- |A stake pool.
data PoolParams crypto =
  PoolParams
    { _poolPubKey  :: KeyHash crypto
    , _poolVrf     :: Hash (HASH crypto) (VerKeyVRF (VRF crypto))
    , _poolPledge  :: Value crypto
    , _poolCost    :: Value crypto
    , _poolMargin  :: UnitInterval
    , _poolRAcnt   :: RewardAcnt crypto
    , _poolOwners  :: Set (KeyHash crypto)
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

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
--
data MultiSig crypto =
       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   (AnyKeyHash crypto)

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [MultiSig crypto]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [MultiSig crypto]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [MultiSig crypto]
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (MultiSig crypto)

-- | Magic number representing the tag of the native multi-signature script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeMultiSigTag :: Word8
nativeMultiSigTag = 0

newtype ScriptHash crypto =
  ScriptHash (Hash (HASH crypto) (Script crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

data Script crypto = MultiSigScript (MultiSig crypto)
                    | PLCScript (ScriptPLC crypto)
                  -- constructors for new languages go here
                   -- e.g | PlutusScriptV1 ScriptPLC
  deriving (Show, Eq, Ord, Generic)

deriving instance Crypto crypto => ToCBOR (ScriptHash crypto)
deriving instance Crypto crypto => FromCBOR (ScriptHash crypto)

newtype ScriptHashPLC crypto =
  ScriptHashPLC (Hash (HASH crypto) (ScriptPLC crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks, Generic)

-- deriving instance Crypto crypto => ToCBOR (ScriptHashPLC crypto)

data ScriptHash crypto =
  ScriptHashMSig (Hash (HASH crypto) (MultiSig crypto))
  | ScriptHashPLC (Hash (HASH crypto) (ScriptPLC crypto))
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (ScriptHash crypto)

newtype Wdrl crypto = Wdrl { unWdrl :: Map (RewardAcnt crypto) Coin }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = toCBOR . CBORMap . unWdrl

instance Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl . unwrapCBORMap <$> fromCBOR


-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId crypto
  = TxId { _TxId :: Hash (HASH crypto) (TxBody crypto) }
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (TxId crypto)
deriving instance Crypto crypto => FromCBOR (TxId crypto)

-- data Info crypto
--   = TxBody
--     { _rdmrhash   :: DataHash crypto
--     } deriving (Show, Eq, Generic)

-- |The input of a UTxO.
data TxIn crypto
  = TxIn (TxId crypto) Natural -- TODO use our own Natural type
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxIn crypto)

-- | the current item being passed to the Plutus interpreter
data CurItem crypto =
  CITxInScr (TxIn crypto) | CIWdrl (Wdrl crypto) | CIDeRegKey (DCert crypto)
  deriving (Show, Eq, Generic)

data ScrInData crypto =
    NoDtRdmr { _vData   :: Data crypto }
  | VldDrRdm { _vData   :: Data crypto
             , _dataValue        :: Data crypto
             , _rdmrValue        :: Data crypto
             }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (CurItem crypto)

-- |The input of a UTxO.
data TxInTx crypto
  =  TxInVK   { _txin   :: TxIn crypto
              , _isfee  ::  IsFee
              }
    | TxInScr { _txin   :: TxIn crypto
              , _rdmrhash :: DataHash crypto
              }
  deriving (Show, Eq, Generic, Ord)


instance NoUnexpectedThunks (TxInTx crypto)

-- |The output of a UTxO.
data TxOut crypto
  =   TxOutVK { _addr   :: Addr crypto
              , _value  :: Value crypto
--              , _slot   :: SlotNo
              }
    | TxOutScr { _addr     :: Addr crypto
               , _value    :: Value crypto
               , _datahash :: DataHash crypto
--               , _slot     :: SlotNo
               }
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxOut crypto)

-- getValue :: TxOut crypto -> Value crypto
-- getValue (TxOutVK  a v) = v
-- getValue (TxOutScr a v d) = v

type StakeCredential crypto = Credential crypto

-- | A heavyweight certificate.
data DCert crypto

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

-- | Hash of protocol parameters relevant to Plutus evaluation
newtype HashPP crypto = HashPP (Hash (HASH crypto) PlutusPP)
  deriving (Show, Eq, Generic, NoUnexpectedThunks, Ord)

deriving instance Crypto crypto => ToCBOR (HashPP crypto)

-- |A raw transaction
data TxBody crypto
  = TxBody
      { _inputs   :: !(Set (TxIn crypto))
      , _outputs  :: Seq (TxOut crypto)
      , _certs    :: Seq (DCert crypto)
      , _wdrls    :: Wdrl crypto
      , _txfee    :: Value crypto
      , _ttl      :: SlotNo
      , _txUpdate :: Update crypto
      , _txlst    :: SlotNo
      , _forged   :: Value crypto
      , _txexunits:: ExUnits
      , _hashPP   :: Maybe (Hash (HASH crypto) PlutusPP)
      , _mdHash   :: Maybe (MetaDataHash crypto)
      } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (TxBody crypto)

-- |Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto
  = WitVKey (VKey crypto) !(Sig crypto (TxBody crypto))
  | WitGVKey (VKeyGenesis crypto) !(Sig crypto (TxBody crypto))
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (WitVKey crypto)

witKeyHash
  :: forall crypto. ( Crypto crypto)
  => WitVKey crypto
  -> AnyKeyHash crypto
witKeyHash (WitVKey key _) = hashAnyKey key
witKeyHash (WitGVKey key _) = hashAnyKey key

instance forall crypto
  . ( Crypto crypto)
  => Ord (WitVKey crypto) where
    compare = comparing witKeyHash

newtype Vlds crypto = Vlds (Map (ScriptHash crypto) (Script crypto))
  deriving (Show, Eq, Generic, NoUnexpectedThunks, ToCBOR, FromCBOR)

-- |A fully formed transaction.
data Tx crypto
  = Tx
      { _body           :: !(TxBody crypto)
      , _unsignedData   :: !(UnsignedData crypto)
      } deriving (Show, Eq, Generic)

data UnsignedData crypto
  = UnsignedData
      { _witnessVKeySet :: !(Set (WitVKey crypto))
      , _txvlds         :: Vlds crypto
      , _txdats         :: Map (DataHash crypto) (Data crypto)
      , _txvaltag       :: IsValidating
      } deriving (Show, Eq, Generic)

-- instance NoUnexpectedThunks (UnsignedData crypto)

newtype StakeCreds crypto =
  StakeCreds (Map (Credential crypto) SlotNo)
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)

addStakeCreds :: (Credential crypto) -> SlotNo -> (StakeCreds crypto) -> StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

newtype StakePools crypto =
  StakePools (Map (KeyHash crypto) SlotNo)
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)


-- CBOR

-- | TODO make this a proper function
-- toValue :: Word8 -> Value crypto
-- toValue _ = Value DM.empty

instance
  (Crypto crypto)
  => ToCBOR (ScrInData crypto)
 where
  toCBOR = \case
    NoDtRdmr vd ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR vd

    VldDrRdm vd dv rd ->
      encodeListLen 4
        <> toCBOR (1 :: Word8)
        <> toCBOR vd
        <> toCBOR dv
        <> toCBOR rd

instance ToCBOR ExUnits
 where
   toCBOR = \case
     PLCUnits exu ->
       encodeListLen 1
        <> toCBOR exu

     MSIGUnits exu ->
       encodeListLen 1
        <> toCBOR exu

instance FromCBOR ExUnits
 where
   fromCBOR = do
     n <- decodeListLen
     decodeWord >>= \case
       0 -> do
         matchSize "PLCUnits" 1 n
         a <- fromCBOR
         pure $ PLCUnits a
       1 -> do
         matchSize "MSIGUnits" 1 n
         a <- fromCBOR
         pure $ MSIGUnits a
       k -> invalidKey k

instance
  (Crypto crypto)
  => ToCBOR (CurItem crypto)
 where
  toCBOR = \case
    CITxInScr txin ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR txin

    CIWdrl wdrl ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR wdrl

    CIDeRegKey dcr ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR dcr

instance
  (Crypto crypto)
  => ToCBOR (TxOut crypto)
 where
  toCBOR = \case
    TxOutVK a v ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR a
        <> toCBOR v


    TxOutScr a v dh ->
      encodeListLen 4
        <> toCBOR (1 :: Word8)
        <> toCBOR a
        <> toCBOR v
        <> toCBOR dh

instance (Crypto crypto) =>
  FromCBOR (ScriptHash crypto) where
  fromCBOR = enforceSize "ScriptHash" 2  >> decodeWord >>= \case
    0 -> do
      a <- fromCBOR
      pure $ ScriptHashMSig a
    1 -> do
      a <- fromCBOR
      pure $ ScriptHashPLC a
    k -> invalidKey k


instance
  (Crypto crypto)
  => ToCBOR (ScriptHash crypto)
 where
  toCBOR = \case
    ScriptHashMSig hs ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR hs

    ScriptHashPLC hs ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR hs

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

-- instance
--   ToCBOR IsThing
--  where
--   toCBOR = \case
--     Yes ->
--       encodeListLen 1
--        <> toCBOR True
--
--     Nope ->
--       encodeListLen 1
--        <> toCBOR False

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxInTx crypto)
 where
  toCBOR (TxOut addr coin) =
    encodeListLen (listLen addr + 1)
      <> toCBORGroup addr
      <> toCBOR coin

  toCBOR = \case
    TxInVK txin isfee ->
      encodeListLen 2
       <> toCBOR txin
       <> toCBOR isfee

    TxInScr txin dh ->
      encodeListLen 2
       <> toCBOR txin
       <> toCBOR dh

instance (Crypto crypto) =>
  FromCBOR (TxInTx crypto) where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "TxInVK" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ TxInVK a b
      1 -> do
        matchSize "TxInScr" 3 n
        a <- fromCBOR
        (b ) <- fromCBOR
        pure $ TxInScr a (DataHash b)
      k -> invalidKey k


instance (Crypto crypto) =>
  FromCBOR (TxOut crypto) where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "TxOutVK" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ TxOutVK a b
      1 -> do
        matchSize "TxOutVK" 4 n
        a <- fromCBOR
        (b ) <- fromCBOR
        (c ) <- fromCBOR
        pure $ TxOutScr a b (DataHash c)
      k -> invalidKey k

instance
  Crypto crypto
  => ToCBOR (WitVKey crypto)
 where
  toCBOR (WitVKey vk sig) =
    encodeListLen 3
      <> toCBOR (0 :: Word8)
      <> toCBOR vk
      <> toCBOR sig
  toCBOR (WitGVKey vk sig) =
    encodeListLen 3
      <> toCBOR (1 :: Word8)
      <> toCBOR vk
      <> toCBOR sig

instance
  Crypto crypto
  => FromCBOR (WitVKey crypto)
 where
  fromCBOR = enforceSize "WitVKey" 3  >> decodeWord >>= \case
    0 -> do
      a <- fromCBOR
      b <- fromCBOR
      pure $ WitVKey a b
    1 -> do
      a <- fromCBOR
      b <- fromCBOR
      pure $ WitGVKey a b
    k -> invalidKey k


instance
  (Crypto crypto)
  => ToCBOR (Script crypto)
 where
  toCBOR = \case
    MSig ms ->
      encodeListLen 1
       <> toCBOR ms

    SPLC plc ->
      encodeListLen 1
       <> toCBOR plc

instance
  Crypto crypto
  => FromCBOR (Script crypto)
 where
  fromCBOR = enforceSize "Script" 2  >> decodeWord >>= \case
    0 -> do
      a <- fromCBOR
      pure $ MSig a
    1 -> do
      a <- fromCBOR
      pure $ SPLC a
    k -> invalidKey k

instance
  (Crypto crypto)
  => ToCBOR (Tx crypto)
 where
  toCBOR tx =
    encodeListLen 3
      <> toCBOR (_body tx)
      <> toCBOR (_unsignedData tx)

instance
  (Crypto crypto)
  => ToCBOR (UnsignedData crypto)
 where
  toCBOR ud =
    encodeListLen 5
      <> toCBOR (_witnessVKeySet ud)
      <> toCBOR (_txvlds ud)
      <> toCBOR (_txdats ud)
      <> toCBOR (_txvaltag ud)

instance (Crypto crypto) =>
  FromCBOR (UnsignedData crypto) where
  fromCBOR = do
    enforceSize "UnsignedData" 4
    a <- fromCBOR
    b <- fromCBOR
    c <- fromCBOR
    d <- fromCBOR
    pure $ UnsignedData a b c d

instance Crypto crypto => FromCBOR (Tx crypto) where
  fromCBOR = decodeListLenOf 3 >>
    Tx <$> fromCBOR <*> fromCBOR <*> fromCBOR

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
          , encodeMapElementUnless (null . unWdrl) 5 $ _wdrls txbody
          , encodeMapElementUnless (updateNull) 6 $ _txUpdate txbody
          , encodeMapElement 7 =<< _mdHash txbody
          ]

    -- let l = toList $
    --           single (encodeWord 0 <> toCBOR (_inputs txbody))
    --         . single (encodeWord 1 <> toCBOR (_outputs txbody))
    --         . single (encodeWord 2 <> toCBOR (_txfee txbody))
    --         . single (encodeWord 3 <> toCBOR (_ttl txbody))
    --         . (if null cs then none else single (encodeWord 4 <> toCBOR (CborSeq cs)))
    --         . (if null ws then none else single (encodeWord 5 <> toCBOR ws))
    --         . (if updateNull us then none else single (encodeWord 6 <> toCBOR us))
    --     toList xs = xs []
    --     single x = (x:)
    --     none = id
        n = fromIntegral $ length l
    in encodeMapLen n <> fold l
    where
  encodeMapElement ix x = Just (encodeWord ix <> toCBOR x)
  encodeMapElementUnless condition ix x =
    if condition x
      then Nothing
      else encodeMapElement ix x


      cs = _certs txbody
      ws = _wdrls txbody
      us = _txUpdate txbody
      hp = _hashPP txbody

mapHelper :: Decoder s b -> Decoder s [b]
mapHelper decodePart = decodeMapLenOrIndef >>= \case
  Just len -> replicateM len decodePart
  Nothing  -> loop [] (not <$> decodeBreakOr) decodePart
  where
  loop acc condition action = condition >>= \case
    False -> pure acc
    True -> action >>= \v -> loop (v:acc) condition action


instance
  (Crypto crypto)
  => FromCBOR (TxBody crypto)
  where
   fromCBOR = do
     mapParts <- mapHelper $
       decodeWord >>= \case
         0 -> fromCBOR                     >>= \x -> pure (0, \t -> t { _txinputs   = x })
         1 -> (unwrapCborSeq <$> fromCBOR) >>= \x -> pure (1, \t -> t { _outputs  = x })
         2 -> fromCBOR                     >>= \x -> pure (2, \t -> t { _txfee      = x })
         3 -> fromCBOR                     >>= \x -> pure (3, \t -> t { _ttl        = x })
         4 -> (unwrapCborSeq <$> fromCBOR) >>= \x -> pure (4, \t -> t { _certs      = x })
         5 -> fromCBOR                     >>= \x -> pure (5, \t -> t { _wdrls      = x })
         6 -> fromCBOR                     >>= \x -> pure (6, \t -> t { _txUpdate   = x })
         7 -> fromCBOR                     >>= \x -> pure (7, \t -> t { _txlst      = x })
         8 -> fromCBOR                     >>= \x -> pure (8, \t -> t { _forged     = x })
         9 -> fromCBOR                     >>= \x -> pure (9, \t -> t { _txexunits  = x })
         10-> fromCBOR                     >>= \x -> pure (10, \t -> t { _hashPP     = x })
         11 -> fromCBOR                    >>= \x -> pure (11, \t -> t { _mdHash   = Just x })
         k -> invalidKey k
     let requiredFields :: Map Int String
         requiredFields = Map.fromList $
           [ (0, "inputs")
           , (1, "outputs")
           , (2, "fee")
           , (3, "ttl")
           , (4, "lst")
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
          , _wdrls    = Wdrl Map.empty
          , _txUpdate = emptyUpdate
          , _txlst     = SlotNo 0
          , _forged    = Value DM.empty
          , _txexunits = PLCUnits (ExUnitsPLC 0 0)
          , _hashPP    = Just $ hash emptyPlutusPP
          , _mdHash   = Nothing
          }

instance (Crypto crypto) =>

-- encodeListLen 6
--   <> toCBOR (_txinputs txbody)
--   <> toCBOR (_outputs txbody)
--   <> toCBOR (toList $ _certs txbody)
--   <> toCBOR (_wdrls txbody)
--   <> toCBOR (_txfee txbody)
--   <> toCBOR (_ttl txbody)
--   <> toCBOR (_txUpdate txbody)
  ToCBOR (MultiSig crypto) where
  toCBOR (RequireSignature hk) =
    encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (RequireAllOf msigs) =
    encodeListLen 2 <> encodeWord 1 <> toCBOR msigs
  toCBOR (RequireAnyOf msigs) =
    encodeListLen 2 <> encodeWord 2 <> toCBOR msigs
  toCBOR (RequireMOf m msigs) =
    encodeListLen 3 <> encodeWord 3 <> toCBOR m <> toCBOR msigs

instance (Crypto crypto) =>
  FromCBOR (MultiSig crypto) where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "RequireSignature" 2 n >> (RequireSignature . AnyKeyHash) <$> fromCBOR
      1 -> matchSize "RequireAllOf" 2 n >> RequireAllOf <$> fromCBOR
      2 -> matchSize "RequireAnyOf" 2 n >> RequireAnyOf <$> fromCBOR
      3 -> do
        matchSize "RequireMOf" 3 n
        m     <- fromCBOR
        msigs <- fromCBOR
        pure $ RequireMOf m msigs
      k -> invalidKey k

instance (Crypto crypto) =>
  ToCBOR (Script crypto) where
  toCBOR (MultiSigScript msig) =
    toCBOR nativeMultiSigTag <> toCBOR msig

instance (Crypto crypto) =>
  FromCBOR (Script crypto) where
  fromCBOR = do
    decodeWord >>= \case
      0 -> MultiSigScript <$> fromCBOR
      k -> invalidKey k

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
  listLen _ = 7

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
    pure $ PoolParams
            { _poolPubKey = vk
            , _poolVrf    = vrf
            , _poolPledge = pledge
            , _poolCost   = cost
            , _poolMargin = margin
            , _poolRAcnt  = ra
            , _poolOwners = owners
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

-- Lenses

makeLenses ''TxInTx

makeLenses ''CurItem

makeLenses ''TxOut

makeLenses ''TxBody

makeLenses ''Tx

makeLenses ''UnsignedData

makeLenses ''ScrInData

makeLenses ''Delegation

makeLenses ''PoolParams
