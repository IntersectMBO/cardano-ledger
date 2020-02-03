{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('Tx')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

module LedgerState
  ( LedgerState(..)
  , Ix
  , DPState(..)
  , DState(..)
  , AccountState(..)
  , RewardUpdate(..)
  , RewardAccounts
  , emptyRewardUpdate
  , EpochState(..)
  , emptyEpochState
  , getIR
  , emptyLedgerState
  , emptyUTxOState
  , clearPpup
  , dstate
  , pstate
  , ptrs
  , fGenDelegs
  , genDelegs
  , irwd
  , PState(..)
  , KeyPairs
  , UTxOState(..)
  , StakeShare(..)
  , emptyAccount
  , emptyPState
  , emptyDState
  , poolRAcnt
  -- * state transitions
  , emptyDelegation
  , applyTxBody
  -- * Genesis State
  , genesisId
  , genesisCoins
  , genesisState
  -- * Validation
  , LedgerValidation(..)
  , minfee
  , txsize
  , validTx
  , produced
  , consumed
  , verifiedWits
  , witsVKeyNeeded
  -- lenses
  , utxoState
  , delegationState
  -- UTxOState
  , utxo
  , deposited
  , fees
  , ups
  -- DelegationState
  , rewards
  , stkCreds
  , delegations
  , stPools
  , pParams
  , retiring
  -- refunds
  , keyRefunds
  , keyRefund
  , decayedTx
  -- epoch boundary
  , memberRew
  , stakeDistr
  , applyRUpd
  , createRUpd
  --
  , NewEpochState(..)
  , NewEpochEnv(..)
  , overlaySchedule
  , getGKeys
  , updateNES
  ) where

import           Address (mkRwdAcnt)
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import           Cardano.Crypto.DSIGN (abstractSizeSig, abstractSizeVKey)
import           Cardano.Crypto.Hash (byteCount)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (ReaderT (..), asks)
import           Data.Foldable (toList)
import           Data.Map.Strict (Map, empty)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Ratio ((%))
import qualified Data.Sequence as Seq (Seq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Delegation.Certificates (delegCWitness, genesisCWitness, poolCWitness,
                     requiresVKeyWitness)
import           EpochBoundary (BlocksMade (..), SnapShots (..), Stake (..), aggregateOuts,
                     baseStake, emptySnapShots, maxPool, poolStake, ptrStake, rewardStake)
import           GHC.Generics (Generic)
import           Keys (AnyKeyHash, GenDelegs (..), GenKeyHash, KeyDiscriminator (..), KeyHash,
                     KeyPair, Signable, hash, undiscriminateKeyHash)
import           Lens.Micro (to, (%~), (&), (.~), (^.))
import           Lens.Micro.TH (makeLenses)

import           Cardano.Ledger.Shelley.Crypto
import           Numeric.Natural (Natural)
import           PParams (PParams (..), activeSlotCoeff, d, emptyPParams, keyDecayRate, keyDeposit,
                     keyMinRefund, minfeeA, minfeeB)
import           Slot (Duration (..), EpochNo (..), SlotNo (..), epochInfoEpoch, epochInfoFirst,
                     epochInfoSize, (+*), (-*))
<<<<<<< HEAD
import           Tx (extractGenKeyHash, extractKeyHash)
import           TxData (Addr (..), Credential (..), DelegCert (..), Ix, MIRCert (..),
                     PoolCert (..), PoolParams (..), Ptr (..), RewardAcnt (..), Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxOut (..), body, certs, countMSigNodes,
                     getRwdCred, inputs, poolOwners, poolPledge, poolRAcnt, ttl, txfee, wdrls,
                     witKeyHash)
import           Updates (AVUpdate (..), Mdt (..), PPUpdate (..), Update (..), UpdateState (..),
                     apps, emptyUpdate, emptyUpdateState)
=======
  -- import           Tx (extractGenKeyHash, extractKeyHash)
  -- import           TxData (Addr (..), Credential (..), DelegCert (..), Ix, MIRCert (..),
  --                      PoolCert (..), PoolParams (..), Ptr (..), RewardAcnt (..), Tx (..),
  --                      TxBody (..), TxId (..), TxIn (..), TxOut (..), body, certs, getRwdCred,
  --                      inputs, poolOwners, poolPledge, poolRAcnt, ttl, txfee, wdrls, witKeyHash)
  -- import           Updates (AVUpdate (..), Mdt (..), PPUpdate (..), Update (..), UpdateState (..),
  --                      apps, emptyUpdate, emptyUpdateState)
import           Tx (extractGenKeyHash, extractKeyHash, makeAdaValue, getrefs, txinputs_vf)
import           TxData (Addr (..), Credential (..), DelegCert (..), Ix, MIRCert (..), PoolCert (..), PoolParams,
                     Ptr (..), RewardAcnt (..), Tx (..), TxBody (..), TxId (..), TxIn (..),
                     TxOut (..), UnsignedData(..), body, certs, getRwdCred, txinputs, poolOwners, poolPledge,
                     poolRAcnt, ttl, txfee, wdrls, witKeyHash, txexunits, forged)
import           Updates (AVUpdate (..), Mdt (..), PPUpdate (..), Update (..), UpdateState (..), emptyUpdate,
                     emptyUpdateState)
>>>>>>> more rebase
import           UTxO (UTxO (..), balance, totalDeposits, txinLookup, txins, txouts, txup,
                     verifyWitVKey, adaAllUTxO)
import           Validation

import           Delegation.Certificates (DCert (..), PoolDistr (..), StakeCreds (..),
                     StakePools (..), decayKey, refund)
import           Delegation.PoolParams (poolSpec)

import           BaseTypes (Globals (..), ShelleyBase, UnitInterval, intervalValue, mkUnitInterval)
import           Ledger.Core (dom, (∪), (∪+), (⋪), (▷), (◁))

import           CostModel
import           PParams
import           Scripts

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs crypto = [(KeyPair 'Regular crypto, KeyPair 'Regular crypto)]

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation crypto
  = LedgerValidation [ValidationError] (LedgerState crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerValidation crypto)

type RewardAccounts crypto
  = Map (RewardAcnt crypto) (Value crypto)

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

-- | State of staking pool delegations and rewards
data DState crypto = DState
    {  -- |The active stake keys.
      _stkCreds    :: StakeCreds           crypto
      -- |The active reward accounts.
    ,  _rewards    :: RewardAccounts       crypto
      -- |The current delegations.
    , _delegations :: Map (Credential crypto) (KeyHash crypto)
      -- |The pointed to hash keys.
    , _ptrs        :: Map Ptr (Credential crypto)
      -- | future genesis key delegations
    , _fGenDelegs  :: Map (SlotNo, GenKeyHash crypto) (KeyHash crypto)
      -- |Genesis key delegations
    , _genDelegs   :: GenDelegs crypto
      -- | Instantaneous Rewards
    , _irwd        :: Map (Credential crypto) (Value crypto)
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DState crypto)

instance Crypto crypto => ToCBOR (DState crypto)
 where
  toCBOR (DState sc rw dlg p fgs gs ir) =
    encodeListLen 7 <> toCBOR sc <> toCBOR rw <> toCBOR dlg <> toCBOR p
      <> toCBOR fgs <> toCBOR gs <> toCBOR ir

instance Crypto crypto => FromCBOR (DState crypto)
 where
  fromCBOR = do
    enforceSize "DState" 7
    sc <- fromCBOR
    rw <- fromCBOR
    dlg <- fromCBOR
    p <- fromCBOR
    fgs <- fromCBOR
    gs <- fromCBOR
    ir <- fromCBOR
    pure $ DState sc rw dlg p fgs gs ir

-- | Current state of staking pools and their certificate counters.
data PState crypto = PState
    { -- |The active stake pools.
      _stPools     :: StakePools crypto
      -- |The pool parameters.
    , _pParams     :: Map (KeyHash crypto) (PoolParams crypto)
      -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map (KeyHash crypto) EpochNo
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PState crypto)

instance Crypto crypto => ToCBOR (PState crypto)
 where
  toCBOR (PState a b c) =
    encodeListLen 3 <> toCBOR a <> toCBOR b <> toCBOR c

instance Crypto crypto => FromCBOR (PState crypto)
 where
  fromCBOR = do
    enforceSize "PState" 3
    a <- fromCBOR
    b <- fromCBOR
    c <- fromCBOR
    pure $ PState a b c

-- | The state associated with the current stake delegation.
data DPState crypto =
    DPState
    {
      _dstate :: DState crypto
    , _pstate :: PState crypto
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DPState crypto)

instance Crypto crypto => ToCBOR (DPState crypto)
 where
  toCBOR (DPState ds ps) =
    encodeListLen 2 <> toCBOR ds <> toCBOR ps

instance Crypto crypto => FromCBOR (DPState crypto)
 where
  fromCBOR = do
    enforceSize "DPState" 2
    ds <- fromCBOR
    ps <- fromCBOR
    pure $ DPState ds ps

data RewardUpdate crypto= RewardUpdate
  { deltaT        :: Value crypto
  , deltaR        :: Value crypto
  , rs            :: Map (RewardAcnt crypto) (Value crypto)
  , deltaF        :: Value crypto
  , updateIRwd    :: Map (Credential crypto) (Value crypto)
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (RewardUpdate crypto)

instance Crypto crypto => ToCBOR (RewardUpdate crypto)
 where
  toCBOR (RewardUpdate dt dr rw df irw) =
    encodeListLen 5
      <> toCBOR dt
      <> toCBOR (-dr) -- TODO change Coin serialization to use integers?
      <> toCBOR rw
      <> toCBOR (-df) -- TODO change Coin serialization to use integers?
      <> toCBOR irw

instance Crypto crypto => FromCBOR (RewardUpdate crypto)
 where
  fromCBOR = do
    enforceSize "RewardUpdate" 5
    dt <- fromCBOR
    dr <- fromCBOR -- TODO change Coin serialization to use integers?
    rw <- fromCBOR
    df <- fromCBOR -- TODO change Coin serialization to use integers?
    irw <- fromCBOR
    pure $ RewardUpdate dt (-dr) rw (-df) irw

emptyRewardUpdate :: RewardUpdate crypto
emptyRewardUpdate = RewardUpdate (Value Map.empty) (Value Map.empty)
  Map.empty (Value Map.empty) Map.empty

data AccountState crypto = AccountState
  { _treasury  :: Value crypto
  , _reserves  :: Value crypto
  } deriving (Show, Eq, Generic)

instance
  (Crypto crypto)
  => ToCBOR (AccountState crypto)
 where
  toCBOR (AccountState t r) =
    encodeListLen 2 <> toCBOR t <> toCBOR r

instance
  (Crypto crypto)
  => FromCBOR (AccountState crypto)
 where
  fromCBOR = do
    enforceSize "AccountState" 2
    t <- fromCBOR
    r <- fromCBOR
    pure $ AccountState t r

instance NoUnexpectedThunks (AccountState crypto)

data EpochState crypto
  = EpochState
    { esAccountState :: AccountState crypto
    , esSnapshots :: SnapShots crypto
    , esLState :: LedgerState crypto
    , esPp :: PParams crypto
    }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (EpochState crypto)

instance Crypto crypto => ToCBOR (EpochState crypto)
 where
  toCBOR (EpochState a s l p) =
    encodeListLen 4 <> toCBOR a <> toCBOR s <> toCBOR l <> toCBOR p

instance Crypto crypto => FromCBOR (EpochState crypto)
 where
  fromCBOR = do
    enforceSize "EpochState" 4
    a <- fromCBOR
    s <- fromCBOR
    l <- fromCBOR
    p <- fromCBOR
    pure $ EpochState a s l p

emptyUTxOState :: (Crypto crypto) => UTxOState crypto
emptyUTxOState = UTxOState (UTxO Map.empty) (Value Map.empty) (Value Map.empty) emptyUpdateState

emptyEpochState :: (Crypto crypto) => EpochState crypto
emptyEpochState =
  EpochState emptyAccount emptySnapShots emptyLedgerState  emptyPParams

<<<<<<< HEAD
getIR :: EpochState crypto -> Map (Credential crypto) Coin
getIR = _irwd . _dstate . _delegationState . esLState

emptyLedgerState :: LedgerState crypto
=======
emptyLedgerState :: (Crypto crypto) => LedgerState crypto
>>>>>>> more rebase
emptyLedgerState =
  LedgerState
  emptyUTxOState
  emptyDelegation

emptyAccount :: (Crypto crypto) => AccountState crypto
emptyAccount = AccountState (Value Map.empty) (Value Map.empty)

emptyDelegation :: DPState crypto
emptyDelegation =
    DPState emptyDState emptyPState

emptyDState :: DState crypto
emptyDState =
  DState (StakeCreds Map.empty) Map.empty Map.empty Map.empty Map.empty (GenDelegs Map.empty) Map.empty

emptyPState :: PState crypto
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty

-- |Clear the protocol parameter updates
clearPpup
  :: UTxOState crypto
  -> UTxOState crypto
clearPpup utxoSt =
  let UpdateState _ avup faps aps = _ups utxoSt
  in utxoSt {_ups = UpdateState (PPUpdate Map.empty) avup faps aps}

data UTxOState crypto=
    UTxOState
    { _utxo      :: !(UTxO crypto)
    , _deposited :: Value crypto
    , _fees      :: Value crypto
    , _ups       :: UpdateState crypto
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UTxOState crypto)

instance Crypto crypto => ToCBOR (UTxOState crypto)
 where
  toCBOR (UTxOState ut dp fs us) =
    encodeListLen 4 <> toCBOR ut <> toCBOR dp <> toCBOR fs <> toCBOR us

instance Crypto crypto => FromCBOR (UTxOState crypto)
 where
  fromCBOR = do
    enforceSize "UTxOState" 4
    ut <- fromCBOR
    dp <- fromCBOR
    fs <- fromCBOR
    us <- fromCBOR
    pure $ UTxOState ut dp fs us

-- | New Epoch state and environment
data NewEpochState crypto=
  NewEpochState {
    nesEL     :: EpochNo                       -- ^ Last epoch
  , nesBprev  :: BlocksMade          crypto  -- ^ Blocks made before current epoch
  , nesBcur   :: BlocksMade          crypto  -- ^ Blocks made in current epoch
  , nesEs     :: EpochState          crypto  -- ^ Epoch state before current
  , nesRu     :: Maybe (RewardUpdate crypto) -- ^ Possible reward update
  , nesPd     :: PoolDistr           crypto  -- ^ Stake distribution within the stake pool
  , nesOsched :: Map  SlotNo
                     (Maybe
                       (GenKeyHash   crypto))  -- ^ Overlay schedule for PBFT vs Praos
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochState crypto)

instance Crypto crypto => ToCBOR (NewEpochState crypto)
 where
  toCBOR (NewEpochState e bp bc es ru pd os) =
    encodeListLen 7 <> toCBOR e <> toCBOR bp <> toCBOR bc <> toCBOR es
      <> toCBOR ru <> toCBOR pd <> toCBOR os

instance Crypto crypto => FromCBOR (NewEpochState crypto)
 where
  fromCBOR = do
    enforceSize "NewEpochState" 7
    e <- fromCBOR
    bp <- fromCBOR
    bc <- fromCBOR
    es <- fromCBOR
    ru <- fromCBOR
    pd <- fromCBOR
    os <- fromCBOR
    pure $ NewEpochState e bp bc es ru pd os

getGKeys
  :: NewEpochState crypto
  -> Set (GenKeyHash crypto)
getGKeys nes = Map.keysSet genDelegs
  where NewEpochState _ _ _ es _ _ _ = nes
        EpochState _ _ ls _ = es
        LedgerState _ (DPState (DState _ _ _ _ _ (GenDelegs genDelegs) _) _) = ls

data NewEpochEnv crypto=
  NewEpochEnv {
    neeS     :: SlotNo
  , neeGkeys :: Set (GenKeyHash crypto)
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochEnv crypto)

-- |The state associated with a 'Ledger'.
data LedgerState crypto=
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !(UTxOState crypto)
    -- |The current delegation state
  , _delegationState   :: !(DPState crypto)
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerState crypto)

instance Crypto crypto => ToCBOR (LedgerState crypto)
 where
  toCBOR (LedgerState u dp) =
    encodeListLen 2 <> toCBOR u <> toCBOR dp

instance Crypto crypto => FromCBOR (LedgerState crypto)
 where
  fromCBOR = do
    enforceSize "LedgerState" 2
    u <- fromCBOR
    dp <- fromCBOR
    pure $ LedgerState u dp

makeLenses ''DPState
makeLenses ''DState
makeLenses ''PState
makeLenses ''UTxOState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId
  :: (Crypto crypto)
  => TxId crypto
genesisId =
  TxId $ hash
  (TxBody
   Set.empty
   []
   Seq.Empty
   Map.empty
   (Value Map.empty)
   (SlotNo 0)
   emptyUpdate
   (SlotNo 0)
   (Value Map.empty)
   defaultUnits
   Nothing)

-- |Creates the UTxO for a new ledger with the specified transaction outputs.
-- TODO should this be SlotNo 0 in the outputs?
genesisCoins
  :: (Crypto crypto)
  => [TxOut crypto]
  -> UTxO crypto
genesisCoins outs = UTxO $
  Map.fromList [(TxIn genesisId idx, (out, 0)) | (idx, out) <- zip [0..] outs]

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState
  :: Map (GenKeyHash crypto) (KeyHash crypto)
  -> UTxO crypto
  -> LedgerState crypto
genesisState genDelegs0 utxo0 = LedgerState
  (UTxOState
    utxo0
    (Value Map.empty)
    (Value Map.empty)
    emptyUpdateState)
  (DPState dState emptyPState)
  where
    dState = emptyDState {_genDelegs = GenDelegs genDelegs0}

-- | Determine if the transaction has expired
current :: TxBody crypto-> SlotNo -> Validity
current tx slot =
    if tx ^. ttl < slot
    then Invalid [Expired (tx ^. ttl) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: TxBody crypto-> Validity
validNoReplay tx =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs
  :: TxBody crypto
  -> UTxOState crypto
  -> Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (u ^. utxo)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize :: forall crypto . (Crypto crypto) => Tx crypto-> Integer
txsize (Tx (TxBody ins outs cs ws _ _ (Update (PPUpdate ppup) (AVUpdate avup) _)) vKeySigs msigScripts) =
  iSize + oSize + cSize + wSize + feeSize + ttlSize + uSize + witnessSize
  where
    -- vkey signatures
    signatures = Set.size vKeySigs

    -- multi-signature scripts
    scriptNodes = Map.foldl (+) 0 (Map.map countMSigNodes msigScripts)

    -- The abstract size of the witnesses is caclucated as the sum of the sizes
    -- of the vkey witnesses (vkey + signature size) and the size of the
    -- scripts. For each script, the abstract size is calculated as the number
    -- of nodes and leaves in the multi-signature script times the size of a
    -- `hashObj`.
    witnessSize =
      (fromIntegral signatures) * ((toInteger . abstractSizeVKey) (Proxy :: Proxy (DSIGN crypto)) +
                                    (toInteger . abstractSizeSig) (Proxy :: Proxy (DSIGN crypto))) +
      hashObj * (fromIntegral scriptNodes) +
      smallArray + labelSize + mapPrefix + (hashObj * fromIntegral (Map.size msigScripts))

    -- hash
    hl = toInteger $ byteCount (Proxy :: Proxy (HASH crypto))
    hashObj = 2 + hl
    uint = 5
    arrayPrefix = 2
    smallArray = 1
    mapPrefix = 2
    labelSize = 1
    cborTag = 2
    address = 2 * hashObj
    credential = labelSize + hashObj
    unitInterval = cborTag + smallArray + uint + uint
    feeSize = labelSize + uint
    ttlSize = labelSize + uint
    iSize = labelSize + cborTag + arrayPrefix
            + (toInteger $ length ins) * (smallArray + uint + hashObj)
    oSize = labelSize + arrayPrefix
            + (toInteger $ length outs) * (smallArray + uint + address)

    numPoolOwners = toInteger . length . _poolOwners
    pparams pps = cborTag + arrayPrefix + (numPoolOwners pps)*(hashObj) -- pool owners
                + uint -- cost
                + unitInterval -- margin
                + uint -- pledge
                + hashObj -- operator
                + hashObj -- vrf keyhash
                + credential -- reward account
    certSize (DCertDeleg (RegKey _)) = smallArray + labelSize + hashObj
    certSize (DCertDeleg (DeRegKey _)) = smallArray + labelSize + hashObj
    certSize (DCertDeleg (Delegate _ )) = smallArray + labelSize + 2 * hashObj
    certSize (DCertPool (RegPool pps)) = smallArray + labelSize + hashObj + pparams pps
    certSize (DCertPool (RetirePool _ _)) = smallArray + labelSize + uint + hashObj
    certSize (DCertGenesis _) = smallArray + labelSize + 2 * hashObj
    certSize (DCertMir (MIRCert m)) = smallArray + labelSize + mapPrefix
                                    + (toInteger $ length m)*(uint + hashObj)
    cSize = sum $ fmap certSize cs
    wSize = labelSize + mapPrefix + (toInteger $ length ws) * (uint + credential)

    protoVersion = (smallArray + uint + uint + uint)
    params = mapPrefix
               + labelSize + uint         -- minfee A
               + labelSize + uint         -- minfee B
               + labelSize + uint         -- max block body size
               + labelSize + uint         -- max transaction size
               + labelSize + uint         -- max block header size
               + labelSize + uint         -- key deposit
               + labelSize + unitInterval -- key deposit min refund
               + labelSize + unitInterval -- key deposit decay rate
               + labelSize + uint         -- pool deposit
               + labelSize + unitInterval -- pool deposit min refund
               + labelSize + unitInterval -- pool deposit decay rate
               + labelSize + uint         -- maximum epoch
               + labelSize + uint         -- n_optimal. desired number of stake pools
               + labelSize + unitInterval -- pool pledge influence
               + labelSize + unitInterval -- expansion rate
               + labelSize + unitInterval -- treasury growth rate
               + labelSize + unitInterval -- active slot coefficient
               + labelSize + unitInterval -- d. decentralization constant
               + labelSize + uint         -- extra entropy
               + labelSize + protoVersion -- protocol version
    ppupSize = mapPrefix + (toInteger $ length ppup) * (hashObj + params)
    avupSize = mapPrefix + (sum $ fmap appsSize avup)
    appsSize as = hashObj + mapPrefix + (sum $ fmap mdSize (apps as))
    nameSize = 12
    sysTagSize = 10
    mdSize (_av, (Mdt m)) = nameSize + arrayPrefix + uint + mapPrefix
                             + (toInteger $ length m) * (sysTagSize + hashObj)
    uSize = arrayPrefix + ppupSize + avupSize + smallArray + uint

-- |Minimum fee calculation including script fees
-- | TODO make this correct calculation
minfee :: PParams -> TxBody crypto -> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + fromIntegral (pc ^. minfeeB)
  + scriptFee (pc ^. plutusPP ^. prices) (txexunits tx)


-- |Determine if the fee is large enough
validFee :: forall crypto . (Crypto crypto) => PParams -> Tx crypto-> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. body . txfee

-- |Compute the lovelace which are created by the transaction
produced
  :: (Crypto crypto)
  => SlotNo
  -> PParams crypto
  -> StakePools crypto
  -> TxBody crypto
  -> Value crypto
produced slot pp stakePools tx =
    balance (txouts slot tx) + tx ^. txfee + totalDeposits pp stakePools (toList $ tx ^. certs)
    + scriptFee (pp ^. plutusPP ^. prices) (tx ^. txexunits)

-- |Compute the key deregistration refunds in a transaction
keyRefunds
  :: PParams crypto
  -> StakeCreds crypto
  -> TxBody crypto
  -> Value crypto
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (tx ^. ttl) c | c@(DCertDeleg (DeRegKey _)) <- toList $ tx ^. certs]
  where (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund
  :: Value crypto
  -> UnitInterval
  -> Rational
  -> StakeCreds crypto
  -> SlotNo
  -> DCert crypto
  -> Value crypto
keyRefund dval dmin lambda (StakeCreds stkcreds) slot c =
    case c of
      DCertDeleg (DeRegKey key) -> case Map.lookup key stkcreds of
                                     Nothing -> Value Map.empty
                                     Just  s -> refund dval dmin lambda $ slot -* s
      _ -> Value Map.empty

-- | Functions to calculate decayed deposits
decayedKey
  :: PParams crypto
  -> StakeCreds crypto
  -> SlotNo
  -> DCert crypto
  -> ShelleyBase (Value crypto)
decayedKey pp stk@(StakeCreds stkcreds) cslot cert =
    case cert of
      DCertDeleg (DeRegKey key) ->
          if Map.notMember key stkcreds
          then pure 0
          else do
            let created'      = stkcreds Map.! key
            start <- do
              ei <- asks epochInfo
              fs <- epochInfoFirst ei =<< epochInfoEpoch ei cslot
              pure $ max fs created'
            let dval          = pp ^. keyDeposit
                dmin          = pp ^. keyMinRefund
                lambda        = pp ^. keyDecayRate
                epochRefund   = keyRefund dval dmin lambda stk start cert
                currentRefund = keyRefund dval dmin lambda stk cslot cert
            pure $ epochRefund - currentRefund
      _ -> pure 0

-- | Decayed deposit portions
decayedTx
  :: PParams crypto
  -> StakeCreds crypto
  -> TxBody crypto
  -> ShelleyBase (Value crypto)
decayedTx pp stk tx =
      lsum [decayedKey pp stk (tx ^. ttl) c | c@(DCertDeleg (DeRegKey _)) <- toList $ tx ^. certs]
    where
      lsum xs = ReaderT $ \s -> sum $ fmap (flip runReaderT s) xs

-- |Compute the lovelace which are destroyed by the transaction
consumed
  :: PParams crypto
  -> UTxO crypto
  -> StakeCreds crypto
  -> TxBody crypto
  -> Value crypto
consumed pp u stakeKeys tx =
    balance (txins tx ◁ u) + refunds + withdrawals + tx ^. forged
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance
  :: (Crypto crypto)
  => SlotNo
  -> StakePools crypto
  -> StakeCreds crypto
  -> PParams crypto
  -> TxBody crypto
  -> UTxOState crypto
  -> Validity
preserveBalance slot stakePools stakeKeys pp tx u =
  if destroyed' == created'
    then Valid
    else Invalid [ValueNotConserved qd qc]
  where
    Value destroyed' = consumed pp (u ^. utxo) stakeKeys tx
    Value created' = produced slot pp stakePools tx
    qd = foldr (+) (Quantity 0) $ fmap ((foldr (+) (Quantity 0)) . Map.elems) destroyed'
    qc = foldr (+) (Quantity 0) $ fmap ((foldr (+) (Quantity 0)) . Map.elems) created'

-- |Determine if the reward witdrawals correspond
-- to the rewards in the ledger state
correctWithdrawals
  :: RewardAccounts crypto
  -> RewardAccounts crypto
  -> Validity
correctWithdrawals accs withdrawals =
  if withdrawals `Map.isSubmapOf` accs
    then Valid
    else Invalid [IncorrectRewards]

-- |Collect the set of hashes of keys that needs to sign a
-- given transaction. This set consists of the txin owners,
-- certificate authors, and withdrawal reward accounts.
witsVKeyNeeded
  :: (Crypto crypto) =>
  UTxO crypto
  -> Tx crypto
  -> GenDelegs crypto
  -> Set (AnyKeyHash crypto)
witsVKeyNeeded utxo' tx@(Tx txbody _) _genDelegs =
    inputAuthors `Set.union`
    wdrlAuthors  `Set.union`
    certAuthors  `Set.union`
    updateKeys   `Set.union`
    owners
  where
    inputAuthors = undiscriminateKeyHash `Set.map` Set.foldr insertHK Set.empty (getrefs $ txbody ^. txinputs)
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOutVK (AddrBase (KeyHashObj pay) _) _) -> Set.insert pay hkeys
        _                               -> hkeys

    wdrlAuthors =
      Set.fromList $ extractKeyHash $ map getRwdCred (Map.keys (txbody ^. wdrls))
    owners = foldl Set.union Set.empty
               [pool ^. poolOwners . to (Set.map undiscriminateKeyHash) | DCertPool (RegPool pool) <- toList $ txbody ^. certs]
    certAuthors = Set.fromList $ foldl (++) [] $ fmap getCertHK certificates
    getCertHK = cwitness

    -- key reg requires no witness but this is already filtered out before the
    -- call to `cwitness`
    cwitness (DCertDeleg dc) = extractKeyHash $ [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHash $ [poolCWitness pc]
    cwitness (DCertGenesis gc) = extractGenKeyHash $ [genesisCWitness gc]
    cwitness c = error $ show c ++ " does not have a witness"

    certificates = filter requiresVKeyWitness (toList $ txbody ^. certs)
    updateKeys = undiscriminateKeyHash `Set.map` propWits (txup tx) _genDelegs

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are correct.
verifiedWits
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
  -> Validity
verifiedWits (Tx tx (UnsignedData wits _ _ _)) =
  if all (verifyWitVKey tx) wits
    then Valid
    else Invalid [InvalidWitness]

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- We check that there are not more witnesses than inputs, if several inputs
-- from the same address are used, it is not strictly necessary to include more
-- than one witness.
enoughWits
  :: Crypto crypto
  => Tx crypto
  -> GenDelegs crypto
  -> UTxOState crypto
  -> Validity
enoughWits tx@(Tx _ (UnsignedData wits _ _ _)) d' u =
  if witsVKeyNeeded (u ^. utxo) tx d' `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map witKeyHash wits

validRuleUTXO
  :: (Crypto crypto)
  => RewardAccounts crypto
  -> StakePools crypto
  -> StakeCreds crypto
  -> PParams crypto
  -> SlotNo
  -> Tx crypto
  -> UTxOState crypto
  -> Validity
validRuleUTXO accs stakePools stakeKeys pc slot tx u =
                          validInputs txb u
                       <> current txb slot
                       <> validNoReplay txb
                       <> validFee pc tx
                       <> preserveBalance stakePools stakeKeys pc txb u
                       <> correctWithdrawals accs (txb ^. wdrls)
  where txb = _body tx

validRuleUTXOW
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
  -> GenDelegs crypto
  -> LedgerState crypto
  -> Validity
validRuleUTXOW tx d' l = verifiedWits tx
                   <> enoughWits tx d' (l ^. utxoState)

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits
  :: Update crypto
  -> GenDelegs crypto
  -> Set (KeyHash crypto)
propWits (Update (PPUpdate pup) (AVUpdate aup') _) (GenDelegs _genDelegs) =
  Set.fromList $ Map.elems updateKeys
  where updateKeys = (Map.keysSet pup `Set.union` Map.keysSet aup') ◁ _genDelegs

validTx
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
  -> GenDelegs crypto
  -> SlotNo
  -> PParams crypto
  -> LedgerState crypto
  -> Validity
validTx tx d' slot pp l =
    validRuleUTXO  (l ^. delegationState . dstate . rewards)
                   (l ^. delegationState . pstate . stPools)
                   (l ^. delegationState . dstate . stkCreds)
                   pp
                   slot
                   tx
                   (l ^. utxoState)
 <> validRuleUTXOW tx d' l

-- Functions for stake delegation model

-- |Calculate the change to the deposit pool for a given transaction.
depositPoolChange
  :: LedgerState crypto
  -> PParams crypto
  -> TxBody crypto
  -> Value crypto
depositPoolChange ls pp tx = (currentPool + txDeposits) - txRefunds
  -- Note that while (currentPool + txDeposits) >= txRefunds,
  -- it could be that txDeposits < txRefunds. We keep the parenthesis above
  -- to emphasize this point.
  where
    currentPool = ls ^. utxoState . deposited
    txDeposits =
      totalDeposits pp (ls ^. delegationState . pstate . stPools) (toList $ tx ^. certs)
    txRefunds = keyRefunds pp (ls ^. delegationState . dstate . stkCreds) tx

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody
  :: (Crypto crypto)
  => SlotNo
  -> IsValidating
  -> LedgerState crypto
  -> PParams crypto
  -> TxBody crypto
  -> LedgerState crypto
applyTxBody slot vtag ls pp tx =
    ls & utxoState %~ flip (applyUTxOUpdate slot vtag) tx
       & utxoState . deposited .~ depositPoolChange ls pp tx
       & utxoState . fees .~ (tx ^. txfee) + (ls ^. utxoState . fees)
       & delegationState . dstate . rewards .~ newAccounts
  where
    newAccounts = reapRewards (ls ^. delegationState . dstate. rewards) (tx ^. wdrls)

reapRewards
  :: RewardAccounts crypto
  -> RewardAccounts crypto
  -> RewardAccounts crypto
reapRewards dStateRewards withdrawals =
    Map.mapWithKey removeRewards dStateRewards
    where removeRewards k v = if k `Map.member` withdrawals then Value Map.empty else v

-- | UTxO update when all scripts validate
--
-- | TODO make this correct
applyUTxOUpdate
  :: (Crypto crypto)
  => SlotNo
  -> IsValidating
  -> UTxOState crypto
  -> TxBody crypto
  -> UTxOState crypto
applyUTxOUpdate slot vtag u tx
  | vtag == IsValidating Yes  = u & utxo .~ txins tx ⋪ (u ^. utxo) ∪ txouts slot tx
  | vtag == IsValidating Nope = u & utxo .~ txinputs_vf tx ⋪ (u ^. utxo)

---------------------------------
-- epoch boundary calculations --
---------------------------------

-- | Calculate pool reward
-- TODO rewrite calc for Value
poolRewards
  :: UnitInterval
  -> UnitInterval
  -> Natural
  -> Natural
  -> Value crypto
  -> Value crypto
poolRewards d_ sigma blocksN blocksTotal (Value maxP) = Value Map.empty
  -- if intervalValue d_ < 0.8
  --   then floor (p * fromIntegral maxP)
  --   else 1
  -- where
  --   p = beta / intervalValue sigma
  --   beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Calculate pool leader reward
-- TODO rewrite calc for Value
leaderRew
  :: Value crypto
  -> PoolParams crypto
  -> StakeShare
  -> StakeShare
  -> Value crypto
leaderRew f@(Value f') pool (StakeShare s) (StakeShare sigma)
  = Value Map.empty
  -- | f' <= c = f
  -- | otherwise =
  --    c + floor (fromIntegral (f' - c) * (m' + (1 - m') * s / sigma))
  -- where
  --   (c, m, _) = poolSpec pool
  --   m' = intervalValue m

-- | Calculate pool member reward
-- TODO rewrite calc for Value
memberRew
  :: Value crypto
  -> PoolParams crypto
  -> StakeShare
  -> StakeShare
  -> Value crypto
memberRew (Value f') pool (StakeShare t) (StakeShare sigma)
  = Value Map.empty
  -- | f' <= c = 0
  -- | otherwise =  floor $ fromIntegral (f' - c) * (1 - m') * t / sigma
  -- where
  --   (Value c, m, _) = poolSpec pool
  --   m' = intervalValue m

-- | Reward one pool
-- TODO make this correct
rewardOnePool
  :: PParams crypto
  -> Value crypto
  -> Natural
  -> Natural
  -> Credential crypto
  -> PoolParams crypto
  -> Stake crypto
  -> Value crypto
  -> Set (RewardAcnt crypto)
  -> Map (RewardAcnt crypto) (Value crypto)
rewardOnePool pp r blocksN blocksTotal poolHK pool (Stake stake) (Value total) addrsRew = Map.empty
  -- rewards'
  -- where
  --   pstake = Map.foldl (+) Map.empty stake
  --   ostake = Set.foldl
  --                   (\c o -> c + (stake Map.! KeyHashObj o))
  --                   Map.empty
  --                   (pool ^. poolOwners)
  --   sigma = fromIntegral pstake % fromIntegral total
  --   Value pledge = pool ^. poolPledge
  --   pr = fromIntegral pledge % fromIntegral total
  --   maxP =
  --     if pledge <= ostake
  --       then maxPool pp r sigma pr
  --       else 0
  --   s' = fromMaybe (error "LedgerState.rewardOnePool: Unexpected Nothing") $ mkUnitInterval sigma
  --   poolR = poolRewards (_d pp) s' blocksN blocksTotal maxP
  --   tot = fromIntegral total
  --   mRewards = Map.fromList
  --    [(RewardAcnt hk,
  --      memberRew poolR pool (StakeShare (fromIntegral c% tot)) (StakeShare sigma))
  --    | (hk, Value c) <- Map.toList stake, hk /= poolHK]
  --   iReward  = leaderRew poolR pool (StakeShare $ fromIntegral ostake % tot) (StakeShare sigma)
  --   potentialRewards = Map.insert (pool ^. poolRAcnt) iReward mRewards
  --   rewards' = addrsRew ◁ potentialRewards

-- TODO make this calculation
reward
  :: PParams crypto
  -> BlocksMade crypto
  -> Value crypto
  -> Set (RewardAcnt crypto)
  -> Map (KeyHash crypto) (PoolParams crypto)
  -> Stake crypto
  -> Map (Credential crypto) (KeyHash crypto)
  -> Map (RewardAcnt crypto) (Value crypto)
reward pp (BlocksMade b) r addrsRew poolParams stake@(Stake stake') delegs = Map.empty
  -- rewards'
  -- where
  --   total = Map.foldl (+) (Value 0) stake'
  --   pdata =
  --     [ ( hk
  --       , ( poolParams Map.! hk
  --         , b Map.! hk
  --         , poolStake hk delegs stake))
  --     | hk <-
  --         Set.toList $ Map.keysSet poolParams `Set.intersection` Map.keysSet b
  --     ]
  --   results =
  --     [ ( hk
  --       , rewardOnePool pp r n totalBlocks (KeyHashObj hk) pool actgr total addrsRew)
  --     | (hk, (pool, n, actgr)) <- pdata
  --     ]
  --   rewards' = foldl (\m (_, r') -> Map.union m r') Map.empty results
  --   totalBlocks = Map.foldr (+) 0 b

-- | Stake distribution
stakeDistr
  :: forall crypto
   . Crypto crypto
   => UTxO crypto
  -> DState crypto
  -> PState crypto
  -> ( Stake crypto
     , Map (Credential crypto) (KeyHash crypto)
     )
stakeDistr u ds ps = ( Stake $ dom activeDelegs ◁ aggregatePlus stakeRelation
                     , delegs)
    where
      DState (StakeCreds stkcreds) rewards' delegs ptrs' _ _ _ = ds
      PState (StakePools stpools) _ _                          = ps
      outs = aggregateOuts $ adaAllUTxO u

      stakeRelation :: [(Credential crypto, Value crypto)]
      stakeRelation = baseStake outs ∪ ptrStake outs ptrs' ∪ rewardStake rewards'

      activeDelegs = dom stkcreds ◁ delegs ▷ dom stpools

      aggregatePlus = Map.fromListWith (+)

-- | Apply a reward update
-- TODO fix this
applyRUpd
  :: RewardUpdate crypto
  -> EpochState crypto
  -> EpochState crypto
applyRUpd ru (EpochState as ss ls pp) = EpochState as' ss ls' pp
  where utxoState_ = _utxoState ls
        delegState = _delegationState ls
        dState = _dstate delegState

        as' = as { _treasury = _treasury as + deltaT ru
                 , _reserves = _reserves as + deltaR ru + nonDistributed
                 }
        ls' = ls { _utxoState =
                     utxoState_ { _fees = _fees utxoState_ + deltaF ru }
                 , _delegationState =
                     delegState
                     {_dstate = dState
                                { _rewards = (_rewards dState ∪+ rs ru) ∪+ updateRwd
                                , _irwd = Map.empty
                                }}}
        StakeCreds stkcreds = _stkCreds dState
        (rewMir', unregistered) =
          Map.partitionWithKey (\cred _ -> cred `Map.member` stkcreds) $ updateIRwd ru
        nonDistributed = Map.foldl (+) (Coin 0) unregistered
        updateRwd = Map.mapKeys mkRwdAcnt rewMir'

-- | Create a reward update
createRUpd
  :: EpochNo
  -> BlocksMade crypto
  -> EpochState crypto
  -> ShelleyBase (RewardUpdate crypto)
createRUpd e b@(BlocksMade b') (EpochState acnt ss ls pp) = do
    ei <- asks epochInfo
    slotsPerEpoch <- epochInfoSize ei e
    let (stake', delegs') = _pstakeGo ss
        poolsSS' = _poolsSS ss
        Coin reserves = _reserves acnt
        ds = _dstate $ _delegationState ls

        -- instantaneous rewards
        Coin rewardsMIR = Map.foldl (+) (Coin 0) (_irwd ds)
        reserves' = reserves - rewardsMIR

        -- reserves and rewards change
        deltaRl = (floor $ min 1 eta * intervalValue (_rho pp) * fromIntegral reserves')
        deltaR_ = deltaRl + Coin rewardsMIR
        expectedBlocks = intervalValue (_activeSlotCoeff pp) * fromIntegral slotsPerEpoch
        eta = fromIntegral blocksMade / expectedBlocks

        Coin rewardPot = _feeSS ss + deltaRl
        deltaT1 = floor $ intervalValue (_tau pp) * fromIntegral rewardPot
        _R = Coin $ rewardPot - deltaT1

        rs_ = reward pp b _R (Map.keysSet $ _rewards ds) poolsSS' stake' delegs'
        deltaT2 = _R - (Map.foldr (+) (Coin 0) rs_)

        blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
    pure $ RewardUpdate (Coin deltaT1 + deltaT2) (-deltaR_) rs_ (-(_feeSS ss)) (_irwd ds)

-- | Overlay schedule
-- This is just a very simple round-robin, evenly spaced schedule.
overlaySchedule
  :: EpochNo
  -> Set (GenKeyHash crypto)
  -> PParams crypto
  -> ShelleyBase (Map SlotNo (Maybe (GenKeyHash crypto)))
overlaySchedule e gkeys pp = do
  let dval = intervalValue $ pp ^. d
  if dval == 0
    then
      pure Map.empty
    else do
      ei <- asks epochInfo
      slotsPerEpoch <- epochInfoSize ei e
      firstSlotNo <- epochInfoFirst ei e
      let
        numActive = dval * fromIntegral slotsPerEpoch
        dInv = 1 / dval
        asc = intervalValue $ pp ^. activeSlotCoeff

        toRelativeSlotNo x = (Duration . floor) (dInv * fromInteger x)
        toSlotNo x = firstSlotNo +* toRelativeSlotNo x

        genesisSlots = [ toSlotNo x | x <-[0..(floor numActive)] ]

        numInactivePerActive = floor ((1 - asc) * fromRational numActive) - 1
        activitySchedule = cycle (True:replicate numInactivePerActive False)
        unassignedSched = zip activitySchedule genesisSlots

        active =
          Map.fromList $ fmap
            (\(gk,(_,s))->(s, Just gk))
            (zip (cycle (Set.toList gkeys)) (filter fst unassignedSched))
        inactive =
          Map.fromList $ fmap
            (\x -> (snd x, Nothing))
            (filter (not . fst) unassignedSched)
      pure $ Map.union active inactive

-- | Update new epoch state
updateNES
  :: NewEpochState crypto
  -> BlocksMade crypto
  -> LedgerState crypto
  -> NewEpochState crypto
updateNES (NewEpochState eL bprev _
           (EpochState acnt ss _ pp) ru pd osched) bcur ls =
  NewEpochState eL bprev bcur (EpochState acnt ss ls pp) ru pd osched
