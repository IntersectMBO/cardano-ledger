{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ModelChain where

import Cardano.Ledger.Coin
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints
import Cardano.Slotting.Slot hiding (at)
import Control.Lens
import Control.Monad
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Keys
import Cardano.Ledger.SafeHash (HashAnnotated)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Slotting.EpochInfo.API
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (lift)
import Data.Default.Class
import Data.Foldable
import Data.Group
import Data.Kind (Type)
import Data.Proxy
import Data.Set (Set)
import Data.Traversable
import Data.Word (Word64)
import Numeric.Natural
import Shelley.Spec.Ledger.API.Genesis
import Shelley.Spec.Ledger.API.Mempool
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (mkKeyPairs)
import Test.Shelley.Spec.Ledger.Utils (mkVRFKeyPair)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as C
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Val as Val
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.State as State hiding (state)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified GHC.Exts as GHC
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.Tx as Shelley
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Credential
  ( Credential(..)
  , StakeCredential
  )
import Shelley.Spec.Ledger.TxBody
  ( DCert(..)
  , PoolParams(..)
  , DelegCert(..)
  , Delegation(..)
  , PoolCert(..)
  )
import qualified Cardano.Crypto.VRF.Class (VerKeyVRF)
import qualified Shelley.Spec.Ledger.UTxO as UTxO
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Cardano.Ledger.SafeHash (SafeHash)


type KeyPair' crypto = (KeyPair 'Payment crypto, KeyPair 'Staking crypto)

data EraElaboratorState era = EraElaboratorState
  { _eesUnusedKeyPairs :: Word64
  , _eesKeys :: Map.Map ModelAddress (TestKeyPair (Crypto era))
  , _eesTxIds :: Map.Map ModelTxId (Shelley.TxId (Crypto era))
  , _eesCurrentEpoch :: EpochNo
  , _eesUTxOs :: Map.Map ModelUTxOId ModelAddress
  , _eesPendingWitnessKeys :: [KeyPair 'Witness (Crypto era)]
  , _eesCurrentSlot :: SlotNo
  }

deriving instance
  ( C.Crypto (Crypto era)
  )
  => Show (EraElaboratorState era)

class HasEraElaboratorState s era | s -> era where
  eraElaboratorState
    :: forall f. Functor f
    => (EraElaboratorState era -> f (EraElaboratorState era)) -> s -> f s

instance HasEraElaboratorState (EraElaboratorState era) era where
  eraElaboratorState = id

eesUnusedKeyPairs :: Functor f => (Word64 -> f Word64) -> EraElaboratorState era -> f (EraElaboratorState era)
eesUnusedKeyPairs a2fb s = (\b -> s {_eesUnusedKeyPairs = b}) <$> a2fb (_eesUnusedKeyPairs s)

eesKeys :: Functor f => (Map.Map ModelAddress (TestKeyPair (Crypto era)) -> f (Map.Map ModelAddress (TestKeyPair (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesKeys a2fb s = (\b -> s {_eesKeys = b}) <$> a2fb (_eesKeys s)

eesTxIds :: Functor f => (Map.Map ModelTxId (Shelley.TxId (Crypto era)) -> f (Map.Map ModelTxId (Shelley.TxId (Crypto era)))) -> EraElaboratorState era -> f (EraElaboratorState era)
eesTxIds a2fb s = (\b -> s {_eesTxIds = b}) <$> a2fb (_eesTxIds s)

eesCurrentEpoch :: Functor f => (EpochNo -> f EpochNo) -> EraElaboratorState era -> f (EraElaboratorState era)
eesCurrentEpoch a2fb s = (\b -> s {_eesCurrentEpoch = b}) <$> a2fb (_eesCurrentEpoch s)

eesPendingWitnessKeys :: Functor f => ([KeyPair 'Witness (Crypto era)] -> f [KeyPair 'Witness (Crypto era)]) -> EraElaboratorState era -> f (EraElaboratorState era)
eesPendingWitnessKeys a2fb s = (\b -> s {_eesPendingWitnessKeys = b}) <$> a2fb (_eesPendingWitnessKeys s)

eesUTxOs :: Lens' (EraElaboratorState era) (Map.Map ModelUTxOId ModelAddress)
eesUTxOs a2fb s = (\b -> s {_eesUTxOs = b}) <$> a2fb (_eesUTxOs s)

eesCurrentSlot :: Lens' (EraElaboratorState era) SlotNo
eesCurrentSlot a2fb s = (\b -> s {_eesCurrentSlot = b}) <$> a2fb (_eesCurrentSlot s)

data TestKeyPair crypto = TestKeyPair
  { _tkpKeyPair :: KeyPair' crypto
  , _tkpVRF :: (SignKeyVRF crypto , VerKeyVRF crypto )
  , _tkpAddr :: Addr crypto
  , _tkpVRFHash :: Hash.Hash (C.HASH crypto) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF crypto))
  , _tkpStakePool :: KeyHash 'Staking crypto
  , _tkpStakeCredential :: StakeCredential crypto
  }

deriving instance
  ( C.Crypto crypto
  )
  => Show (TestKeyPair crypto)


getKeyPairForImpl
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => proxy era -> ModelAddress
  -> m (TestKeyPair (Crypto era))
getKeyPairForImpl _ mAddr = do
  st <- use eraElaboratorState
  case Map.lookup mAddr (_eesKeys st) of
    Just k -> pure k
    Nothing -> do
      unusedKeyPairId <- eraElaboratorState . eesUnusedKeyPairs <<%= succ

      let
        keyPair@(_, poolKey) = mkKeyPairs @(Crypto era) unusedKeyPairId
        vrf@(_, vrf') = mkVRFKeyPair @(C.VRF (Crypto era)) (1, 0, 0, 0, unusedKeyPairId)
        k = TestKeyPair
          { _tkpKeyPair = keyPair
          , _tkpVRF = vrf
          , _tkpAddr = toAddr Testnet keyPair -- TODO: network can be read from Globals
          , _tkpVRFHash = hashVerKeyVRF vrf'
          , _tkpStakePool = hashKey $ vKey poolKey
          , _tkpStakeCredential = KeyHashObj . hashKey $ vKey poolKey
          }

      eraElaboratorState . eesKeys . at mAddr .= Just k
      pure k

getKeyPairFor :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (KeyPair' (Crypto era))
getKeyPairFor proxy mAddr = _tkpKeyPair <$> getKeyPairForImpl proxy mAddr

getAddrFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (Addr (Crypto era))
getAddrFor proxy mAddr = _tkpAddr <$> getKeyPairForImpl proxy mAddr

getStakingKeyHashFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (KeyHash 'Staking (Crypto era))
getStakingKeyHashFor proxy maddr = _tkpStakePool <$> getKeyPairForImpl proxy maddr

getStakePoolFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (KeyHash 'StakePool (Crypto era))
getStakePoolFor proxy maddr = coerceKeyRole @_ @_ @(Crypto era) . _tkpStakePool <$> getKeyPairForImpl proxy maddr

getStakeCredenetialFor
  :: forall m era st proxy.
  ( MonadState st m , HasEraElaboratorState st era , C.Crypto (Crypto era))
  => proxy era -> ModelAddress
  -> m (StakeCredential (Crypto era))
getStakeCredenetialFor proxy maddr = _tkpStakeCredential <$> getKeyPairForImpl proxy maddr

getHashKeyVRFFor
  :: forall m era st proxy.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => proxy era -> ModelAddress
  -> m (Hash.Hash (C.HASH (Crypto era)) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF (Crypto era))))
getHashKeyVRFFor proxy maddr = _tkpVRFHash <$> getKeyPairForImpl proxy maddr


instance Default (EraElaboratorState era) where
  def = EraElaboratorState
    { _eesUnusedKeyPairs = 1
    , _eesKeys = Map.empty
    , _eesTxIds = Map.empty
    , _eesCurrentEpoch = 0
    , _eesCurrentSlot = 0
    , _eesPendingWitnessKeys = []
    , _eesUTxOs = Map.empty
    }

mkTxOut
  :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , Era era
  , UsesTxOut era
  )
  => ModelUTxOId
  -> ModelTxOut
  -> m (Core.TxOut era)
mkTxOut mutxoId (ModelTxOut mAddr (ModelValue mValue)) = do
  eraElaboratorState . eesUTxOs . at mutxoId .= Just mAddr
  addr <- getAddrFor (Proxy :: Proxy era) mAddr
  pure (makeTxOut (Proxy :: Proxy era) addr (Val.inject $ Coin mValue))

mkTxIn :: forall m era st.
  ( MonadState st m
  , HasEraElaboratorState st era
  , C.Crypto (Crypto era)
  )
  => ModelTxIn
  -> m (Set.Set (Shelley.TxIn (Crypto era)))
mkTxIn = \case
  -- TODO: handle missing txnIds more gracefully?
  ModelTxIn mtxId idx -> do
    ses <- use eraElaboratorState
    ownerMAddr <- use $ eraElaboratorState . eesUTxOs . at (ModelUTxOId mtxId idx)
    for_ ownerMAddr $ \mAddr -> do
      (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
      pushWitness myKeys
    pure . maybe Set.empty Set.singleton $ Shelley.TxIn <$> Map.lookup mtxId (_eesTxIds ses) <*> pure idx
  ModelGensisIn mAddr -> do
    myAddr <- getAddrFor (Proxy :: Proxy era) mAddr
    (myKeys, _) <- getKeyPairFor (Proxy :: Proxy era) mAddr
    pushWitness myKeys
    pure $ Set.singleton $ initialFundsPseudoTxIn $ myAddr

pushWitness
  :: forall kr m era st.
  ( HasEraElaboratorState st era
  , MonadState st m
  )
  => KeyPair kr (Crypto era)
  -> m ()
pushWitness keyP = eraElaboratorState . eesPendingWitnessKeys %= (:) (coerceKeyRole keyP)

popWitnesses
  :: forall proxy m era st.
  ( HasEraElaboratorState st era
  , MonadState st m
  , DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody)
  , C.Crypto (Crypto era)
  )
  => proxy era
  -> SafeHash (Crypto era) Shelley.EraIndependentTxBody
  -> m (Set.Set ( Shelley.WitVKey 'Witness (Crypto era) ) )
popWitnesses _ bodyHash = do
  witness <- eraElaboratorState . eesPendingWitnessKeys <<.= []
  pure $ flip foldMap witness $ \keyP ->
    Set.singleton $ UTxO.makeWitnessVKey bodyHash keyP


newtype ModelTxId = ModelTxId Integer
  deriving (Eq, Ord, Show, Num)
newtype ModelAddress = ModelAddress String
  deriving (Eq, Ord, Show, GHC.IsString)

-- similarity to coin is merely a temporary convenience; not a design feature.
newtype ModelValue = ModelValue { unModelValue :: Integer }
  deriving (Eq, Ord, Show, Num)
deriving via Coin instance Semigroup ModelValue
deriving via Coin instance Monoid ModelValue
deriving via Coin instance Group ModelValue
deriving via Coin instance Abelian ModelValue
deriving via Coin instance Val.Val ModelValue


data ModelTxIn
  = ModelTxIn ModelTxId Natural
  | ModelGensisIn ModelAddress
  deriving (Eq, Ord, Show)
data ModelTxOut = ModelTxOut ModelAddress ModelValue
  deriving (Eq, Ord, Show)

data ModelUTxOId = ModelUTxOId ModelTxId Natural
  deriving (Eq, Ord, Show)

data ModelTx = ModelTx
  { _mtxId :: !ModelTxId
  , _mtxInputs :: !(Set ModelTxIn)
  , _mtxOutputs :: ![ModelTxOut]
  , _mtxFee :: !ModelValue
  , _mtxDCert :: ![ModelDCert]
  , _mtxWdrl :: !(Map.Map ModelAddress Coin)
  }

data ModelBlock = ModelBlock SlotNo [ModelTx]
data ModelBlocksMade = ModelBlocksMade (Map.Map ModelAddress Natural)
data ModelEpoch = ModelEpoch [ModelBlock] ModelBlocksMade

data ModelDelegation = ModelDelegation
  { _mdDelegator :: !ModelAddress
  , _mdDelegatee :: !ModelAddress
  }

data ModelPoolParams = ModelPoolParams
  { _mppId :: !ModelAddress
  , _mppPledge :: !Coin
  , _mppCost :: !Coin
  , _mppMargin :: !UnitInterval
  , _mppRAcnt :: !ModelAddress
  , _mppOwners :: ![ModelAddress]
  }

-- ignores genesis delegation details.
data ModelDCert
   = ModelRegisterStake ModelAddress
   | ModelDeRegisterStake ModelAddress
   | ModelDelegate ModelDelegation
   | ModelRegisterPool ModelPoolParams
   | ModelRetirePool ModelAddress EpochNo
   -- TODO: | ModelMIRCert Shelley.MIRPot (Map.Map ModelAddress DeltaCoin)


instance Semigroup ModelBlocksMade where
  ModelBlocksMade x <> ModelBlocksMade y = ModelBlocksMade $ Map.unionWith (+) x y
instance Monoid ModelBlocksMade where
  mempty = ModelBlocksMade Map.empty


data ModelPredicateFailure
  = ModelValueNotConservedUTxO
      !ModelValue
      -- ^ the Coin consumed by this transaction
      !ModelValue
      -- ^ the Coin produced by this transaction

mempoolState :: Functor f => (MempoolState era -> f (MempoolState era)) -> (NewEpochState era -> f (NewEpochState era))
mempoolState = \a2b s ->
  let
    nesEs = LedgerState.nesEs s
    esLState = LedgerState.esLState nesEs

    mkNES (utxoState, delegationState) = s
      { LedgerState.nesEs = nesEs
        { LedgerState.esLState = esLState
          { LedgerState._utxoState = utxoState
          , LedgerState._delegationState = delegationState
          }
        }
      }
  in mkNES <$> a2b (mkMempoolState s)
{-# INLINE mempoolState #-}

class ElaborateEraModel era where
  type ElaborateEraModelState era :: Type
  type ElaborateEraModelState era = EraElaboratorState era

  elaborateBlock
    :: Globals
    -> ModelBlock
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( Either (ApplyTxError era) ()
       , (NewEpochState era, ElaborateEraModelState era)
       )

  default elaborateBlock
    ::
    ( ApplyBlock era
    , ApplyTx era
    , HasEraElaboratorState (ElaborateEraModelState era) era
    )
    => Globals
    -> ModelBlock
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( Either (ApplyTxError era) ()
       , (NewEpochState era, ElaborateEraModelState era)
       )
  elaborateBlock globals = State.runState . Except.runExceptT . \case
    ModelBlock mslot mtxSeq -> do
      currentEpoch <- use $ _2 . eraElaboratorState . eesCurrentEpoch
      let
        slot = runIdentity (epochInfoFirst ei currentEpoch) + mslot
        ei = epochInfo globals
        ttl = succ slot

      unless (currentEpoch == runIdentity (epochInfoEpoch ei slot)) $ error $ "model slot out of range: " <> show mslot
      _2 . eraElaboratorState . eesCurrentSlot .= slot
      -- tick the model
      lift $ zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)
      txSeq <- lift $ zoom _2 $ for mtxSeq $ \tx -> State.state $ elaborateTx (Proxy :: Proxy era) globals ttl tx
      mempoolEnv <- (\(nes0, _) -> mkMempoolEnv nes0 slot) <$> get

      -- apply the transactions.
      for_ txSeq $ \tx -> do
        (nes0, ems) <- get
        mps' <- applyTxInBlock globals mempoolEnv (view mempoolState nes0) tx
        let nes1 = set mempoolState mps' nes0
        put (nes1, ems)

      -- pure (slot, txSeq)
      pure ()

  elaborateInitialState
    :: ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Map.Map ModelAddress Coin
    -> ElaborateEraModelState era
    -> ( NewEpochState era
       , ElaborateEraModelState era
       )

  default elaborateInitialState
    :: ( HasEraElaboratorState (ElaborateEraModelState era) era
       , C.Crypto (Crypto era)
       , CanStartFromGenesis era
       )
    => ShelleyGenesis era
    -> AdditionalGenesisConfig era
    -> Map.Map ModelAddress Coin
    -> ElaborateEraModelState era
    -> ( NewEpochState era
       , ElaborateEraModelState era
       )
  elaborateInitialState sg additionalGenesesConfig genesisAccounts = State.runState $ do
    utxo0 <- fmap Map.fromList $ for (Map.toList genesisAccounts) $ \(mAddr, coins) -> do
      addr <- getAddrFor (Proxy :: Proxy era) mAddr
      pure (addr, coins)

    pure $ initialState sg {sgInitialFunds = Map.unionWith const utxo0 $ sgInitialFunds sg } additionalGenesesConfig

  makeTxBody
    :: proxy era
    -> SlotNo -- ttl
    -> Coin -- fee
    -> Set.Set (Shelley.TxIn (Crypto era))
    -> StrictSeq.StrictSeq (Core.TxOut era)
    -> StrictSeq.StrictSeq (DCert (Crypto era))
    -> Shelley.Wdrl (Crypto era)
    -> Core.TxBody era

  elaborateTx
    :: proxy era
    -> Globals
    -> SlotNo
    -> ModelTx
    -> ElaborateEraModelState era
    -> (Era.TxInBlock era, ElaborateEraModelState era)
  default elaborateTx ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody)
    , Hash.HashAlgorithm (C.HASH (Crypto era))
    , HashAnnotated (Core.TxBody era) Shelley.EraIndependentTxBody (Crypto era)
    , C.Crypto (Crypto era)
    , UsesTxOut era
    )
    => proxy era
    -> Globals
    -> SlotNo
    -> ModelTx
    -> ElaborateEraModelState era
    -> (Era.TxInBlock era, ElaborateEraModelState era)
  elaborateTx proxy _ maxTTL (ModelTx mtxId mtxInputs mtxOutputs mtxFee mtxDCert mtxWdrl) = State.runState $ do
    outs <- ifor mtxOutputs $ \idx -> mkTxOut $ ModelUTxOId mtxId $ toEnum @Natural idx
    ins :: Set.Set (Shelley.TxIn (Crypto era)) <- fmap fold $ traverse mkTxIn $ Set.toList mtxInputs
    dcerts <- traverse (mkDCerts (Proxy :: Proxy era)) mtxDCert
    wdrl <- fmap Map.fromList $ for (Map.toList mtxWdrl) $ \(mAddr, qty) -> do
      stakeCredential <- getStakeCredenetialFor proxy mAddr
      (_, stakeKey) <- getKeyPairFor proxy mAddr
      pushWitness stakeKey
      pure (RewardAcnt Testnet stakeCredential, qty)
    let
      realTxBody = makeTxBody proxy maxTTL (Coin . unModelValue $ mtxFee) ins (StrictSeq.fromList outs) (StrictSeq.fromList dcerts) (Shelley.Wdrl wdrl)
      bodyHash = hashAnnotated realTxBody

    wits <- popWitnesses (Proxy :: Proxy era) bodyHash

    eraElaboratorState . eesTxIds %= Map.insert mtxId (UTxO.txid @era realTxBody)
    pure $ makeTx proxy realTxBody wits

  makeTx
    :: proxy era
    -> Core.TxBody era
    -> Set.Set ( Shelley.WitVKey 'Witness (Crypto era) )
    -> Era.TxInBlock era

  toEraPredicateFailure
    :: ModelPredicateFailure
    -> ApplyBlockTransitionError era

  elaborateBlocksMade
    :: Globals
    -> ModelBlocksMade
    -> ( NewEpochState era, ElaborateEraModelState era)
    -> ( BlocksMade (Crypto era)
       , (NewEpochState era, ElaborateEraModelState era)
       )

  default elaborateBlocksMade ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , C.Crypto (Crypto era)
    , ApplyBlock era
    )
    => Globals
    -> ModelBlocksMade
    -> (NewEpochState era, ElaborateEraModelState era)
    -> ( BlocksMade (Crypto era)
       , (NewEpochState era, ElaborateEraModelState era)
       )
  elaborateBlocksMade globals (ModelBlocksMade mblocksMade) = State.runState $ do
    bs <- for (Map.toList mblocksMade) $ \(maddr, n) -> do
      poolKey <- zoom _2 $ getStakePoolFor (Proxy :: Proxy era) maddr
      pure (poolKey, n)

    let bs' = BlocksMade $ Map.fromList bs
    _1 %= emulateBlocksMade bs'

    prevEpoch <- use $ _2 . eraElaboratorState . eesCurrentEpoch
    epoch <- _2 . eraElaboratorState . eesCurrentEpoch <%= succ
    let
      ei = epochInfo globals
      firstOfNew = runIdentity $ epochInfoFirst ei epoch

      neededSlot
        = SlotNo (randomnessStabilisationWindow globals)
        + runIdentity (epochInfoFirst ei prevEpoch)

    currentSlot <- _2 . eraElaboratorState . eesCurrentSlot <<.= firstOfNew

    unless (currentSlot > neededSlot) $
      zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 (neededSlot + 1))
    zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 firstOfNew)

    pure bs'


  elaborateDCert
    :: proxy era
    -> ModelDCert
    -> ElaborateEraModelState era
    -> (DCert (Crypto era), ElaborateEraModelState era)

  default elaborateDCert ::
    ( HasEraElaboratorState (ElaborateEraModelState era) era
    , C.Crypto (Crypto era)
    )
    => proxy era
    -> ModelDCert
    -> ElaborateEraModelState era
    -> (DCert (Crypto era), ElaborateEraModelState era)
  elaborateDCert proxy = State.runState . \case
    ModelRegisterStake maddr -> DCertDeleg . RegKey
      <$> getStakeCredenetialFor proxy maddr
    ModelDeRegisterStake maddr -> DCertDeleg . DeRegKey
      <$> getStakeCredenetialFor proxy maddr
    ModelDelegate (ModelDelegation mdelegator mdelegatee) -> do
      dtor <- getStakeCredenetialFor proxy mdelegator
      dtee <- getStakePoolFor proxy mdelegatee
      (_, stakeKey) <- getKeyPairFor proxy mdelegator
      pushWitness stakeKey
      pure $ (DCertDeleg . Delegate) $ Delegation dtor dtee

    ModelRegisterPool (ModelPoolParams mPoolId pledge cost margin mRAcnt mOwners) -> do
      poolId <- getStakePoolFor proxy mPoolId
      poolVRF <- getHashKeyVRFFor proxy mPoolId
      (_, poolKey) <- getKeyPairFor proxy mPoolId
      poolOwners <- Set.fromList <$> traverse (getStakingKeyHashFor proxy) mOwners
      pushWitness poolKey
      rAcnt <- RewardAcnt Testnet <$> getStakeCredenetialFor proxy mRAcnt
      pure $ (DCertPool . RegPool) $ PoolParams
        { _poolId = poolId
        , _poolVrf = poolVRF
        , _poolPledge = pledge
        , _poolCost = cost
        , _poolMargin = margin
        , _poolRAcnt = rAcnt
        , _poolOwners = poolOwners
        , _poolRelays = StrictSeq.empty
        , _poolMD = SNothing
        }

    ModelRetirePool maddr epochNo -> do
      fmap DCertPool $ RetirePool
        <$> getStakePoolFor proxy maddr
        <*> pure epochNo

    -- TODO: maybe useful someday
    -- ModelMIRCert srcPot mRewards ->
    --   fmap ( DCertMir . Shelley.MIRCert srcPot . Shelley.StakeAddressesMIR . Map.fromList) $
    --   for (Map.toList mRewards) $ \(maddr, reward) -> (,)
    --     <$> getStakeCredenetialFor proxy maddr
    --     <*> pure reward


mkDCerts :: forall m era proxy.
  ( MonadState (ElaborateEraModelState era) m
  , ElaborateEraModel era
  )
  => proxy era
  -> ModelDCert
  -> m (DCert (Crypto era))
mkDCerts proxy x = State.state $ elaborateDCert proxy x

-- | simulate blocks made in the current epoch.  this functions like ApplyBlock or
-- ApplyTx, but without presenting real blocks to the ledger.  This is only
-- useful in testing the correctness of specific aspects of the ledger rather
-- than in normal use.
emulateBlocksMade
  :: forall era.
     BlocksMade (Crypto era)
  -> NewEpochState era
  -> NewEpochState era
emulateBlocksMade (BlocksMade newBlocksMade) nes@(LedgerState.NewEpochState {LedgerState.nesBcur = BlocksMade currentBlocksMade })
  = nes {LedgerState.nesBcur = BlocksMade (Map.unionWith (+) newBlocksMade currentBlocksMade)}

type ModelChainInteraction = ModelEpoch

elaborateBlocks_
  :: forall era.
  ( ElaborateEraModel era )
  => Globals
  -> [ModelChainInteraction]
  -> (NewEpochState era, ElaborateEraModelState era)
  -> ( Either (ApplyTxError era) ()
     , (NewEpochState era, ElaborateEraModelState era)
     )
elaborateBlocks_ globals = State.runState . Except.runExceptT . traverse_ f
  where
    f (ModelEpoch blocks blocksMade) = do
      for_ blocks (Except.ExceptT . State.state . elaborateBlock globals)
      _ :: BlocksMade (Crypto era) <- lift $ State.state $ elaborateBlocksMade globals blocksMade
      pure ()


observeRewards
  :: forall era.
  (HasEraElaboratorState (ElaborateEraModelState era) era)
  => ( NewEpochState era, ElaborateEraModelState era )
  -> Map.Map ModelAddress Coin
observeRewards (nes, ems) =
  let
    creds :: Map.Map (StakeCredential (Crypto era)) ModelAddress
    creds = Map.fromList $ (\(alias, tkp) -> (_tkpStakeCredential tkp, alias)) <$> Map.toList (view (eraElaboratorState . eesKeys) ems)
  in Map.fromList $ do
    (a, b) <- Map.toList . LedgerState._rewards . LedgerState._dstate . LedgerState._delegationState . LedgerState.esLState $ LedgerState.nesEs nes
    a' <- case Map.lookup a creds of
      Just a' -> pure a'
      Nothing -> error $ "observeRewards: can't find " <> show a
    pure (a', b)

data ApplyBlockTransitionError era
   = ApplyBlockTransitionError_Tx (ApplyTxError era)

deriving instance
 ( Show (ApplyTxError era)
 ) => Show (ApplyBlockTransitionError era)

deriving instance
 ( Eq (ApplyTxError era)
 ) => Eq (ApplyBlockTransitionError era)

deriving instance
 ( Ord (ApplyTxError era)
 ) => Ord (ApplyBlockTransitionError era)

