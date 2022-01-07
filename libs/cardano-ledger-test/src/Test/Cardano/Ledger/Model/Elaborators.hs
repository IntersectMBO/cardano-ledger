{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains the mechanism used to related a ledger model
-- transaction with a real implementation.
module Test.Cardano.Ledger.Model.Elaborators
  ( -- | = API for property testing
    ElaborateEraModel (..),
    elaborateBlocks_,
    CompareModelLedger (..),
    EraElaboratorStats (..),
    eraFeatureSet,
    EraValueFeature,
    EraScriptFeature,
    -- | = API for instances
    noScriptAction,
    lookupModelValue,
    TxWitnessArguments (..),
    TxBodyArguments (..),
    ExpectedValueTypeC (..),
    -- | = semi-internal for property testing.
    ApplyBlockTransitionError (..),
    ElaborateApplyTxError (..),
    ElaborateBlockError (..),
    observeRewards,
    -- | = internal implementation details.
    EraElaboratorState (..),
    TestCredentialInfo (..),
    ElaborateValueType,
  )
where

import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class (VerKeyVRF)
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ScriptPurpose (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Globals (..),
    Network (Testnet),
    StrictMaybe (..),
    epochInfo,
  )
import Cardano.Ledger.Coin (Coin (..), toDeltaCoin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as C
import Cardano.Ledger.Era (Crypto, Era, ValidateScript, hashScript)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    HasKeyRole,
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Cardano.Ledger.Mary.Value (PolicyID (..), Value)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.API.Mempool
  ( ApplyTx,
    ApplyTxError,
    MempoolEnv,
    MempoolState,
    applyTx,
    mkMempoolEnv,
    mkMempoolState,
  )
import Cardano.Ledger.Shelley.API.Validation (ApplyBlock, applyTick)
import Cardano.Ledger.Shelley.API.Wallet (totalAdaES)
import Cardano.Ledger.Shelley.Constraints (UsesTxOut, UsesValue, makeTxOut)
import Cardano.Ledger.Shelley.Genesis (initialFundsPseudoTxIn)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import qualified Cardano.Ledger.Shelley.Rules.Ledger
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
  )
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as UTxO
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import qualified Cardano.Ledger.TxIn as TxIn
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API
  ( epochInfoEpoch,
    epochInfoFirst,
  )
import Cardano.Slotting.Slot hiding (at)
import Control.Arrow ((&&&))
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Lens
  ( Lens',
    at,
    ifor,
    ifor_,
    itoListOf,
    set,
    use,
    view,
    zoom,
    (%=),
    (.=),
    (<%=),
    (<<%=),
    (<<.=),
    (<>=),
    _1,
    _2,
    _Just,
  )
import Control.Monad (unless)
import qualified Control.Monad.Except as Except
import Control.Monad.State (MonadState (..))
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State hiding (get, gets, state)
import qualified Control.Monad.Writer.CPS as CPS
import Data.Bool (bool)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.Group.GrpMap (GrpMap (..))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.UMap (rewView)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic, (:.:) (Comp1))
import GHC.Records (HasField (..))
import qualified PlutusTx
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Model.API
  ( ModelBlock (..),
    ModelEpoch (..),
    ModelGenesis (..),
    ModelLedger (..),
    applyModelBlock,
    applyModelBlocksMade,
    execModelM,
    mkModelLedger,
  )
import Test.Cardano.Ledger.Model.Acnt (ModelAcntF (..))
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelBlocksMade (..),
    ModelPoolId (..),
    ModelValue (..),
    ModelValueVars (..),
    totalPreservedAda,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    FeatureSupport (..),
    FeatureTag,
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    IfSupportsScript (..),
    IfSupportsTimelock (..),
    KnownRequiredFeatures (..),
    ScriptFeature,
    ScriptFeatureTag (..),
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeature,
    ifSupportsPlutus,
    reifyScriptFeature,
    reifySupportsPlutus,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelDPState (..),
    ModelDState (..),
    ModelEpochState (..),
    ModelInstantaneousReward (..),
    ModelInstantaneousRewards,
    ModelLState (..),
    ModelNewEpochState (..),
    ModelUTxOState (..),
  )
import Test.Cardano.Ledger.Model.PParams (ModelPParams)
import Test.Cardano.Ledger.Model.Rules (ModelPredicateFailure (..))
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
    ModelScript (..),
    coerceKeyRole',
    elaborateModelScript,
    elaborateModelTimelock,
    filterModelCredential,
    liftModelCredential,
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelDelegCert (..),
    ModelDelegation (..),
    ModelGenesisDelegCert (..),
    ModelMIRCert (..),
    ModelMIRTarget (..),
    ModelPoolCert (..),
    ModelPoolParams (..),
    ModelScriptPurpose (..),
    ModelTx (..),
    ModelTxId,
    mkMintValue,
    modelTx_redeemers,
  )
import Test.Cardano.Ledger.Model.TxOut (ModelTxOut (..), ModelUTxOId)
import Test.Cardano.Ledger.Model.UTxO (ModelUTxOMap (..))
import Test.Cardano.Ledger.Model.Value (evalModelValue)
import Test.Cardano.Ledger.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkVRFKeyPair)
import Test.Cardano.Ledger.ValueFromList (ValueFromList (..))
import qualified Text.Show as Show

data ExpectedValueTypeC era where
  ExpectedValueTypeC_Simple ::
    ( Core.Value era ~ Coin,
      EraValueFeature era ~ 'ExpectAdaOnly
    ) =>
    ExpectedValueTypeC era
  ExpectedValueTypeC_MA ::
    ( ValueFromList (Core.Value era) (Crypto era),
      EraValueFeature era ~ 'ExpectAnyOutput
    ) =>
    ExpectedValueTypeC era

newtype ElaboratedScriptsCache era = ElaboratedScriptsCache
  {unElaboratedScriptsCache :: Map.Map (ScriptHash (Crypto era)) (Core.Script era)}
  deriving (Generic)

instance Semigroup (ElaboratedScriptsCache era) where
  ElaboratedScriptsCache a <> ElaboratedScriptsCache b = ElaboratedScriptsCache $ Map.unionWith const a b

instance Monoid (ElaboratedScriptsCache era) where
  mempty = ElaboratedScriptsCache Map.empty

instance Show (ElaboratedScriptsCache era) where
  showsPrec n (ElaboratedScriptsCache xs) =
    Show.showParen (n >= 11) $
      Show.showString "ElaboratedScriptsCache "
        . showsPrec 11 (Map.keysSet xs)

data EraElaboratorStats era = EraElaboratorStats
  { -- | total wdrls in model
    _eeStats_wdrls :: Int,
    -- | number of wdrls when no rewards are available
    _eeStats_badWdrls :: Int,
    -- | the arguments to applyTx
    _eeStats_adaConservedErrors ::
      [ ( Globals,
          MempoolEnv era,
          (LedgerState.UTxOState era, LedgerState.DPState (Crypto era)),
          Core.Tx era
        )
      ]
  }
  deriving (Generic)

instance Eq (EraElaboratorStats era) where
  x == y = compare x y == EQ

instance Ord (EraElaboratorStats era) where
  compare =
    on compare _eeStats_wdrls
      <> on compare _eeStats_badWdrls
      <> on compare (length . _eeStats_adaConservedErrors)

deriving instance
  ( LedgerState.TransUTxOState Show era,
    Show (Core.Tx era)
  ) =>
  Show (EraElaboratorStats era)

instance NFData (EraElaboratorStats era) where
  rnf = rwhnf

instance Semigroup (EraElaboratorStats era) where
  EraElaboratorStats wdrls badWdrls adaConservedErrors <> EraElaboratorStats wdrls' badWdrls' adaConservedErrors' =
    EraElaboratorStats
      (wdrls + wdrls')
      (badWdrls + badWdrls')
      (adaConservedErrors <> adaConservedErrors')

instance Monoid (EraElaboratorStats era) where
  mempty = EraElaboratorStats 0 0 []

data EraElaboratorState era = EraElaboratorState
  { _eesUnusedKeyPairs :: Word64,
    _eesKeys :: Map.Map (ModelCredential 'Witness (EraScriptFeature era)) (TestCredentialInfo 'Witness era),
    _eesCurrentEpoch :: EpochNo,
    _eesUTxOs :: Map.Map ModelUTxOId (TestUTxOInfo era),
    _eesCurrentSlot :: SlotNo,
    _eesStakeCredentials :: Map.Map (Credential 'Witness (Crypto era)) (ModelCredential 'Witness (EraScriptFeature era)),
    _eesStats :: EraElaboratorStats era,
    _eesModel :: !(ModelLedger (EraFeatureSet era))
  }

deriving instance
  ( C.Crypto (Crypto era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era,
    Show (Core.Tx era)
  ) =>
  Show (EraElaboratorState era)

data TestUTxOInfo era = TestUTxOInfo
  { -- | when creating outputs at "genesis", we know the txid's apriori; and record
    -- them immediately.  otherwise we don't know the txid's until we're done
    -- composing the txBody, and need a layer of indirection.  By the time the
    -- utxo is being used as an input, this should be filled in with a Just.
    _tuoi_txid :: Maybe (Shelley.TxIn (Crypto era)),
    _tuoi_maddr :: ModelAddress (EraScriptFeature era),
    _tuoi_data :: StrictMaybe (Alonzo.DataHash (Crypto era), (Alonzo.Data era))
  }
  deriving stock (Generic)
  deriving (Show) via (Quiet (TestUTxOInfo era))

tuoi_txid :: Lens' (TestUTxOInfo era) (Maybe (Shelley.TxIn (Crypto era)))
tuoi_txid a2fb s = (\b -> s {_tuoi_txid = b}) <$> a2fb (_tuoi_txid s)
{-# INLINE tuoi_txid #-}

-- tuoi_maddr :: Lens' (TestUTxOInfo era) (ModelAddress (EraScriptFeature era))
-- tuoi_maddr a2fb s = (\b -> s {_tuoi_maddr = b}) <$> a2fb (_tuoi_maddr s)
-- {-# INLINE tuoi_maddr #-}
--
-- tuoi_data :: Lens' (TestUTxOInfo era) (StrictMaybe (Alonzo.DataHash (Crypto era), (Alonzo.Data era)))
-- tuoi_data a2fb s = (\b -> s {_tuoi_data = b}) <$> a2fb (_tuoi_data s)
-- {-# INLINE tuoi_data #-}

data EraElaboratorTxAccum era = EraElaboratorTxAccum
  { _eesScripts :: ElaboratedScriptsCache era,
    _eetaDats :: Alonzo.TxDats era
  }

instance Typeable era => Semigroup (EraElaboratorTxAccum era) where
  EraElaboratorTxAccum a b <> EraElaboratorTxAccum a' b' =
    EraElaboratorTxAccum
      (a <> a')
      (b <> b')

instance Typeable era => Monoid (EraElaboratorTxAccum era) where
  mempty = EraElaboratorTxAccum mempty mempty

-- under normal circumstances, this arises during elaboration, at which time the
-- era is already known
data TestRedeemer era = TestRedeemer
  { _trdmrPtr :: !(Either (Alonzo.RdmrPtr) (Alonzo.ScriptPurpose (Crypto era))),
    _trdmData :: !PlutusTx.Data,
    _trdmExUnits :: !ExUnits
  }
  deriving stock (Eq, Generic)
  deriving (Show) via Quiet (TestRedeemer era)

elaborateModelRedeemer ::
  forall era.
  ( HasField "inputs" (Core.TxBody era) (Set.Set (Shelley.TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Shelley.Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq.StrictSeq (DCert (Crypto era))),
    HasField "minted" (Core.TxBody era) (Set.Set (ScriptHash (Crypto era)))
  ) =>
  Core.TxBody era ->
  TestRedeemer era ->
  StrictMaybe (Alonzo.RdmrPtr, (Alonzo.Data era, Alonzo.ExUnits))
elaborateModelRedeemer tx (TestRedeemer scriptPurpose dat exUnits) =
  (,) <$> (either pure (Alonzo.rdptr @era tx) scriptPurpose) <*> pure (Alonzo.Data dat, exUnits)

eesUnusedKeyPairs :: Functor f => (Word64 -> f Word64) -> EraElaboratorState era -> f (EraElaboratorState era)
eesUnusedKeyPairs a2fb s = (\b -> s {_eesUnusedKeyPairs = b}) <$> a2fb (_eesUnusedKeyPairs s)

eesKeys :: Lens' (EraElaboratorState era) (Map.Map (ModelCredential 'Witness (ScriptFeature (EraFeatureSet era))) (TestCredentialInfo 'Witness era))
eesKeys a2fb s = (\b -> s {_eesKeys = b}) <$> a2fb (_eesKeys s)

eesCurrentEpoch :: Functor f => (EpochNo -> f EpochNo) -> EraElaboratorState era -> f (EraElaboratorState era)
eesCurrentEpoch a2fb s = (\b -> s {_eesCurrentEpoch = b}) <$> a2fb (_eesCurrentEpoch s)

eesStakeCredentials :: Lens' (EraElaboratorState era) (Map.Map (Credential 'Witness (Crypto era)) (ModelCredential 'Witness (EraScriptFeature era)))
eesStakeCredentials a2fb s = (\b -> s {_eesStakeCredentials = b}) <$> a2fb (_eesStakeCredentials s)

eesUTxOs :: Lens' (EraElaboratorState era) (Map.Map ModelUTxOId (TestUTxOInfo era))
eesUTxOs a2fb s = (\b -> s {_eesUTxOs = b}) <$> a2fb (_eesUTxOs s)

eesCurrentSlot :: Lens' (EraElaboratorState era) SlotNo
eesCurrentSlot a2fb s = (\b -> s {_eesCurrentSlot = b}) <$> a2fb (_eesCurrentSlot s)

eesStats :: Lens' (EraElaboratorState era) (EraElaboratorStats era)
eesStats a2fb s = (\b -> s {_eesStats = b}) <$> a2fb (_eesStats s)

eesModel :: Lens' (EraElaboratorState era) (ModelLedger (EraFeatureSet era))
eesModel a2fb s = (\b -> s {_eesModel = b}) <$> a2fb (_eesModel s)

data TestAddrInfo era = TestAddrInfo
  { _taiPmt :: TestCredentialInfo 'Payment era,
    _taiStk :: TestCredentialInfo 'Staking era
  }
  deriving (Generic)

getTaiAddr :: TestAddrInfo era -> Addr (Crypto era)
getTaiAddr (TestAddrInfo (TestCredentialInfo pmt _) (TestCredentialInfo stk _)) =
  Addr Testnet pmt (StakeRefBase stk)

deriving instance Eq (Core.Script era) => Eq (TestAddrInfo era)

deriving instance Ord (Core.Script era) => Ord (TestAddrInfo era)

deriving via (Quiet (TestAddrInfo era)) instance (C.Crypto (Crypto era), Show (Core.Script era)) => Show (TestAddrInfo era)

data TestCredentialInfo k era = TestCredentialInfo
  { _tciCred :: Credential k (Crypto era),
    _tciKey :: TestKey k era
  }
  deriving (Generic)

deriving instance Eq (Core.Script era) => Eq (TestCredentialInfo k era)

deriving instance Ord (Core.Script era) => Ord (TestCredentialInfo k era)

deriving via (Quiet (TestCredentialInfo k era)) instance (C.Crypto (Crypto era), Show (Core.Script era)) => Show (TestCredentialInfo k era)

instance HasKeyRole TestCredentialInfo

data TestKey k era
  = TestKey_Keyed (TestKeyKeyed k (Crypto era))
  | TestKey_Script (TestKeyScript era)

deriving instance Eq (Core.Script era) => Eq (TestKey k era)

deriving instance Ord (Core.Script era) => Ord (TestKey k era)

deriving instance (C.Crypto (Crypto era), Show (Core.Script era)) => Show (TestKey k era)

instance HasKeyRole TestKey

data TestKeyKeyed (k :: KeyRole) c = TestKeyKeyed
  { _tkkSeed :: RawSeed,
    _tkkKeyPair :: KeyPair k c,
    _tkkKeyHash :: KeyHash k c,
    _tkkVrfHash :: Hash.Hash (C.HASH c) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF c))
  }
  deriving (Generic)

instance Eq (TestKeyKeyed k c) where (==) = (==) `on` _tkkKeyHash

instance Ord (TestKeyKeyed k c) where compare = compare `on` _tkkKeyHash

deriving via (Quiet (TestKeyKeyed k c)) instance C.Crypto c => Show (TestKeyKeyed k c)

data TestKeyScript era = TestKeyScript (Core.Script era) (ScriptHash (Crypto era))

deriving instance Eq (Core.Script era) => Eq (TestKeyScript era)

deriving instance Ord (Core.Script era) => Ord (TestKeyScript era)

deriving instance Show (Core.Script era) => Show (TestKeyScript era)

elaborateKeyPair ::
  forall era k proxy.
  ElaborateEraModel era =>
  proxy era ->
  Word64 ->
  (TestKeyKeyed k (Crypto era))
elaborateKeyPair _ seed =
  let rawSeed = RawSeed seed seed seed seed seed
      pmtKey =
        uncurry KeyPair . swap $
          mkKeyPair @(Crypto era) rawSeed
      pmtHash = hashKey $ vKey pmtKey

      vrf = hashVerKeyVRF . snd . mkVRFKeyPair $ rawSeed
   in TestKeyKeyed rawSeed pmtKey pmtHash vrf

elaborateModelCredential ::
  forall era k proxy.
  ElaborateEraModel era =>
  proxy era ->
  Word64 ->
  ModelCredential k (EraScriptFeature era) ->
  TestCredentialInfo k era
elaborateModelCredential proxy seed (ModelKeyHashObj _) =
  let pmt@(TestKeyKeyed {_tkkKeyHash = pmtHash}) = elaborateKeyPair proxy seed
   in TestCredentialInfo (KeyHashObj pmtHash) (TestKey_Keyed pmt)
elaborateModelCredential proxy _ (ModelScriptHashObj ms) =
  let realPolicy = makePlutusScript proxy (SupportsPlutus $ elaborateModelScript ms)
      scriptHash = hashScript @era realPolicy
   in TestCredentialInfo
        (ScriptHashObj scriptHash)
        (TestKey_Script $ TestKeyScript realPolicy scriptHash)

-- | get the TestKeyPair for a ModelAddress
getCredentialForImpl ::
  forall r m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential r (EraScriptFeature era) ->
  m (TestCredentialInfo r era)
getCredentialForImpl proxy mCred = do
  st <- use id
  let mCred' = coerceKeyRole' mCred
  case Map.lookup mCred' (_eesKeys st) of
    Just k -> pure $ coerceKeyRole k
    Nothing -> do
      unusedKeyPairId <- eesUnusedKeyPairs <<%= succ
      let k = elaborateModelCredential proxy unusedKeyPairId mCred'
      eesKeys . at mCred' .= Just k
      eesStakeCredentials . at (_tciCred k) .= Just mCred'
      pure $ coerceKeyRole k

-- get the keypair for a credential known to be keyed
getTestKeyKeyedFor ::
  forall r m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential r ('TyScriptFeature 'False 'False) ->
  m (TestKeyKeyed r (Crypto era))
getTestKeyKeyedFor proxy cred =
  getKeyFor proxy (liftModelCredential cred) >>= \case
    TestKey_Keyed kp -> pure kp
    TestKey_Script _ -> error "elaborated wrong kind of key"

getKeyPairFor ::
  forall r m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential r ('TyScriptFeature 'False 'False) ->
  m (KeyPair r (Crypto era))
getKeyPairFor proxy cred = _tkkKeyPair <$> getTestKeyKeyedFor proxy cred

getCredentialFor ::
  forall r m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential r (EraScriptFeature era) ->
  m (Credential r (Crypto era))
getCredentialFor proxy mCred = _tciCred <$> getCredentialForImpl proxy mCred

getKeyFor ::
  forall r m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential r (EraScriptFeature era) ->
  m (TestKey r era)
getKeyFor proxy mCred = _tciKey <$> getCredentialForImpl proxy mCred

getKeyPairForImpl ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelAddress (EraScriptFeature era) ->
  m (TestAddrInfo era)
getKeyPairForImpl proxy (ModelAddress pmt stk) =
  TestAddrInfo
    <$> getCredentialForImpl proxy pmt
    <*> getCredentialForImpl proxy stk

getPmtKeyFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelAddress (EraScriptFeature era) ->
  m (TestKey 'Payment era)
getPmtKeyFor proxy mAddr = _tciKey . _taiPmt <$> getKeyPairForImpl proxy mAddr

getStakeKeyFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'Staking (EraScriptFeature era) ->
  m (TestKey 'Staking era)
getStakeKeyFor proxy mAddr = _tciKey <$> getCredentialForImpl proxy mAddr

-- | get the Addr for a ModelAddress
getAddrFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelAddress (EraScriptFeature era) ->
  m (Addr (Crypto era))
getAddrFor proxy mAddr = getTaiAddr <$> getKeyPairForImpl proxy mAddr

-- | get the Addr for a ModelAddress
getStakingKeyHashFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'Staking ('TyScriptFeature 'False 'False) ->
  m (KeyHash 'Staking (Crypto era))
getStakingKeyHashFor proxy maddr =
  let f (TestKey_Keyed (TestKeyKeyed {_tkkKeyHash = k})) = k
      f _ = error $ ("unexpected script address:" <> show maddr)
   in f . _tciKey <$> getCredentialForImpl proxy (liftModelCredential maddr)

getTestPoolId ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelPoolId ->
  m (KeyHash 'StakePool (Crypto era))
getTestPoolId proxy (ModelPoolId maddr) = do
  fmap _tkkKeyHash $ getTestKeyKeyedFor proxy maddr

getTestPoolVrf ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'StakePool ('TyScriptFeature 'False 'False) ->
  m (Hash.Hash (C.HASH (Crypto era)) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF (Crypto era))))
getTestPoolVrf proxy maddr = do
  fmap _tkkVrfHash $ getTestKeyKeyedFor proxy maddr

getGenesisKeyHash ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'Genesis ('TyScriptFeature 'False 'False) ->
  m
    ( KeyHash 'Genesis (Crypto era)
    )
getGenesisKeyHash proxy maddr =
  fmap _tkkKeyHash $ getTestKeyKeyedFor proxy maddr

getGenesisDelegateKeyHash ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'GenesisDelegate ('TyScriptFeature 'False 'False) ->
  m
    ( KeyHash 'GenesisDelegate (Crypto era),
      Hash.Hash (C.HASH (Crypto era)) (Cardano.Crypto.VRF.Class.VerKeyVRF (C.VRF (Crypto era)))
    )
getGenesisDelegateKeyHash proxy maddr = do
  fmap (_tkkKeyHash &&& _tkkVrfHash) $ getTestKeyKeyedFor proxy maddr

-- | get the StakePool hash for a ModelAddress
getScriptWitnessFor ::
  forall m era proxy.
  ( MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelCredential 'Witness (EraScriptFeature era) ->
  m (KeyHash 'Witness (Crypto era))
getScriptWitnessFor proxy maddr0 = case filterModelCredential (eraFeatureSet (Proxy @era)) maddr0 of
  Nothing -> error $ "unexpectedly unupgradable address: " <> show maddr0
  Just maddr ->
    let f (TestKey_Keyed (TestKeyKeyed {_tkkKeyHash = x})) = x
        f (TestKey_Script _) = error $ "unexpected script addr in timelock" <> show maddr0
     in coerceKeyRole @_ @_ @(Crypto era) . f . _tciKey <$> getCredentialForImpl proxy maddr

-- instance Default (EraElaboratorState era) where
--   def =

tellMintWitness ::
  ( ElaborateEraModel era,
    MonadState (NewEpochState era, EraElaboratorState era) m
  ) =>
  proxy era ->
  ScriptHash (Crypto era) ->
  Core.Script era ->
  CPS.WriterT (EraElaboratorTxAccum era) m ()
tellMintWitness _ policyHash realPolicy = do
  CPS.tell $
    mempty
      { _eesScripts = ElaboratedScriptsCache $ Map.singleton policyHash realPolicy
      }

noScriptAction ::
  Applicative m =>
  proxy era ->
  ScriptHash (Crypto era) ->
  Core.Script era ->
  m ()
noScriptAction _ _ _ = pure ()

lookupModelValue ::
  forall era valF m.
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  (Proxy era -> ScriptHash (Crypto era) -> Core.Script era -> m ()) ->
  ModelValueVars (EraFeatureSet era) valF ->
  m (ElaborateValueType valF (Crypto era))
lookupModelValue scriptAction = \case
  ModelValue_MA (modelPolicy, assetName) -> case reifyValueConstraint @era of
    ExpectedValueTypeC_MA -> do
      realPolicy :: Core.Script era <- State.state $ State.runState $ zoom _2 $ State.StateT $ Identity . elaborateScript (Proxy :: Proxy era) modelPolicy
      let policyHash = hashScript @era realPolicy
      scriptAction (Proxy @era) policyHash realPolicy
      pure $ valueFromList 0 $ [(PolicyID policyHash, assetName, 1)]

mkTxOut ::
  forall m era.
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    Except.MonadError (ElaborateBlockError era) m,
    Era era,
    UsesTxOut era,
    ElaborateEraModel era
  ) =>
  ModelUTxOId ->
  ModelTxOut (EraFeatureSet era) ->
  m (Core.TxOut era)
mkTxOut mutxoId (ModelTxOut mAddr (ModelValue mValue) mDat) = (=<<) Except.liftEither $
  State.state $
    State.runState $
      Except.runExceptT $ do
        addr <- zoom _2 $ getAddrFor (Proxy :: Proxy era) mAddr
        val :: Core.Value era <- evalModelValue (lookupModelValue noScriptAction) mValue
        let txo = makeTxOut (Proxy :: Proxy era) addr val
            (dat, txo') = case mDat of
              NoPlutusSupport () -> (SNothing, txo)
              SupportsPlutus Nothing -> (SNothing, txo)
              SupportsPlutus (Just dat') ->
                let d = Alonzo.Data dat'
                    dh = (Alonzo.hashData @era $ d)
                 in (SJust (dh, d), makeExtendedTxOut (Proxy @era) txo $ SupportsPlutus dh)
        _2 . eesUTxOs . at mutxoId .= Just (TestUTxOInfo Nothing mAddr dat)
        pure txo'

mkTxIn ::
  forall m era.
  ( MonadState (EraElaboratorState era) m,
    CPS.MonadWriter (EraElaboratorTxAccum era) m,
    ElaborateEraModel era
  ) =>
  ModelUTxOId ->
  m (Set.Set (Shelley.TxIn (Crypto era)))
mkTxIn moid = do
  mutxo <- use $ eesUTxOs . at moid
  case mutxo of
    -- TODO: handle missing txnIds more gracefully?
    Nothing -> pure mempty
    Just (TestUTxOInfo mutxo' mAddr mDat) -> do
      getPmtKeyFor (Proxy :: Proxy era) mAddr >>= tellScriptWitness

      for_ mDat $ \(k, v) ->
        CPS.tell $
          mempty
            { _eetaDats = Alonzo.TxDats $ Map.singleton k v
            }

      txins <- case mutxo' of
        Just txin -> pure (Set.singleton txin)
        Nothing -> error ("no UTXO! " <> show moid)
      pure txins

-- | accumulate a needed script witness while elaborating a ModelTx.
tellScriptWitness ::
  forall kr m era.
  CPS.MonadWriter (EraElaboratorTxAccum era) m =>
  TestKey kr era ->
  m ()
tellScriptWitness = \case
  TestKey_Keyed _ -> pure ()
  TestKey_Script (TestKeyScript realPolicy policyHash) ->
    CPS.tell $
      mempty
        { _eesScripts = ElaboratedScriptsCache $ Map.singleton policyHash realPolicy
        }

-- | return all accumulated witnesses.
elaborateWitnesses ::
  forall kr proxy era.
  ( DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody),
    C.Crypto (Crypto era)
  ) =>
  proxy era ->
  [KeyPair kr (Crypto era)] ->
  SafeHash (Crypto era) Shelley.EraIndependentTxBody ->
  (Set.Set (Shelley.WitVKey 'Witness (Crypto era)))
elaborateWitnesses _ witness bodyHash = flip foldMap witness $ \keyP ->
  Set.singleton $ UTxO.makeWitnessVKey bodyHash keyP

-- | lens to focus the ledger state from that used by ApplyBlock to that used by
-- ApplyTx
mempoolState :: Functor f => (MempoolState era -> f (MempoolState era)) -> (NewEpochState era -> f (NewEpochState era))
mempoolState = \a2b s ->
  let nesEs = LedgerState.nesEs s
      esLState = LedgerState.esLState nesEs

      mkNES (utxoState, delegationState) =
        s
          { LedgerState.nesEs =
              nesEs
                { LedgerState.esLState =
                    esLState
                      { LedgerState._utxoState = utxoState,
                        LedgerState._delegationState = delegationState
                      }
                }
          }
   in mkNES <$> a2b (mkMempoolState s)
{-# INLINE mempoolState #-}

data ElaborateApplyTxError era = ElaborateApplyTxError
  { _eateTx :: !(Core.Tx era),
    _eateMtx :: !(ModelTx (EraFeatureSet era)),
    _eateNes :: !(NewEpochState era),
    _eateEes :: !(EraElaboratorState era),
    _eateErr :: !(ApplyTxError era)
  }

deriving instance
  ( Show (Core.Tx era),
    LedgerState.TransUTxOState Show era,
    Show (Core.Script era),
    Show (ApplyTxError era)
  ) =>
  Show (ElaborateApplyTxError era)

data ElaborateBlockError era
  = ElaborateBlockError_ApplyTx (ElaborateApplyTxError era) -- (Core.Tx era) (ApplyTxError era)
  deriving (Generic)

instance NFData (ElaborateBlockError era) where
  rnf = rwhnf

deriving instance
  ( Show (ApplyTxError era),
    Show (Core.Tx era),
    LedgerState.TransUTxOState Show era,
    Show (Core.Script era)
  ) =>
  Show (ElaborateBlockError era)

data TxBodyArguments era = TxBodyArguments
  { -- | ttl
    _txBodyArguments_ttl :: !SlotNo,
    -- | fee
    _txBodyArguments_fee :: !Coin,
    -- | inputs
    _txBodyArguments_inputs :: !(Set.Set (Shelley.TxIn (Crypto era))),
    -- | outputs
    _txBodyArguments_outputs :: !(StrictSeq.StrictSeq (Core.TxOut era)),
    -- | Deleg certs.
    _txBodyArguments_delegCerts :: !(StrictSeq.StrictSeq (DCert (Crypto era))),
    -- | withdrawals
    _txBodyArguments_withdrawals :: !(Shelley.Wdrl (Crypto era)),
    -- | mint
    _txBodyArguments_mint :: !(IfSupportsMint () (Core.Value era) (EraValueFeature era)),
    _txBodyArguments_redeemers :: !(IfSupportsPlutus () (StrictMaybe (Alonzo.Redeemers era, Alonzo.TxDats era)) (EraScriptFeature era)),
    -- | collateral
    _txBodyArguments_collateral :: !(IfSupportsPlutus () (Set.Set (Shelley.TxIn (Crypto era))) (EraScriptFeature era)),
    _txBodyArguments_isValid :: !(IfSupportsPlutus () IsValid (EraScriptFeature era))
  }

data TxWitnessArguments era = TxWitnessArguments
  { _txWitnessArguments_vkey :: !(Set.Set (Shelley.WitVKey 'Witness (Crypto era))),
    _txWitnessArguments_scripts ::
      !( IfSupportsScript
           ()
           (Map.Map (ScriptHash (Crypto era)) (Core.Script era))
           (EraScriptFeature era)
       ),
    _txWitnessArguments_redeemers ::
      !( IfSupportsPlutus
           ()
           (Alonzo.Redeemers era, Alonzo.TxDats era)
           (EraScriptFeature era)
       ),
    _txWitnessArguments_isValid :: !(IfSupportsPlutus () IsValid (EraScriptFeature era))
  }

mkTxWitnessArguments ::
  forall era m.
  ( Monad m,
    C.Crypto (Crypto era),
    ElaborateEraModel era,
    HasField "certs" (Core.TxBody era) (StrictSeq.StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set.Set (Shelley.TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Shelley.Wdrl (Crypto era)),
    DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody)
  ) =>
  Set.Set ModelUTxOId ->
  NewEpochState era ->
  [KeyPair 'Witness (Crypto era)] ->
  ElaboratedScriptsCache era ->
  [TestRedeemer era] ->
  Alonzo.TxDats era ->
  TxBodyArguments era ->
  m (Core.TxBody era, TxWitnessArguments era)
mkTxWitnessArguments mtxInputs nes pendingWits (ElaboratedScriptsCache pendingScripts) pendingRedeemers pendingDats txBodyArguments = do
  let fakeTxBody = makeTxBody @era nes $ txBodyArguments
  redeemers <- case reifySupportsPlutus (Proxy @(EraScriptFeature era)) of
    NoPlutusSupport () -> pure (NoPlutusSupport ())
    SupportsPlutus () -> do
      case traverse (elaborateModelRedeemer fakeTxBody) pendingRedeemers of
        SNothing -> error $ "cant elaborate ptr ; " <> show pendingRedeemers <> show mtxInputs
        SJust xs -> do
          let redeemers = Alonzo.Redeemers $ Map.fromList xs
          pure (SupportsPlutus (redeemers, pendingDats))
  let bodyHash = hashAnnotated realTxBody
      realTxBody = case redeemers of
        SupportsPlutus (r, _)
          | not (Alonzo.nullRedeemers r) ->
            makeTxBody @era nes $
              txBodyArguments
                { _txBodyArguments_redeemers = mapSupportsFeature SJust redeemers
                }
          | otherwise -> fakeTxBody
        NoPlutusSupport () -> fakeTxBody
      witVKey = elaborateWitnesses (Proxy :: Proxy era) pendingWits bodyHash

      scripts :: IfSupportsScript () (Map.Map (ScriptHash (Crypto era)) (Core.Script era)) (EraScriptFeature era)
      scripts = case reifyScriptFeature (Proxy @(EraScriptFeature era)) of
        ScriptFeatureTag_None -> NoScriptSupport ()
        tag@ScriptFeatureTag_Simple -> SupportsScript tag pendingScripts
        tag@ScriptFeatureTag_PlutusV1 -> SupportsScript tag pendingScripts

  pure $
    (,) realTxBody $
      TxWitnessArguments
        { _txWitnessArguments_vkey = witVKey,
          _txWitnessArguments_scripts = scripts,
          _txWitnessArguments_redeemers = redeemers,
          _txWitnessArguments_isValid = _txBodyArguments_isValid txBodyArguments
        }

type EraValueFeature era = ValueFeature (EraFeatureSet era)

type EraScriptFeature era = ScriptFeature (EraFeatureSet era)

elaborateScriptPurpose ::
  forall era proxy.
  ElaborateEraModel era =>
  proxy era ->
  ModelScriptPurpose (EraFeatureSet era) ->
  (EraElaboratorState era) ->
  ( (ScriptPurpose (Crypto era)),
    (EraElaboratorState era)
  )
elaborateScriptPurpose proxy =
  State.runState . \case
    ModelScriptPurpose_Minting ms -> do
      realPolicy :: Core.Script era <- State.state $ elaborateScript proxy ms
      let policyHash :: ScriptHash (Crypto era)
          policyHash = hashScript @era realPolicy
      pure $ Minting $ PolicyID policyHash
    ModelScriptPurpose_Spending mui -> do
      ui' <- use (eesUTxOs . at mui)
      case ui' >>= _tuoi_txid of
        Nothing -> error ("missing UTxO: " <> show mui)
        Just ui -> pure $ Spending $ ui
    ModelScriptPurpose_Rewarding mAddr -> do
      stakeCredential <- getCredentialFor proxy mAddr
      pure $ Rewarding $ RewardAcnt Testnet stakeCredential
    ModelScriptPurpose_Certifying mcert -> Certifying <$> State.state (elaborateDCert proxy mcert)

tellDCertScriptWits ::
  ( CPS.MonadWriter (EraElaboratorTxAccum era) m,
    MonadState (EraElaboratorState era) m,
    ElaborateEraModel era
  ) =>
  proxy era ->
  ModelDCert (EraFeatureSet era) ->
  m ()
tellDCertScriptWits proxy mdc = do
  case mdc of
    ModelCertDeleg mcd -> case mcd of
      ModelRegKey {} -> pure ()
      ModelDeRegKey {} -> pure ()
      ModelDelegate (ModelDelegation mdelegator _) ->
        getStakeKeyFor proxy mdelegator >>= tellScriptWitness
      ModelDCertGenesis {} -> pure () -- can't contain scripts
      ModelDCertMir {} -> pure () -- can't contain scripts
    ModelCertPool mcp -> case mcp of
      ModelRegPool (ModelPoolParams {}) -> pure ()
      ModelRetirePool {} -> pure ()

-- TODO: this is a bit overconstrained, we probably want a more constraints
-- based approach using ValueFromList
type family ElaborateValueType (valF :: TyValueExpected) crypto :: Type where
  ElaborateValueType 'ExpectAdaOnly _ = Coin
  ElaborateValueType 'ExpectAnyOutput crypto = Cardano.Ledger.Mary.Value.Value crypto

eraFeatureSet ::
  forall era proxy.
  ElaborateEraModel era =>
  proxy era ->
  FeatureTag (EraFeatureSet era)
eraFeatureSet _ = reifyRequiredFeatures (Proxy :: Proxy (EraFeatureSet era))

-- | Ledger Eras that can be modeled.
class
  ( ElaborateValueType (EraValueFeature era) (Crypto era) ~ Core.Value era,
    KnownRequiredFeatures (EraFeatureSet era),
    ValidateScript era
  ) =>
  ElaborateEraModel era
  where
  -- | the features that can be used in the model for this era.
  type EraFeatureSet era :: FeatureSet

  -- | refies the constraints neccessary to instantiate a 'Core.Value' from
  -- a 'ModelValue'
  reifyValueConstraint :: ExpectedValueTypeC era
  default reifyValueConstraint ::
    ( ValueFromList (Core.Value era) (Crypto era),
      EraValueFeature era ~ 'ExpectAnyOutput
    ) =>
    ExpectedValueTypeC era
  reifyValueConstraint = ExpectedValueTypeC_MA

  -- | Apply a ModelBlock to a specific era's ledger.
  elaborateBlock ::
    Globals ->
    ModelBlock (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateBlock ::
    ( ApplyBlock era,
      ApplyTx era,
      UsesValue era,
      Show (Core.Script era),
      LedgerState.TransUTxOState Show era
    ) =>
    Globals ->
    ModelBlock (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateBlock globals =
    State.runState . Except.runExceptT . \case
      mblock@(ModelBlock mslot mtxSeq) -> do
        currentEpoch <- use $ _2 . eesCurrentEpoch
        let slot = runIdentity (epochInfoFirst ei currentEpoch) + mslot
            ei = epochInfo globals
            ttl = succ slot

        unless (currentEpoch == runIdentity (epochInfoEpoch ei slot)) $ error $ "model slot out of range: " <> show mslot
        _2 . eesCurrentSlot .= slot
        -- tick the model
        lift $ zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 slot)
        txSeq ::
          [Core.Tx era] <-
          for mtxSeq $ \tx -> Except.ExceptT $ State.state $ elaborateTx (Proxy :: Proxy era) globals ttl tx

        mempoolEnv <- (\(nes0, _) -> mkMempoolEnv nes0 slot) <$> get

        -- apply the transactions.

        ifor_ (zip txSeq mtxSeq) $ \txIx (tx, mtx) -> do
          (nes0, ems0) <- get
          (mps', _) <- liftApplyTxError (ElaborateApplyTxError tx mtx nes0 ems0) $ applyTx globals mempoolEnv {Cardano.Ledger.Shelley.Rules.Ledger.ledgerIx = toEnum txIx} (view mempoolState nes0) tx

          let nes1 = set mempoolState mps' nes0
              adaSupply = totalAdaES (LedgerState.nesEs nes1)
          unless (adaSupply == Coin (toInteger $ maxLovelaceSupply globals)) $
            _2 . eesStats
              <>= mempty
                { _eeStats_adaConservedErrors = [(globals, mempoolEnv, (view mempoolState nes0), tx)]
                }
          put (nes1, ems0)

        _2 . eesModel %= execModelM (applyModelBlock mblock) globals

        compareModelLedger (cmsError mblock)

        pure ()

  -- | get a NewEpochState from genesis conditions.
  elaborateInitialState ::
    Globals ->
    ModelGenesis (EraFeatureSet era) ->
    ( NewEpochState era,
      EraElaboratorState era
    )
  default elaborateInitialState ::
    ( Show (Core.Script era),
      Show (Core.Tx era),
      LedgerState.TransUTxOState Show era,
      UsesValue era
    ) =>
    Globals ->
    ModelGenesis (EraFeatureSet era) ->
    ( NewEpochState era,
      EraElaboratorState era
    )
  elaborateInitialState globals mgen@(ModelGenesis mpp genDelegs genesisAccounts) = go ees
    where
      ees =
        EraElaboratorState
          { _eesUnusedKeyPairs = 1,
            _eesKeys = Map.empty,
            _eesCurrentEpoch = 0,
            _eesCurrentSlot = 0,
            _eesStakeCredentials = Map.empty,
            _eesUTxOs = Map.empty,
            _eesStats = mempty,
            _eesModel = mkModelLedger globals mgen
          }

      go = (State.execState (compareModelLedger $ cmsError genesisAccounts) .) $
        State.runState $ do
          utxo0 <- fmap (Map.fromListWith (\_ _ -> error "addr collision") . toList) $
            ifor genesisAccounts $ \oid (mAddr, coins) -> do
              addr <- getAddrFor (Proxy :: Proxy era) mAddr
              eesUTxOs . at oid .= Just (TestUTxOInfo (Just (initialFundsPseudoTxIn addr)) mAddr SNothing)
              pure (addr, coins)
          genDelegs0 <- fmap (Map.fromListWith (\_ _ -> error "gen delegate collision")) $
            for (Map.toList genDelegs) $ \(mgkh, mvkh) -> do
              gkh <- getGenesisKeyHash (Proxy :: Proxy era) mgkh
              (vkh, vrf) <- getGenesisDelegateKeyHash (Proxy :: Proxy era) mvkh
              pure (gkh, GenDelegPair vkh vrf)

          pure $ makeInitialState globals mpp genDelegs0 utxo0

  makeInitialState ::
    Globals ->
    ModelPParams (EraFeatureSet era) ->
    Map.Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)) ->
    Map.Map (Addr (Crypto era)) Coin ->
    NewEpochState era

  makeTimelockScript ::
    proxy era ->
    IfSupportsTimelock Void (Timelock (Crypto era)) (EraScriptFeature era) ->
    Core.Script era

  makePlutusScript ::
    proxy era ->
    IfSupportsPlutus Void (Alonzo.Script era) (EraScriptFeature era) ->
    Core.Script era

  makeExtendedTxOut ::
    proxy era ->
    Core.TxOut era ->
    IfSupportsPlutus Void (Alonzo.DataHash (Crypto era)) (EraScriptFeature era) ->
    Core.TxOut era

  elaborateScript ::
    proxy era ->
    ModelScript (EraScriptFeature era) ->
    (EraElaboratorState era) ->
    (Core.Script era, EraElaboratorState era)
  default elaborateScript ::
    proxy era ->
    ModelScript (EraScriptFeature era) ->
    (EraElaboratorState era) ->
    (Core.Script era, EraElaboratorState era)
  elaborateScript proxy ms0 = State.runState $ case ms0 of
    ModelScript_PlutusV1 ms ->
      pure $ makePlutusScript proxy (SupportsPlutus $ elaborateModelScript ms)
    ModelScript_Timelock ms -> do
      x <- elaborateModelTimelock (getScriptWitnessFor (Proxy :: Proxy era) . liftModelCredential) ms
      pure $ makeTimelockScript proxy $ SupportsTimelock x

  -- | Construct a TxBody from some elaborated inputs.
  makeTxBody ::
    NewEpochState era ->
    TxBodyArguments era ->
    Core.TxBody era

  -- | apply a single Model transaction to a specific eras ledger
  elaborateTx ::
    proxy era ->
    Globals ->
    -- | ttl
    SlotNo ->
    ModelTx (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (Core.Tx era),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateTx ::
    ( DSIGN.Signable (C.DSIGN (Crypto era)) (Hash.Hash (C.HASH (Crypto era)) Shelley.EraIndependentTxBody),
      UsesTxOut era,
      HasField "inputs" (Core.TxBody era) (Set.Set (Shelley.TxIn (Crypto era))),
      HasField "wdrls" (Core.TxBody era) (Shelley.Wdrl (Crypto era)),
      HasField "certs" (Core.TxBody era) (StrictSeq.StrictSeq (DCert (Crypto era)))
    ) =>
    proxy era ->
    Globals ->
    SlotNo ->
    ModelTx (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    ( Either (ElaborateBlockError era) (Core.Tx era),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateTx proxy _ maxTTL mtx@(ModelTx mtxInputs mtxOutputs (ModelValue mtxFee) mtxDCert mtxWdrl mtxMint mtxCollateral mtxValidity mtxWits) = State.runState $
    Except.runExceptT $ do
      (txBodyArguments, EraElaboratorTxAccum scripts dats) <- CPS.runWriterT $ do
        let mkDCerts ::
              (ModelDCert (EraFeatureSet era), a) ->
              CPS.WriterT
                (EraElaboratorTxAccum era)
                ( Except.ExceptT
                    (ElaborateBlockError era)
                    ( State.StateT
                        (NewEpochState era, EraElaboratorState era)
                        Identity
                    )
                )
                (DCert (Crypto era))
            mkDCerts (x, _) = do
              zoom _2 $ tellDCertScriptWits proxy x
              lift . lift . zoom _2 . State.state $ elaborateDCert proxy x

        outs <- for mtxOutputs $ \(oid, o) -> mkTxOut oid o
        ins :: Set.Set (Shelley.TxIn (Crypto era)) <- zoom _2 $ fmap fold $ traverse mkTxIn $ Map.keys mtxInputs
        cins <- traverseSupportsFeature (zoom _2 . fmap fold . traverse mkTxIn . Set.toList) mtxCollateral
        dcerts <- traverse mkDCerts mtxDCert
        wdrl' <-
          fmap Map.fromList $
            for (Map.toList mtxWdrl) $ \(mAddr, (ModelValue mqty, _)) -> do
              stakeCredential <- zoom _2 $ getCredentialFor proxy mAddr
              let ra = RewardAcnt Testnet stakeCredential
              -- tellWitness (Rewarding ra) stakeKey
              zoom _2 $ getStakeKeyFor proxy mAddr >>= tellScriptWitness
              qty <- evalModelValue (lookupModelValue noScriptAction) mqty
              pure (ra, (qty, (mAddr, mqty)))

        let badWdrls = Map.filter ((== Val.zero) . fst) wdrl'
            wdrl :: Map.Map (RewardAcnt (Crypto era)) Coin
            wdrl = fmap fst wdrl'

        _2 . eesStats
          <>= mempty
            { _eeStats_wdrls = Map.size mtxWdrl,
              _eeStats_badWdrls = Map.size badWdrls
            }

        fee :: Coin <- evalModelValue (lookupModelValue noScriptAction) mtxFee

        mint ::
          IfSupportsMint () (Core.Value era) (EraValueFeature era) <-
          case reifyValueConstraint @era of
            ExpectedValueTypeC_Simple -> pure $ NoMintSupport ()
            ExpectedValueTypeC_MA -> case mtxMint of
              SupportsMint {} ->
                fmap SupportsMint $
                  evalModelValue (lookupModelValue (tellMintWitness @era)) (unModelValue $ mkMintValue mtxMint)

        pure $
          TxBodyArguments
            { _txBodyArguments_ttl = maxTTL,
              _txBodyArguments_fee = fee,
              _txBodyArguments_inputs = ins,
              _txBodyArguments_outputs = StrictSeq.fromList outs,
              _txBodyArguments_delegCerts = StrictSeq.fromList dcerts,
              _txBodyArguments_withdrawals = Shelley.Wdrl wdrl,
              _txBodyArguments_mint = mint,
              _txBodyArguments_redeemers = ifSupportsPlutus (Proxy @(EraScriptFeature era)) () SNothing,
              _txBodyArguments_collateral = cins,
              _txBodyArguments_isValid = mtxValidity
            }

      let printLengths =
            error
              ( "ModelTx inputs length: " <> (show $ length mtxInputs)
                  <> ", txBodyInputs length: "
                  <> (show $ length $ _txBodyArguments_inputs txBodyArguments)
                  <> ", ins: "
                  <> (show mtxInputs)
              )
      bool (pure ()) printLengths $ (length mtxInputs) /= (length $ _txBodyArguments_inputs txBodyArguments)

      let printOutputLengths =
            error
              ( "ModelTx outputs length: " <> (show $ length mtxOutputs)
                  <> ", txBodyOutputs length: "
                  <> (show $ length $ _txBodyArguments_outputs txBodyArguments)
              )
      bool (pure ()) printOutputLengths $ (length mtxOutputs) /= (length $ _txBodyArguments_outputs txBodyArguments)

      nes <- State.gets fst
      witA <- zoom _2 $ traverse (getKeyPairFor proxy) $ Set.toList mtxWits

      rdm <- do
        certRdmr <- ifor (_mtxDCert mtx) $ \i -> \case
          (_, NoPlutusSupport ()) -> pure Nothing
          (_, SupportsPlutus Nothing) -> pure Nothing
          (_, SupportsPlutus (Just (mdat, exu))) ->
            pure $ Just $ TestRedeemer (Left $ Alonzo.RdmrPtr Alonzo.Cert $ toEnum i) mdat exu

        otherRdmr <- for (itoListOf modelTx_redeemers mtx) $ \(msp, (mdat, exu)) ->
          let mapLikeRdmr = case msp of
                ModelScriptPurpose_Certifying {} -> False
                ModelScriptPurpose_Minting {} -> True
                ModelScriptPurpose_Rewarding {} -> True
                ModelScriptPurpose_Spending {} -> True
           in if mapLikeRdmr
                then do
                  sp <- zoom _2 $ State.state $ elaborateScriptPurpose proxy msp
                  pure $ Just $ TestRedeemer (Right sp) mdat exu
                else pure Nothing

        pure $
          concat
            [ catMaybes certRdmr,
              catMaybes otherRdmr
            ]

      (realTxBody, txWitnessArguments) <- mkTxWitnessArguments (Map.keysSet mtxInputs) nes witA scripts rdm dats txBodyArguments

      let txid = TxIn.txid @era realTxBody
      ifor_ mtxOutputs $ \n (mutxoid, _) ->
        _2 . eesUTxOs . at mutxoid . _Just . tuoi_txid .= Just (Shelley.TxIn txid (fromIntegral n))
      pure $ makeTx proxy realTxBody txWitnessArguments

  -- | build a full tx from TxBody and set of witnesses.
  makeTx ::
    proxy era ->
    Core.TxBody era ->
    TxWitnessArguments era ->
    Core.Tx era

  -- | convert an expeced failure by the model to the concrete error that will
  -- be produced.
  toEraPredicateFailure ::
    ModelPredicateFailure (EraFeatureSet era) ->
    (NewEpochState era, EraElaboratorState era) ->
    (ApplyBlockTransitionError era)

  -- | apply the model New Epoch event to a specific eras ledger
  elaborateBlocksMade ::
    Globals ->
    ModelBlocksMade ->
    (NewEpochState era, EraElaboratorState era) ->
    ( BlocksMade (Crypto era),
      (NewEpochState era, EraElaboratorState era)
    )
  default elaborateBlocksMade ::
    ( ApplyBlock era,
      Show (Core.Script era),
      Show (Core.Tx era),
      LedgerState.TransUTxOState Show era,
      UsesValue era
    ) =>
    Globals ->
    ModelBlocksMade ->
    (NewEpochState era, EraElaboratorState era) ->
    ( BlocksMade (Crypto era),
      (NewEpochState era, EraElaboratorState era)
    )
  elaborateBlocksMade globals mblocksMade = State.runState $ do
    nesBefore <- use _1
    _2 . eesModel %= execModelM (applyModelBlocksMade mblocksMade) globals

    prevEpoch <- use $ _2 . eesCurrentEpoch
    epoch <- _2 . eesCurrentEpoch <%= succ
    let ei = epochInfo globals
    bs <- for (Map.toList $ unModelBlocksMade mblocksMade) $ \(maddr, n) -> do
      poolKey <- zoom _2 $ getTestPoolId (Proxy :: Proxy era) maddr
      pure (poolKey, n)

    let bs' = BlocksMade $ Map.fromList bs
    _1 %= emulateBlocksMade bs'

    let firstOfNew = runIdentity $ epochInfoFirst ei epoch

        neededSlot =
          SlotNo (randomnessStabilisationWindow globals)
            + runIdentity (epochInfoFirst ei prevEpoch)

    currentSlot <- _2 . eesCurrentSlot <<.= firstOfNew

    unless (currentSlot > neededSlot) $
      zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 (neededSlot + 1))
    zoom _1 $ State.state $ \nes0 -> ((), applyTick globals nes0 firstOfNew)

    compareModelLedger (cmsError (mblocksMade, nesBefore))

    pure bs'

  -- | convert a model deleg certificate to a real DCert
  elaborateDCert ::
    proxy era ->
    ModelDCert (EraFeatureSet era) ->
    EraElaboratorState era ->
    (DCert (Crypto era), EraElaboratorState era)
  default elaborateDCert ::
    proxy era ->
    ModelDCert (EraFeatureSet era) ->
    EraElaboratorState era ->
    (DCert (Crypto era), EraElaboratorState era)
  elaborateDCert proxy mdc = State.runState $ do
    case mdc of
      ModelCertDeleg mcd -> case mcd of
        ModelRegKey maddr ->
          DCertDeleg . RegKey
            <$> getCredentialFor proxy maddr
        ModelDeRegKey maddr ->
          DCertDeleg . DeRegKey
            <$> getCredentialFor proxy maddr
        ModelDelegate (ModelDelegation mdelegator mdelegatee) -> do
          dtor <- getCredentialFor proxy mdelegator
          dtee <- getTestPoolId proxy mdelegatee
          pure $ (DCertDeleg . Delegate) $ Delegation dtor dtee
        ModelDCertGenesis (ModelGenesisDelegCert mgkh mvkh) -> do
          gkh <- getGenesisKeyHash proxy mgkh
          (vkh, vrf) <- getGenesisDelegateKeyHash proxy mvkh
          pure $ DCertGenesis $ Shelley.GenesisDelegCert gkh vkh vrf
        ModelDCertMir (ModelMIRCert mirPot mirTarget) -> case mirTarget of
          ModelSendToOppositePotMIR qty -> pure $ DCertMir $ Shelley.MIRCert mirPot (Shelley.SendToOppositePotMIR qty)
          ModelStakeAddressesMIR maddrs -> do
            addrs <- for (Map.toList maddrs) $ \(maddr, qty) -> do
              addr <- getCredentialFor proxy maddr
              pure (addr, qty)
            pure $ DCertMir $ Shelley.MIRCert mirPot $ Shelley.StakeAddressesMIR $ Map.fromList addrs
      ModelCertPool mcp -> case mcp of
        ModelRegPool (ModelPoolParams mPoolId mPoolVrf pledge cost margin mRAcnt mOwners) -> do
          poolId <- getTestPoolId proxy mPoolId
          poolVRF <- getTestPoolVrf proxy mPoolVrf
          poolOwners <- Set.fromList <$> traverse (getStakingKeyHashFor proxy) mOwners
          rAcnt <- RewardAcnt Testnet <$> getCredentialFor proxy mRAcnt
          pure $
            (DCertPool . RegPool) $
              PoolParams
                { _poolId = poolId,
                  _poolVrf = poolVRF,
                  _poolPledge = pledge,
                  _poolCost = cost,
                  _poolMargin = margin,
                  _poolRAcnt = rAcnt,
                  _poolOwners = poolOwners,
                  _poolRelays = StrictSeq.empty,
                  _poolMD = SNothing
                }
        ModelRetirePool maddr epochNo -> do
          pool <- getTestPoolId proxy maddr
          pure $ DCertPool $ RetirePool pool epochNo

liftApplyTxError ::
  Except.MonadError (ElaborateBlockError era) m =>
  (ApplyTxError era -> ElaborateApplyTxError era) -> -- Core.Tx era ->
  Except.Except (ApplyTxError era) a ->
  m a
liftApplyTxError tx = either (Except.throwError . ElaborateBlockError_ApplyTx . tx) pure . Except.runExcept

-- | simulate blocks made in the current epoch.  this functions like ApplyBlock or
-- ApplyTx, but without presenting real blocks to the ledger.  This is only
-- useful in testing the correctness of specific aspects of the ledger rather
-- than in normal use.
emulateBlocksMade ::
  forall era.
  BlocksMade (Crypto era) ->
  NewEpochState era ->
  NewEpochState era
emulateBlocksMade (BlocksMade newBlocksMade) nes@(LedgerState.NewEpochState {LedgerState.nesBcur = BlocksMade currentBlocksMade}) =
  nes {LedgerState.nesBcur = BlocksMade (Map.unionWith (+) newBlocksMade currentBlocksMade)}

-- | apply several epochs full of blocks to a ledger
elaborateBlocks_ ::
  forall era.
  (ElaborateEraModel era) =>
  Globals ->
  [ModelEpoch (EraFeatureSet era)] ->
  (NewEpochState era, EraElaboratorState era) ->
  ( Either (ElaborateBlockError era) (),
    (NewEpochState era, EraElaboratorState era)
  )
elaborateBlocks_ globals = State.runState . Except.runExceptT . traverse_ f
  where
    f (ModelEpoch blocks blocksMade) = do
      for_ blocks (Except.ExceptT . State.state . elaborateBlock globals)
      _ :: BlocksMade (Crypto era) <- lift $ State.state $ elaborateBlocksMade globals blocksMade
      pure ()

-- | get the current balance of rewards in terms of the model addresses that can
-- spend them.
observeRewards ::
  forall era.
  ModelTxId ->
  (NewEpochState era, EraElaboratorState era) ->
  Map.Map (ModelCredential 'Staking (EraScriptFeature era)) Coin
observeRewards mtxid (nes, ems) =
  let creds = _eesStakeCredentials ems
   in Map.fromList $ do
        (a, b) <- Map.toList . rewView . LedgerState._unified . LedgerState._dstate . LedgerState._delegationState . LedgerState.esLState $ LedgerState.nesEs nes
        a' <- case Map.lookup (asWitness a) creds of
          Just a' -> pure $ coerceKeyRole' a'
          Nothing -> error $ unwords ["observeRewards:", show mtxid, "can't find", show a]
        pure (a', b)

cmsError ::
  ( Show a,
    Show (Core.Script era),
    Show (Core.Tx era),
    LedgerState.TransUTxOState Show era
  ) =>
  a ->
  NewEpochState era ->
  EraElaboratorState era ->
  NonEmpty.NonEmpty (ElaboratorLedgerIssue era) ->
  x
cmsError hint nes ees issues =
  error $
    unlines
      [ "model/elaborated diverged",
        show hint,
        show (toList issues),
        show nes,
        show ees,
        show (toList issues)
      ]

-- TODO: cause this to float errors out as values in ElaborateEraModel methods.
compareModelLedger ::
  forall era m.
  ( MonadState (NewEpochState era, EraElaboratorState era) m,
    UsesValue era
  ) =>
  (NewEpochState era -> EraElaboratorState era -> NonEmpty.NonEmpty (ElaboratorLedgerIssue era) -> m ()) ->
  m ()
compareModelLedger k = do
  (nes, ees) <- State.get
  let issues = compareModelLedgerImpl (_eesModel ees) nes
  traverse_ (k nes ees) $ NonEmpty.nonEmpty issues

newtype ComposeCrypto f era = ComposeCrypto (f (Crypto era))

-- | compare a model and real implementation of a ledger data type and gather
-- the (semantically relevant) differences between them.
class CompareModelLedger a where
  type ModelFor a :: FeatureSet -> Type

  compareModel ::
    UsesValue era =>
    ModelFor a (EraFeatureSet era) ->
    a era ->
    [ElaboratorLedgerIssue era]

instance CompareModelLedger NewEpochState where
  type ModelFor LedgerState.NewEpochState = ModelNewEpochState

  compareModel
    ( ModelNewEpochState
        { _modelNewEpochState_el = mepochNo,
          _modelNewEpochState_es = mes
        }
      )
    ( LedgerState.NewEpochState
        { LedgerState.nesEL = epochNo,
          LedgerState.nesEs = es
        }
      ) = CPS.execWriter $ do
      unless (mepochNo == epochNo) $ CPS.tell [WrongEpoch mepochNo epochNo]
      let mcirc = totalPreservedAda mes
          circ = totalAdaES es
      unless (mcirc == circ) $ CPS.tell [AdaNotPreserved mcirc circ]
      CPS.tell $ compareModel mes es

instance CompareModelLedger LedgerState.EpochState where
  type ModelFor LedgerState.EpochState = ModelEpochState

  compareModel
    ( ModelEpochState
        { _modelEpochState_acnt = ModelAcnt mtreasury mreserves,
          _modelEpochState_ls = mlstate
        }
      )
    ( LedgerState.EpochState
        { LedgerState.esAccountState =
            LedgerState.AccountState
              { LedgerState._treasury = treasury,
                LedgerState._reserves = reserves
              },
          LedgerState.esLState = lstate
        }
      ) = CPS.execWriter $ do
      unless (mtreasury == treasury) $ CPS.tell [MismatchedTreasury mtreasury treasury]
      unless (mreserves == reserves) $ CPS.tell [MismatchedReserves mreserves reserves]
      CPS.tell $ compareModel mlstate lstate

instance CompareModelLedger LedgerState.LedgerState where
  type ModelFor LedgerState.LedgerState = ModelLState
  compareModel
    ( ModelLState
        { _modelLState_utxoSt = mutxo,
          _modelLState_dpstate = mdstate
        }
      )
    ( LedgerState.LedgerState
        { LedgerState._utxoState = utxo,
          LedgerState._delegationState = dstate
        }
      ) = CPS.execWriter $ do
      CPS.tell $ compareModel mutxo utxo
      CPS.tell $ compareModel mdstate $ ComposeCrypto dstate

instance CompareModelLedger (ComposeCrypto LedgerState.DPState) where
  type ModelFor (ComposeCrypto LedgerState.DPState) = ModelDPState
  compareModel
    (ModelDPState mdstate _mpstate)
    (ComposeCrypto (LedgerState.DPState dstate _pstate)) =
      compareModel mdstate (ComposeCrypto dstate) --  <> compareModel mpstate pstate

instance CompareModelLedger (ComposeCrypto LedgerState.DState) where
  type ModelFor (ComposeCrypto LedgerState.DState) = ModelDState
  compareModel
    ( ModelDState
        { _modelDState_rewards = mrewards,
          _modelDState_iRwd = mir
        }
      )
    ( ComposeCrypto
        ( LedgerState.DState
            { LedgerState._unified = unified,
              LedgerState._irwd = ir
            }
          )
      ) = CPS.execWriter $ do
      let rewards = rewView unified
      CPS.tell $ compareModel (WrappedModelInstantaneousRewards mir) (ComposeCrypto ir)
      unless (fold mrewards == fold rewards) $ CPS.tell [MismatchedRewards mrewards rewards]

newtype WrappedModelInstantaneousRewards era
  = WrappedModelInstantaneousRewards
      (ModelInstantaneousRewards era)

instance CompareModelLedger (ComposeCrypto LedgerState.InstantaneousRewards) where
  type ModelFor (ComposeCrypto LedgerState.InstantaneousRewards) = WrappedModelInstantaneousRewards
  compareModel
    ( WrappedModelInstantaneousRewards
        mir@( Comp1
                ( ModelAcnt
                    { _modelAcnt_treasury = ModelInstantaneousReward mdt (GrpMap mirt),
                      _modelAcnt_reserves = ModelInstantaneousReward mdr (GrpMap mirr)
                    }
                  )
              )
      )
    ( ComposeCrypto
        ir@( LedgerState.InstantaneousRewards
               { LedgerState.iRReserves = irr,
                 LedgerState.iRTreasury = irt,
                 LedgerState.deltaReserves = dr,
                 LedgerState.deltaTreasury = dt
               }
             )
      ) =
      let err = MismatchedImmediateRewards mir ir
       in [ err
            | toDeltaCoin mdt /= dt
                || toDeltaCoin mdr /= dr
                || fold mirr /= fold irr
                || fold mirt /= fold irt
          ]

instance CompareModelLedger LedgerState.UTxOState where
  type ModelFor LedgerState.UTxOState = ModelUTxOState
  compareModel
    ( ModelUTxOState
        { _modelUTxOState_utxo = mutxos,
          _modelUTxOState_fees = mfees,
          _modelUTxOState_deposited = mdeposited
        }
      )
    ( LedgerState.UTxOState
        { LedgerState._utxo = utxos,
          LedgerState._fees = fees,
          LedgerState._deposited = deposited
        }
      ) = CPS.execWriter $ do
      unless (mfees == fees) $ CPS.tell [MismatchedFees mfees fees]
      unless (mdeposited == deposited) $ CPS.tell [MismatchedDeposits mdeposited deposited]
      CPS.tell $ compareModel mutxos utxos

instance CompareModelLedger UTxO.UTxO where
  type ModelFor UTxO.UTxO = ModelUTxOMap

  compareModel mutxos utxos = CPS.execWriter $ do
    unless (length (_modelUTxOMap_utxos mutxos) == length (UTxO.unUTxO utxos)) $ CPS.tell [MissingUtxos mutxos utxos]

compareModelLedgerImpl ::
  UsesValue era =>
  ModelLedger (EraFeatureSet era) ->
  NewEpochState era ->
  [ElaboratorLedgerIssue era]
compareModelLedgerImpl = compareModel . _modelLedger_nes

data ElaboratorLedgerIssue era
  = MissingUtxos (ModelUTxOMap (EraFeatureSet era)) (UTxO.UTxO era)
  | WrongEpoch EpochNo EpochNo
  | MismatchedTreasury Coin Coin
  | MismatchedReserves Coin Coin
  | MismatchedFees Coin Coin
  | MismatchedDeposits Coin Coin
  | MismatchedRewards
      (Map.Map (ModelCredential 'Staking (EraScriptFeature era)) Coin)
      (Map.Map (Credential 'Staking (Crypto era)) Coin)
  | AdaNotPreserved Coin Coin
  | MismatchedImmediateRewards (ModelInstantaneousRewards (EraFeatureSet era)) (LedgerState.InstantaneousRewards (Crypto era))

deriving instance
  ( Show (Core.TxOut era),
    C.Crypto (Crypto era)
  ) =>
  Show (ElaboratorLedgerIssue era)

data ApplyBlockTransitionError era
  = ApplyBlockTransitionError_Tx (ApplyTxError era)

deriving instance
  ( Show (ApplyTxError era)
  ) =>
  Show (ApplyBlockTransitionError era)

deriving instance
  ( Eq (ApplyTxError era)
  ) =>
  Eq (ApplyBlockTransitionError era)

deriving instance
  ( Ord (ApplyTxError era)
  ) =>
  Ord (ApplyBlockTransitionError era)
