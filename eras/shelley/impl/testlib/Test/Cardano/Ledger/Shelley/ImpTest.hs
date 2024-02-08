{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestM,
  runImpTestM,
  runImpTestM_,
  evalImpTestM,
  execImpTestM,
  runImpTestGenM,
  runImpTestGenM_,
  evalImpTestGenM,
  execImpTestGenM,
  ImpTestState,
  ImpTestEnv (..),
  ImpException (..),
  ShelleyEraImp (..),
  PlutusArgs,
  ScriptTestContext,
  initShelleyImpNES,
  impWitsVKeyNeeded,
  modifyPrevPParams,
  passEpoch,
  passNEpochs,
  passTick,
  freshKeyHash,
  freshSafeHash,
  freshKeyHashVRF,
  lookupKeyPair,
  submitTx,
  submitTx_,
  submitTxAnn,
  submitTxAnn_,
  submitFailingTx,
  trySubmitTx,
  modifyNES,
  getsNES,
  getUTxO,
  impAddNativeScript,
  impAnn,
  runImpRule,
  tryRunImpRule,
  registerRewardAccount,
  lookupReward,
  registerPool,
  getRewardAccountAmount,
  withImpState,
  shelleyFixupTx,
  lookupImpRootTxOut,
  sendValueTo,
  sendCoinTo,
  dispenseIndex,
  expectRegisteredRewardAddress,
  expectNotRegisteredRewardAddress,
  expectTreasury,
  getScriptTestContext,
  updateAddrTxWits,
  addNativeScriptTxWits,
  addRootTxIn,
  fixupFees,

  -- * Logging
  logEntry,
  logToExpr,
  logStakeDistr,
  logFeeMismatch,

  -- * Combinators
  withNoFixup,

  -- * Lenses
  impScriptsL,
  impCollateralTxIdsL,
  -- We only export getters, because internal state should not be accessed during testing
  impNESG,
  impLastTickG,
  impKeyPairsG,
  impNativeScriptsG,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash, HashAlgorithm)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochSize (..),
  Globals (..),
  Inject,
  Network (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  TxIx (..),
  inject,
  mkTxIxPartial,
 )
import Cardano.Ledger.CertState (certDStateL, dsUnifiedL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..), credToText)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash,
  KeyRole (..),
  VerKeyVRF,
  hashKey,
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (AdaPots, sumAdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  asTreasuryL,
  consumed,
  curPParamsEpochStateL,
  epochStateIncrStakeDistrL,
  epochStateUMapL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  prevPParamsEpochStateL,
  produced,
  smartUTxOState,
  startStep,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.Scripts (MultiSig (..))
import Cardano.Ledger.Tools (calcMinFeeTxNativeScriptWits, integralToByteStringN)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO (..),
  txinLookup,
 )
import Cardano.Ledger.Val (Val (..))
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State.Strict (MonadState (..), gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.State.Transition (STS (..), TRC (..))
import Control.State.Transition.Trace (applySTSTest)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Default.Class (Default (..))
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stack (SrcLoc (..), getCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=))
import Numeric.Natural (Natural)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.String (renderString)
import System.Random
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkKeyHash, mkKeyPair, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs, ScriptTestContext)
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec.Core.Spec (Example (..), Params, paramsQuickCheckArgs)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (QCGen (..), mkQCGen)
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (
  Exception (..),
  SomeException (..),
  catchAny,
  catchAnyDeep,
  evaluateDeep,
  throwIO,
 )

data ImpTestState era = ImpTestState
  { impNES :: !(NewEpochState era)
  , impRootTxIn :: !(TxIn (EraCrypto era))
  , impCollateralTxIds :: ![TxIn (EraCrypto era)]
  , impFreshIdx :: !Integer
  , impKeyPairs :: !(forall k. Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
  , impNativeScripts :: !(Map (ScriptHash (EraCrypto era)) (NativeScript era))
  , impScripts :: !(Map (ScriptHash (EraCrypto era)) ScriptTestContext)
  , impLastTick :: !SlotNo
  , impGlobals :: !Globals
  , impLog :: !(Doc ())
  , impGen :: !QCGen
  }

impLogL :: Lens' (ImpTestState era) (Doc ())
impLogL = lens impLog (\x y -> x {impLog = y})

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

impNESG :: SimpleGetter (ImpTestState era) (NewEpochState era)
impNESG = impNESL

impLastTickL :: Lens' (ImpTestState era) SlotNo
impLastTickL = lens impLastTick (\x y -> x {impLastTick = y})

impLastTickG :: SimpleGetter (ImpTestState era) SlotNo
impLastTickG = impLastTickL

impRootTxInL :: Lens' (ImpTestState era) (TxIn (EraCrypto era))
impRootTxInL = lens impRootTxIn (\x y -> x {impRootTxIn = y})

impFreshIdxL :: Lens' (ImpTestState era) Integer
impFreshIdxL = lens impFreshIdx (\x y -> x {impFreshIdx = y})

impKeyPairsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
impKeyPairsG = to impKeyPairs

impNativeScriptsL :: Lens' (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsL = lens impNativeScripts (\x y -> x {impNativeScripts = y})

impNativeScriptsG :: SimpleGetter (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsG = impNativeScriptsL

impScriptsL :: Lens' (ImpTestState era) (Map (ScriptHash (EraCrypto era)) ScriptTestContext)
impScriptsL = lens impScripts (\x y -> x {impScripts = y})

impCollateralTxIdsL :: Lens' (ImpTestState era) [TxIn (EraCrypto era)]
impCollateralTxIdsL = lens impCollateralTxIds (\x y -> x {impCollateralTxIds = y})

class
  ( Show (NewEpochState era)
  , ToExpr (NewEpochState era)
  , ToExpr (Tx era)
  , ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (PParamsHKD Identity era)
  , Show (TxOut era)
  , Show (PParams era)
  , Show (StashedAVVMAddresses era)
  , Show (GovState era)
  , Eq (TxOut era)
  , Eq (PParams era)
  , Eq (StashedAVVMAddresses era)
  , Eq (GovState era)
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , STS (EraRule "LEDGER" era)
  , Signable
      (DSIGN (EraCrypto era))
      (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , EraUTxO era
  , ShelleyEraTxCert era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , EraGov era
  , BaseM (EraRule "TICK" era) ~ ShelleyBase
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , STS (EraRule "TICK" era)
  , NFData (PredicateFailure (EraRule "TICK" era))
  , NFData (StashedAVVMAddresses era)
  , NFData (Tx era)
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  , NFData (PredicateFailure (EraRule "LEDGER" era))
  , VRF.VRFAlgorithm (VRF (EraCrypto era))
  , HashAlgorithm (HASH (EraCrypto era))
  , DSIGNAlgorithm (DSIGN (EraCrypto era))
  ) =>
  ShelleyEraImp era
  where
  initImpNES :: Coin -> NewEpochState era

  -- | Try to find a sufficient number of KeyPairs that would satisfy a native script.
  -- Whenever script can't be satisfied, Nothing is returned
  impSatisfyNativeScript ::
    NativeScript era ->
    ImpTestM era (Maybe (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))))

  -- | This modifer should change not only the current PParams, but also the future
  -- PParams. If the future PParams are not updated, then they will overwrite the
  -- mofication of the current PParams at the next epoch.
  modifyPParams ::
    (PParams era -> PParams era) ->
    ImpTestM era ()
  modifyPParams f = modifyNES $ nesEsL . curPParamsEpochStateL %~ f

  fixupTx :: Tx era -> ImpTestM era (Tx era)

impLedgerEnv :: EraGov era => NewEpochState era -> ImpTestM era (LedgerEnv era)
impLedgerEnv nes = do
  slotNo <- gets impLastTick
  pure
    LedgerEnv
      { ledgerSlotNo = slotNo
      , ledgerPp = nes ^. nesEsL . curPParamsEpochStateL
      , ledgerIx = TxIx 0
      , ledgerAccount = nes ^. nesEsL . esAccountStateL
      }

-- | Modify the previous PParams in the current state with the given function. For current
-- and future PParams, use `modifyPParams`
modifyPrevPParams ::
  EraGov era =>
  (PParams era -> PParams era) ->
  ImpTestM era ()
modifyPrevPParams f = modifyNES $ nesEsL . prevPParamsEpochStateL %~ f

-- | Logs the current stake distribution
logStakeDistr :: ImpTestM era ()
logStakeDistr = do
  stakeDistr <- getsNES $ nesEsL . epochStateIncrStakeDistrL
  logEntry $ "Stake distr: " <> showExpr stakeDistr

mkHashVerKeyVRF ::
  forall era.
  ShelleyEraImp era =>
  Integer ->
  Hash (HASH (EraCrypto era)) (VerKeyVRF (EraCrypto era))
mkHashVerKeyVRF =
  VRF.hashVerKeyVRF
    . VRF.deriveVerKeyVRF
    . VRF.genKeyVRF
    . mkSeedFromBytes
    . integralToByteStringN seedSize
  where
    seedSize = fromIntegral . seedSizeDSIGN $ Proxy @(DSIGN (EraCrypto era))

freshKeyHashVRF ::
  forall era.
  ShelleyEraImp era =>
  ImpTestM era (Hash (HASH (EraCrypto era)) (VerKeyVRF (EraCrypto era)))
freshKeyHashVRF = do
  idx <- dispenseIndex
  pure $ mkHashVerKeyVRF @era $ fromIntegral idx

testKeyHash :: Crypto c => KeyHash kd c
testKeyHash = mkKeyHash (-1)

impAddr :: Crypto c => Int -> Addr c
impAddr idx =
  let KeyPair vk _ = mkKeyPair idx
   in Addr
        Testnet
        (KeyHashObj $ hashKey vk)
        StakeRefNull

initShelleyImpNES ::
  forall era.
  ( Default (StashedAVVMAddresses era)
  , ShelleyEraImp era
  ) =>
  Coin ->
  NewEpochState era
initShelleyImpNES rootCoin =
  NewEpochState
    { stashedAVVMAddresses = def
    , nesRu =
        SJust $
          startStep
            (EpochSize 432_000)
            (BlocksMade (Map.singleton testKeyHash 10))
            epochState
            (Coin 45)
            (activeSlotCoeff testGlobals)
            10
    , nesPd =
        PoolDistr $
          Map.fromList
            [
              ( testKeyHash
              , IndividualPoolStake
                  1
                  (mkHashVerKeyVRF @era 0)
              )
            ]
    , nesEs = epochState
    , nesEL = 0
    , nesBprev = BlocksMade (Map.singleton testKeyHash 10)
    , nesBcur = BlocksMade mempty
    }
  where
    rootTxId = mkTxId 0
    pp =
      emptyPParams
        & ppMinFeeAL .~ Coin 44
        & ppMinFeeBL .~ Coin 155_381
    epochState =
      EpochState
        { esAccountState =
            AccountState
              { asTreasury = Coin 10_000
              , asReserves = Coin 1_000
              }
        , esSnapshots = emptySnapShots
        , esLState =
            LedgerState
              { lsUTxOState =
                  smartUTxOState
                    emptyPParams
                    utxo
                    zero
                    zero
                    emptyGovState
                    mempty
              , lsCertState = def
              }
        , esNonMyopic = def
        }
        & prevPParamsEpochStateL .~ pp
        & curPParamsEpochStateL .~ pp
    utxo =
      UTxO
        . Map.fromList
        $ (TxIn rootTxId minBound, mkBasicTxOut (impAddr 0) $ inject rootCoin)
          : mkCollateralUTxO rootCoin

mkTxId :: Crypto c => Int -> TxId c
mkTxId idx = TxId (mkDummySafeHash Proxy idx)

mkCollateralUTxO ::
  ( EraTxOut era
  , Inject t (Value era)
  ) =>
  t ->
  [(TxIn (EraCrypto era), TxOut era)]
mkCollateralUTxO rootCoin =
  [ ( TxIn (mkTxId idx) minBound
    , mkBasicTxOut (impAddr idx) $ inject rootCoin
    )
  | idx <- [1 .. 100]
  ]

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (ShelleyEra c)
  where
  initImpNES = initShelleyImpNES

  impSatisfyNativeScript script = do
    keyPairs <- gets impKeyPairs
    let
      satisfyMOf m []
        | m <= 0 = Just mempty
        | otherwise = Nothing
      satisfyMOf m (x : xs) =
        case satisfyScript x of
          Nothing -> satisfyMOf m xs
          Just kps -> do
            kps' <- satisfyMOf (m - 1) xs
            Just $ kps <> kps'
      satisfyScript = \case
        RequireSignature keyHash -> do
          keyPair <- Map.lookup keyHash keyPairs
          Just $ Map.singleton keyHash keyPair
        RequireAllOf ss -> satisfyMOf (length ss) ss
        RequireAnyOf ss -> satisfyMOf 1 ss
        RequireMOf m ss -> satisfyMOf m ss
    pure $ satisfyScript script

  fixupTx = shelleyFixupTx

getAdaPots :: (EraGov era, EraTxOut era) => ImpTestM era AdaPots
getAdaPots = totalAdaPotsES <$> getsNES nesEsL

impWitsVKeyNeeded ::
  EraUTxO era => TxBody era -> ImpTestM era (Set.Set (KeyHash 'Witness (EraCrypto era)))
impWitsVKeyNeeded txBody = do
  ls <- getsNES (nesEsL . esLStateL)
  pure $ getWitsVKeyNeeded (ls ^. lsCertStateL) (ls ^. lsUTxOStateL . utxosUtxoL) txBody

data ImpTestEnv era = ImpTestEnv
  { iteState :: !(IORef (ImpTestState era))
  , iteDoTxFixup :: !Bool
  -- ^ This flag is used to toggle the fixing up of transactions. If it
  -- is set to False then any transaction should be submitted as-is.
  }

iteDoTxFixupL :: Lens' (ImpTestEnv era) Bool
iteDoTxFixupL = lens iteDoTxFixup (\x y -> x {iteDoTxFixup = y})

newtype ImpTestM era a = ImpTestM {unImpTestM :: ReaderT (ImpTestEnv era) (GenT IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader (ImpTestEnv era)
    )

instance MonadFail (ImpTestM era) where
  fail = assertFailure

instance MonadState (ImpTestState era) (ImpTestM era) where
  get = ImpTestM $ do
    liftIO . readIORef . iteState =<< ask
  put x = ImpTestM $ do
    liftIO . flip writeIORef x . iteState =<< ask

instance Example (ImpTestM era ()) where
  type Arg (ImpTestM era ()) = ImpTestState era

  evaluateExample impTest params =
    evaluateExample (\s -> evalImpTestM (getParamsQCGen params) s impTest) params

instance (Arbitrary a, Show a) => Example (a -> ImpTestM era ()) where
  type Arg (a -> ImpTestM era ()) = ImpTestState era

  evaluateExample impTest params =
    evaluateExample (\s -> property $ evalImpTestM (getParamsQCGen params) s . impTest) params

instance MonadGen (ImpTestM era) where
  liftGen = ImpTestM . lift . liftGen
  variant n (ImpTestM f) = ImpTestM $ ask >>= lift . variant n . runReaderT f
  sized f = ImpTestM $ do
    env <- ask
    lift $ sized (\n -> runReaderT (unImpTestM (f n)) env)
  resize n (ImpTestM f) = ImpTestM $ ask >>= lift . resize n . runReaderT f
  choose = ImpTestM . lift . choose

instance HasStatefulGen (StateGenM (ImpTestState era)) (ImpTestM era) where
  askStatefulGen = pure StateGenM

instance HasSubState (ImpTestState era) where
  type SubState (ImpTestState era) = StateGen QCGen
  getSubState = StateGen . impGen
  setSubState s (StateGen g) = s {impGen = g}

getParamsQCGen :: Params -> Maybe (QCGen, Int)
getParamsQCGen params = replay (paramsQuickCheckArgs params)

evalImpTestGenM :: ImpTestState era -> ImpTestM era b -> Gen (IO b)
evalImpTestGenM impState = fmap (fmap fst) . runImpTestGenM impState

evalImpTestM :: Maybe (QCGen, Int) -> ImpTestState era -> ImpTestM era b -> IO b
evalImpTestM qc impState = fmap fst . runImpTestM qc impState

execImpTestGenM :: ImpTestState era -> ImpTestM era b -> Gen (IO (ImpTestState era))
execImpTestGenM impState = fmap (fmap snd) . runImpTestGenM impState

execImpTestM :: Maybe (QCGen, Int) -> ImpTestState era -> ImpTestM era b -> IO (ImpTestState era)
execImpTestM qc impState = fmap snd . runImpTestM qc impState

runImpTestGenM_ :: ImpTestState era -> ImpTestM era b -> Gen (IO ())
runImpTestGenM_ impState = fmap void . runImpTestGenM impState

runImpTestM_ :: Maybe (QCGen, Int) -> ImpTestState era -> ImpTestM era b -> IO ()
runImpTestM_ qc impState = void . runImpTestM qc impState

runImpTestGenM :: ImpTestState era -> ImpTestM era b -> Gen (IO (b, ImpTestState era))
runImpTestGenM impState m = MkGen $ \qcGen qcSz -> runImpTestM (Just (qcGen, qcSz)) impState m

runImpTestM :: Maybe (QCGen, Int) -> ImpTestState era -> ImpTestM era b -> IO (b, ImpTestState era)
runImpTestM mQCGen impState (ImpTestM m) = do
  let
    (qcGen, qcSize, impState') =
      case fromMaybe (impGen impState, 30) mQCGen of
        (initGen, sz) ->
          case split initGen of
            (qc, stdGen) -> (qc, sz, impState {impGen = stdGen})
  ioRef <- newIORef impState'
  let
    env =
      ImpTestEnv
        { iteState = ioRef
        , iteDoTxFixup = True
        }
  res <-
    unGenT (runReaderT m env) qcGen qcSize `catchAny` \exc -> do
      logsDoc <- impLog <$> readIORef ioRef
      let logs = renderString (layoutPretty defaultLayoutOptions logsDoc)
          adjustHUnitExc header (HUnitFailure srcLoc failReason) =
            toException $
              HUnitFailure srcLoc $
                case failReason of
                  Reason msg -> Reason $ logs <> "\n" <> header <> msg
                  ExpectedButGot Nothing expected got ->
                    ExpectedButGot (Just $ logs <> header) expected got
                  ExpectedButGot (Just msg) expected got ->
                    ExpectedButGot (Just (logs <> "\n" <> header <> msg)) expected got
          newExc
            | Just hUnitExc <- fromException exc =
                adjustHUnitExc [] hUnitExc
            | Just (ImpException ann excThrown) <- fromException exc =
                let header = unlines $ zipWith (\n str -> replicate n ' ' <> str) [0, 2 ..] ann
                 in case fromException excThrown of
                      Nothing -> toException $ ImpException [logs, header] excThrown
                      Just hUnitExc -> adjustHUnitExc header hUnitExc
            | otherwise = toException $ ImpException [logs] exc
      throwIO newExc
  endState <- readIORef ioRef
  pure (res, endState)

runShelleyBase :: Globals -> ShelleyBase a -> a
runShelleyBase globals act = runIdentity $ runReaderT act globals

getRewardAccountAmount :: RewardAccount (EraCrypto era) -> ImpTestM era Coin
getRewardAccountAmount rewardAcount = do
  umap <- getsNES $ nesEsL . epochStateUMapL
  let cred = raCredential rewardAcount
  case UMap.lookup cred (RewDepUView umap) of
    Nothing -> assertFailure $ "Expected a reward account: " ++ show cred
    Just RDPair {rdReward} -> pure $ fromCompact rdReward

lookupImpRootTxOut :: ImpTestM era (TxIn (EraCrypto era), TxOut era)
lookupImpRootTxOut = do
  ImpTestState {impRootTxIn} <- get
  utxo <- getUTxO
  case txinLookup impRootTxIn utxo of
    Nothing -> error "Root txId no longer points to an existing unspent output"
    Just rootTxOut -> pure (impRootTxIn, rootTxOut)

impAddNativeScript ::
  forall era.
  EraScript era =>
  NativeScript era ->
  ImpTestM era (ScriptHash (EraCrypto era))
impAddNativeScript nativeScript = do
  let script = fromNativeScript nativeScript
      scriptHash = hashScript @era script
  ImpTestState {impNativeScripts} <- get
  impNativeScriptsL .= Map.insert scriptHash nativeScript impNativeScripts
  pure scriptHash

getScriptTestContext :: ScriptHash (EraCrypto era) -> ImpTestM era (Maybe ScriptTestContext)
getScriptTestContext sh = Map.lookup sh <$> gets impScripts

impNativeScriptsRequired ::
  EraUTxO era =>
  Tx era ->
  ImpTestM
    era
    (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsRequired tx = do
  utxo <- getUTxO
  ImpTestState {impNativeScripts} <- get
  let needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      hashesNeeded = getScriptsHashesNeeded needed
  pure $ impNativeScripts `Map.restrictKeys` hashesNeeded

-- | Modifies transaction by adding necessary scripts
addNativeScriptTxWits ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM
    era
    (Tx era)
addNativeScriptTxWits tx = do
  scriptsRequired <- impNativeScriptsRequired tx
  utxo <- getUTxO
  let ScriptsProvided provided = getScriptsProvided utxo tx
      scriptsToAdd = scriptsRequired Map.\\ provided
  pure $
    tx
      & witsTxL . scriptTxWitsL <>~ fmap fromNativeScript scriptsToAdd

-- | Adds @TxWits@ that will satisfy all of the required key witnesses
updateAddrTxWits ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
updateAddrTxWits tx = do
  let txBody = tx ^. bodyTxL
      txBodyHash = hashAnnotated txBody
  scriptsRequired <- impNativeScriptsRequired tx
  let nativeScripts = Map.elems scriptsRequired
  nativeScriptsKeyPairs <- mapM impSatisfyNativeScript nativeScripts
  witsVKeyNeeded <- impWitsVKeyNeeded txBody
  keyPairs <- mapM lookupKeyPair (Set.toList witsVKeyNeeded)
  let wits = keyPairs ++ Map.elems (mconcat (catMaybes nativeScriptsKeyPairs))
  pure $
    tx
      & witsTxL . addrTxWitsL .~ mkWitnessesVKey txBodyHash wits

addRootTxIn ::
  EraTx era =>
  Tx era ->
  ImpTestM era (Tx era)
addRootTxIn tx = do
  rootTxIn <- fst <$> lookupImpRootTxOut
  pure $
    tx
      & bodyTxL . inputsTxBodyL %~ Set.insert rootTxIn

impNativeScriptKeyPairs ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM
    era
    (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
impNativeScriptKeyPairs tx = do
  scriptsRequired <- impNativeScriptsRequired tx
  let nativeScripts = Map.elems scriptsRequired
  keyPairs <- mapM impSatisfyNativeScript nativeScripts
  pure . mconcat $ catMaybes keyPairs

fixupFees ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupFees tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  kpSpending <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  rootTxIn <- fst <$> lookupImpRootTxOut
  nativeScriptKeyPairs <- impNativeScriptKeyPairs tx
  let
    nativeScriptKeyWits = Map.keysSet nativeScriptKeyPairs
    txWithRoot = tx & bodyTxL . inputsTxBodyL %~ Set.insert rootTxIn
    consumedValue = consumed pp certState utxo (txWithRoot ^. bodyTxL)
    producedValue = produced pp certState (txWithRoot ^. bodyTxL)
    changeBeforeFee = consumedValue <-> producedValue
    changeBeforeFeeTxOut =
      mkBasicTxOut
        (mkAddr (kpSpending, kpStaking))
        changeBeforeFee
  let
    txNoWits =
      txWithRoot & bodyTxL . outputsTxBodyL %~ (:|> changeBeforeFeeTxOut)
    outsBeforeFee = txWithRoot ^. bodyTxL . outputsTxBodyL
    fee = calcMinFeeTxNativeScriptWits utxo pp txNoWits nativeScriptKeyWits
    change = changeBeforeFeeTxOut ^. coinTxOutL <-> fee
    changeTxOut = changeBeforeFeeTxOut & coinTxOutL .~ change
    -- If the remainder is sufficently big we add it to outputs, otherwise we add the
    -- extraneous coin to the fee and discard the remainder TxOut
    txWithFee
      | change >= getMinCoinTxOut pp changeTxOut =
          txNoWits
            & bodyTxL . outputsTxBodyL .~ (outsBeforeFee :|> changeTxOut)
            & bodyTxL . feeTxBodyL .~ fee
      | otherwise =
          txNoWits
            & bodyTxL . outputsTxBodyL .~ outsBeforeFee
            & bodyTxL . feeTxBodyL .~ (fee <> changeTxOut ^. coinTxOutL)
  pure txWithFee

shelleyFixupTx ::
  forall era.
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
shelleyFixupTx =
  addNativeScriptTxWits
    >=> addRootTxIn
    >=> fixupFees
    >=> updateAddrTxWits
    >=> (\tx -> logFeeMismatch tx $> tx)

logFeeMismatch :: (EraGov era, EraUTxO era) => Tx era -> ImpTestM era ()
logFeeMismatch tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let Coin feeUsed = tx ^. bodyTxL . feeTxBodyL
      Coin feeMin = getMinFeeTxUtxo pp tx utxo
  when (feeUsed /= feeMin) $ do
    logEntry $
      "Estimated fee " <> show feeUsed <> " while required fee is " <> show feeMin

submitTx_ :: (HasCallStack, ShelleyEraImp era) => Tx era -> ImpTestM era ()
submitTx_ = void . submitTx

submitTx :: (HasCallStack, ShelleyEraImp era) => Tx era -> ImpTestM era (Tx era)
submitTx tx = trySubmitTx tx >>= expectRightDeepExpr

trySubmitTx ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Either [PredicateFailure (EraRule "LEDGER" era)] (Tx era))
trySubmitTx tx = do
  st <- gets impNES
  doFixup <- asks iteDoTxFixup
  txFixed <-
    if doFixup
      then fixupTx tx
      else pure tx
  lEnv <- impLedgerEnv st
  res <- tryRunImpRule @"LEDGER" lEnv (st ^. nesEsL . esLStateL) txFixed
  let txId = TxId . hashAnnotated $ txFixed ^. bodyTxL
      outsSize = SSeq.length $ txFixed ^. bodyTxL . outputsTxBodyL
      rootIndex
        | outsSize > 0 = outsSize - 1
        | otherwise = error ("Expected at least 1 output after submitting tx: " <> show txId)
  forM res $ \st' -> do
    modify $ impNESL . nesEsL . esLStateL .~ st'
    impRootTxInL .= TxIn txId (mkTxIxPartial (fromIntegral rootIndex))
    pure txFixed

-- | Submit a transaction that is expected to be rejected. The inputs and
-- outputs are automatically balanced.
submitFailingTx ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx era ->
  [PredicateFailure (EraRule "LEDGER" era)] ->
  ImpTestM era ()
submitFailingTx tx expectedFailure = trySubmitTx tx >>= (`shouldBeLeftExpr` expectedFailure)

tryRunImpRule ::
  forall rule era.
  (STS (EraRule rule era), BaseM (EraRule rule era) ~ ShelleyBase) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM era (Either [PredicateFailure (EraRule rule era)] (State (EraRule rule era)))
tryRunImpRule stsEnv stsState stsSignal = do
  let trc = TRC (stsEnv, stsState, stsSignal)
  globals <- use $ to impGlobals
  pure $ runShelleyBase globals (applySTSTest @(EraRule rule era) trc)

runImpRule ::
  forall rule era.
  ( HasCallStack
  , KnownSymbol rule
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , NFData (State (EraRule rule era))
  ) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM era (State (EraRule rule era))
runImpRule stsEnv stsState stsSignal = do
  let ruleName = symbolVal (Proxy @rule)
  tryRunImpRule @rule stsEnv stsState stsSignal >>= \case
    Left [] -> error "Impossible: STS rule failed without a predicate failure"
    Left fs ->
      assertFailure $
        unlines $
          ("Failed to run " <> ruleName <> ":") : map show fs
    Right res -> evaluateDeep res

-- | Runs the TICK rule once
passTick ::
  forall era.
  ( HasCallStack
  , NFData (State (EraRule "TICK" era))
  , ShelleyEraImp era
  ) =>
  ImpTestM era ()
passTick = do
  impLastTick <- gets impLastTick
  curNES <- getsNES id
  nes <- runImpRule @"TICK" () curNES impLastTick
  impLastTickL += 1
  impNESL .= nes

-- | Runs the TICK rule until the next epoch is reached
passEpoch ::
  forall era.
  ShelleyEraImp era =>
  ImpTestM era ()
passEpoch = do
  startEpoch <- getsNES nesELL
  logEntry $ "Entering " <> show (succ startEpoch)
  let
    tickUntilNewEpoch curEpoch = do
      passTick @era
      newEpoch <- getsNES nesELL
      unless (newEpoch > curEpoch) $ tickUntilNewEpoch newEpoch
  preAdaPots <- getAdaPots
  tickUntilNewEpoch startEpoch
  impAnn "Checking ADA preservation at the epoch boundary" $ do
    postAdaPots <- getAdaPots
    logEntry $ diffExpr preAdaPots postAdaPots
    let
      preSum = sumAdaPots preAdaPots
      postSum = sumAdaPots postAdaPots
    unless (preSum == postSum) . expectationFailure $
      "Total ADA in the epoch state is not preserved\n\tpost - pre = "
        <> show (postSum <-> preSum)

-- | Runs the TICK rule until the `n` epochs are passed
passNEpochs ::
  forall era.
  ShelleyEraImp era =>
  Natural ->
  ImpTestM era ()
passNEpochs n = when (n > 0) $ passEpoch >> passNEpochs (n - 1)

-- | Stores extra information about the failure of the unit test
data ImpException = ImpException
  { ieAnnotation :: [String]
  -- ^ Description of the IO action that caused the failure
  , ieThrownException :: SomeException
  -- ^ Exception that caused the test to fail
  }

instance Show ImpException where
  show (ImpException ann e) =
    "Log:\n"
      <> unlines ann
      <> "\nFailed with Exception:\n\t"
      <> displayException e
instance Exception ImpException

-- | Annotation for when failure happens. All the logging done within annotation will be
-- discarded if there no failures within the annotation.
impAnn :: NFData a => String -> ImpTestM era a -> ImpTestM era a
impAnn msg m = do
  logs <- use impLogL
  res <- catchAnyDeep m $ \exc ->
    throwIO $
      case fromException exc of
        Just (ImpException ann origExc) -> ImpException (msg : ann) origExc
        Nothing -> ImpException [msg] exc
  impLogL .= logs
  pure res

-- | Adds a string to the log, which is only shown if the test fails
logEntry :: HasCallStack => String -> ImpTestM era ()
logEntry e = impLogL %= (<> pretty loc <> "\t" <> pretty e <> line)
  where
    formatSrcLoc srcLoc =
      "[" <> srcLocModule srcLoc <> ":" <> show (srcLocStartLine srcLoc) <> "]\n"
    loc =
      case getCallStack ?callStack of
        (_, srcLoc) : _ -> formatSrcLoc srcLoc
        _ -> ""

logToExpr :: (HasCallStack, ToExpr a) => a -> ImpTestM era ()
logToExpr e = logEntry (showExpr e)

withImpState ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpTestState era) ->
  Spec
withImpState =
  beforeAll $
    pure
      ImpTestState
        { impNES = initImpNES rootCoin
        , impRootTxIn = TxIn (mkTxId 0) minBound
        , impFreshIdx = 1000 -- Added some leeway to prevent collisions with values in the initial state
        , impKeyPairs =
            Map.fromList $
              (\kp@KeyPair {vKey} -> (hashKey vKey, kp)) . mkKeyPair <$> [0 .. 100]
        , impNativeScripts = mempty
        , impLastTick = 0
        , impGlobals = testGlobals
        , impLog = mempty
        , impGen = mkQCGen 2024
        , impScripts = mempty
        , impCollateralTxIds = fst <$> mkCollateralUTxO @era rootCoin
        }
  where
    rootCoin = Coin 1_000_000_000

dispenseIndex :: ImpTestM era Integer
dispenseIndex = do
  ImpTestState {impFreshIdx} <- get
  modify $ \st -> st {impFreshIdx = succ impFreshIdx}
  pure impFreshIdx

-- | Creates a fresh @SafeHash@
freshSafeHash :: Era era => ImpTestM era (SafeHash (EraCrypto era) a)
freshSafeHash = mkDummySafeHash Proxy . fromInteger <$> dispenseIndex

-- | Adds a key pair to the keyhash lookup map
addKeyPair :: Era era => KeyPair r (EraCrypto era) -> ImpTestM era ()
addKeyPair kp@(KeyPair vk _) = do
  ImpTestState {impKeyPairs} <- get
  modify $ \st ->
    st
      { impKeyPairs =
          Map.insert
            (coerceKeyRole $ hashKey vk)
            (coerce kp)
            impKeyPairs
      }

-- | Looks up the keypair corresponding to the keyhash. The keyhash must be
-- created with @freshKeyHash@ for this to work.
lookupKeyPair :: HasCallStack => KeyHash r (EraCrypto era) -> ImpTestM era (KeyPair r (EraCrypto era))
lookupKeyPair keyHash = do
  keyPairs <- gets impKeyPairs
  case Map.lookup keyHash keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to keyHash:"
          ++ show keyHash
          ++ "\nAlways use `freshKeyHash` to create key hashes."

-- | Generates a fresh keyhash and stores the corresponding keypair in the
-- ImpTestState. Use @lookupKeyPair@ to look up the keypair corresponding to the
-- keyhash
freshKeyHash :: Era era => ImpTestM era (KeyHash r (EraCrypto era))
freshKeyHash = do
  ImpTestState {impFreshIdx} <- get
  let kp@(KeyPair kh _) = mkKeyPair $ fromIntegral impFreshIdx
  addKeyPair kp
  impFreshIdxL += 1
  pure $ hashKey kh

sendCoinTo ::
  ShelleyEraImp era =>
  Addr (EraCrypto era) ->
  Coin ->
  ImpTestM era ()
sendCoinTo addr = sendValueTo addr . inject

sendValueTo ::
  ShelleyEraImp era =>
  Addr (EraCrypto era) ->
  Value era ->
  ImpTestM era ()
sendValueTo addr amount = do
  submitTxAnn_
    ("Giving " <> show amount <> " to " <> show addr)
    $ mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr amount)

-- | Modify the current new epoch state with a function
modifyNES :: (NewEpochState era -> NewEpochState era) -> ImpTestM era ()
modifyNES = (impNESL %=)

-- | Get a value from the current new epoch state using the lens
getsNES :: SimpleGetter (NewEpochState era) a -> ImpTestM era a
getsNES l = gets . view $ impNESL . l

getUTxO :: ImpTestM era (UTxO era)
getUTxO = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL

submitTxAnn ::
  (HasCallStack, ShelleyEraImp era) =>
  String ->
  Tx era ->
  ImpTestM era (Tx era)
submitTxAnn msg tx = impAnn msg (trySubmitTx tx >>= expectRightDeepExpr)

submitTxAnn_ ::
  (HasCallStack, ShelleyEraImp era) => String -> Tx era -> ImpTestM era ()
submitTxAnn_ msg = void . submitTxAnn msg

registerRewardAccount ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era (RewardAccount (EraCrypto era))
registerRewardAccount = do
  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  kpSpending <- lookupKeyPair =<< freshKeyHash
  let stakingCredential = KeyHashObj khDelegator
  submitTxAnn_ ("Register Reward Account: " <> T.unpack (credToText stakingCredential)) $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.fromList
          [ mkBasicTxOut
              (mkAddr (kpSpending, kpDelegator))
              (inject $ Coin 10_000_000)
          ]
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList [RegTxCert @era stakingCredential]
  networkId <- use (to impGlobals . to networkId)
  pure $ RewardAccount networkId stakingCredential

lookupReward :: HasCallStack => Credential 'Staking (EraCrypto era) -> ImpTestM era Coin
lookupReward stakingCredential = do
  umap <- getsNES (nesEsL . epochStateUMapL)
  case UMap.lookup stakingCredential (RewDepUView umap) of
    Nothing ->
      error $
        "Staking Credential is not found in the state: "
          <> show stakingCredential
          <> "\nMake sure you have the reward account registered with `registerRewardAccount` "
          <> "or by some other means."
    Just rd -> pure $ fromCompact (rdReward rd)

registerPool :: ShelleyEraImp era => ImpTestM era (KeyHash 'StakePool (EraCrypto era))
registerPool = do
  khPool <- freshKeyHash
  rewardAccount <- registerRewardAccount
  vrfHash <- freshKeyHashVRF
  let
    poolParams =
      PoolParams
        { ppVrf = vrfHash
        , ppRewardAccount = rewardAccount
        , ppRelays = mempty
        , ppPledge = zero
        , ppOwners = mempty
        , ppMetadata = SNothing
        , ppMargin = def
        , ppId = khPool
        , ppCost = zero
        }
  submitTxAnn_ "Registering a new stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert poolParams)
  pure khPool

-- | Performs the action without running the fix-up function on any transactions
withNoFixup :: ImpTestM era a -> ImpTestM era a
withNoFixup = local $ iteDoTxFixupL .~ False

expectRegisteredRewardAddress :: RewardAccount (EraCrypto era) -> ImpTestM era ()
expectRegisteredRewardAddress (RewardAccount _ cred) = do
  umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
  Map.member cred (rdPairMap umap) `shouldBe` True

expectNotRegisteredRewardAddress :: RewardAccount (EraCrypto era) -> ImpTestM era ()
expectNotRegisteredRewardAddress (RewardAccount _ cred) = do
  umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
  Map.member cred (rdPairMap umap) `shouldBe` False

expectTreasury :: HasCallStack => Coin -> ImpTestM era ()
expectTreasury c =
  impAnn "Checking treasury amount" $ do
    treasuryAmt <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    c `shouldBe` treasuryAmt
