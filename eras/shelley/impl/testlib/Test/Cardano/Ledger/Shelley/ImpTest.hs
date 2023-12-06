{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  ImpTestState,
  ImpTestEnv (..),
  ImpException (..),
  ShelleyEraImp (..),
  emptyShelleyImpNES,
  shelleyImpWitsVKeyNeeded,
  modifyPrevPParams,
  passEpoch,
  passTick,
  freshKeyHash,
  freshSafeHash,
  lookupKeyPair,
  submitTx,
  submitFailingTx,
  trySubmitTx,
  modifyNES,
  getsNES,
  impAnn,
  mkTxWits,
  runImpRule,
  registerRewardAccount,
  getRewardAccountAmount,
  constitutionShouldBe,
  withImpState,
  fixupFees,
  -- Logging
  logEntry,
  logStakeDistr,
  -- Combinators
  withNoFixup,
  -- Lenses
  impNESL,
  impLastTickL,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  BlocksMade (..),
  EpochSize (..),
  Globals (..),
  Network (..),
  ShelleyBase,
  SlotNo,
  StrictMaybe (..),
  TxIx (..),
  textToUrl,
 )
import Cardano.Ledger.CertState (certsTotalDepositsTxBody, certsTotalRefundsTxBody)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..), credToText)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash,
  KeyRole (..),
  hashKey,
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (Constitution (..), EraGov (..), ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  certDStateL,
  curPParamsEpochStateL,
  dsGenDelegsL,
  epochStateIncrStakeDistrL,
  epochStateUMapL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  prevPParamsEpochStateL,
  smartUTxOState,
  startStep,
  utxosGovStateL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), shelleyWitsVKeyNeeded)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), sumAllCoin)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State.Strict (MonadState (..), gets, modify)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.State.Transition (STS (..), TRC (..))
import Control.State.Transition.Trace (applySTSTest)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Default.Class (Default (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stack (SrcLoc (..), getCallStack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=))
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.String (renderString)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkKeyHash, mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec.Core.Spec (Example (..))
import UnliftIO (MonadUnliftIO)
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
  , impRootTxCoin :: !Coin
  , impRootTxId :: !(TxId (EraCrypto era))
  , impSafeHashIdx :: !Int
  , impKeyPairs :: !(forall k. Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
  , impLastTick :: !SlotNo
  , impGlobals :: !Globals
  , impLog :: !(Doc ())
  }

impLogL :: Lens' (ImpTestState era) (Doc ())
impLogL = lens impLog (\x y -> x {impLog = y})

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

impLastTickL :: Lens' (ImpTestState era) SlotNo
impLastTickL = lens impLastTick (\x y -> x {impLastTick = y})

impRootTxCoinL :: Lens' (ImpTestState era) Coin
impRootTxCoinL = lens impRootTxCoin (\x y -> x {impRootTxCoin = y})

impRootTxIdL :: Lens' (ImpTestState era) (TxId (EraCrypto era))
impRootTxIdL = lens impRootTxId (\x y -> x {impRootTxId = y})

class
  ( Show (NewEpochState era)
  , ToExpr (NewEpochState era)
  , ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
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
  , EraUTxO era
  , ShelleyEraTxBody era
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
  ) =>
  ShelleyEraImp era
  where
  emptyImpNES :: Coin -> NewEpochState era

  impWitsVKeyNeeded ::
    NewEpochState era ->
    TxBody era ->
    Set.Set (KeyHash 'Witness (EraCrypto era))

  -- | This modifer should change not only the current PParams, but also the future
  -- PParams. If the future PParams are not updated, then they will overwrite the
  -- mofication of the current PParams at the next epoch.
  modifyPParams ::
    (PParams era -> PParams era) ->
    ImpTestM era ()
  modifyPParams f = modifyNES $ nesEsL . curPParamsEpochStateL %~ f

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

emptyShelleyImpNES ::
  forall era.
  ( EraGov era
  , EraTxOut era
  , Default (StashedAVVMAddresses era)
  ) =>
  Coin ->
  NewEpochState era
emptyShelleyImpNES rootCoin =
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
                  (VRF.hashVerKeyVRF . VRF.deriveVerKeyVRF . VRF.genKeyVRF . mkSeedFromBytes $ BS.replicate seedSize 1)
              )
            ]
    , nesEs = epochState
    , nesEL = 0
    , nesBprev = BlocksMade (Map.singleton testKeyHash 10)
    , nesBcur = BlocksMade mempty
    }
  where
    testKeyHash = mkKeyHash (-1)
    rootTxId = TxId (mkDummySafeHash Proxy 0)
    seedSize = fromIntegral . seedSizeDSIGN $ Proxy @(DSIGN (EraCrypto era))
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
        & prevPParamsEpochStateL .~ emptyPParams
        & curPParamsEpochStateL .~ emptyPParams
    addr =
      Addr
        Testnet
        (KeyHashObj $ hashKey vk)
        (StakeRefBase (KeyHashObj testKeyHash))
    utxo =
      UTxO $
        Map.fromList
          [
            ( TxIn rootTxId minBound
            , mkBasicTxOut addr $ inject rootCoin
            )
          ]
    KeyPair vk _ = mkKeyPair 0

instance
  ( Crypto c
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (ShelleyEra c)
  where
  emptyImpNES = emptyShelleyImpNES

  impWitsVKeyNeeded = shelleyImpWitsVKeyNeeded

shelleyImpWitsVKeyNeeded ::
  ( ProtVerAtMost era 8
  , EraTx era
  , ShelleyEraTxBody era
  ) =>
  NewEpochState era ->
  TxBody era ->
  Set.Set (KeyHash 'Witness (EraCrypto era))
shelleyImpWitsVKeyNeeded nes txb = shelleyWitsVKeyNeeded utxo txb genDelegs
  where
    utxo = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
    genDelegs = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL

data ImpTestEnv era = ImpTestEnv
  { iteState :: !(IORef (ImpTestState era))
  , iteDoTxFixup :: !Bool
  -- ^ This flag is used to toggle the fixing up of transactions. If it
  -- is set to False then any transaction should be submitted as-is.
  }

iteDoTxFixupL :: Lens' (ImpTestEnv era) Bool
iteDoTxFixupL = lens iteDoTxFixup (\x y -> x {iteDoTxFixup = y})

newtype ImpTestM era a = ImpTestM (ReaderT (ImpTestEnv era) IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader (ImpTestEnv era)
    )

instance MonadState (ImpTestState era) (ImpTestM era) where
  get = ImpTestM $ do
    liftIO . readIORef . iteState =<< ask
  put x = ImpTestM $ do
    liftIO . flip writeIORef x . iteState =<< ask

instance Example (ImpTestM era ()) where
  type Arg (ImpTestM era ()) = ImpTestState era

  evaluateExample impTest = evaluateExample (`evalImpTestM` impTest)

instance (Arbitrary a, Show a) => Example (a -> ImpTestM era ()) where
  type Arg (a -> ImpTestM era ()) = ImpTestState era

  evaluateExample impTest = evaluateExample (\s -> property $ evalImpTestM s . impTest)

evalImpTestM :: ImpTestState era -> ImpTestM era b -> IO b
evalImpTestM impState = fmap fst . runImpTestM impState

execImpTestM :: ImpTestState era -> ImpTestM era b -> IO (ImpTestState era)
execImpTestM impState = fmap snd . runImpTestM impState

runImpTestM_ :: ImpTestState era -> ImpTestM era b -> IO ()
runImpTestM_ impState = void . runImpTestM impState

runImpTestM :: ImpTestState era -> ImpTestM era b -> IO (b, ImpTestState era)
runImpTestM impState (ImpTestM m) = do
  ioRef <- newIORef impState
  let
    env =
      ImpTestEnv
        { iteState = ioRef
        , iteDoTxFixup = True
        }
  res <-
    runReaderT m env `catchAny` \exc -> do
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

getRewardAccountAmount :: RewardAcnt (EraCrypto era) -> ImpTestM era Coin
getRewardAccountAmount rewardAcount = do
  umap <- getsNES $ nesEsL . epochStateUMapL
  let cred = getRwdCred rewardAcount
  case UMap.lookup cred (RewDepUView umap) of
    Nothing -> assertFailure $ "Expected a reward account: " ++ show cred
    Just RDPair {rdReward} -> pure $ fromCompact rdReward

fixupFees ::
  forall era.
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupFees tx = do
  ImpTestState {impRootTxId, impRootTxCoin} <- get
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  kpSpending <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  let
    deposits = certsTotalDepositsTxBody pp certState (tx ^. bodyTxL)
    refunds = certsTotalRefundsTxBody pp certState (tx ^. bodyTxL)
    outputsTotalCoin = sumAllCoin $ tx ^. bodyTxL . outputsTxBodyL
    remainingCoin = impRootTxCoin <-> (outputsTotalCoin <+> deposits <-> refunds)
    remainingTxOut =
      mkBasicTxOut @era
        (mkAddr (kpSpending, kpStaking))
        (inject remainingCoin)
  impRootTxCoinL .= remainingCoin
  let
    balancedTx =
      setMinFeeTx pp tx
        & bodyTxL . inputsTxBodyL %~ Set.insert (TxIn impRootTxId $ TxIx 0)
        & bodyTxL . outputsTxBodyL %~ (remainingTxOut :<|)
    txId = TxId . hashAnnotated $ balancedTx ^. bodyTxL
  impRootTxIdL .= txId
  wits <- mkTxWits (balancedTx ^. bodyTxL)
  pure $ balancedTx & witsTxL .~ wits

submitTx_ ::
  forall era.
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (State (EraRule "LEDGER" era), Tx era)
    )
submitTx_ tx = do
  st <- gets impNES
  doFixup <- asks iteDoTxFixup
  txFixed <-
    if doFixup
      then fixupFees tx
      else pure tx
  lEnv <- impLedgerEnv st
  globals <- use $ to impGlobals
  let
    trc =
      TRC
        ( lEnv
        , st ^. nesEsL . esLStateL
        , txFixed
        )
  pure $ (,txFixed) <$> runShelleyBase globals (applySTSTest @(EraRule "LEDGER" era) trc)

trySubmitTx ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Either [PredicateFailure (EraRule "LEDGER" era)] (TxId (EraCrypto era)))
trySubmitTx tx = do
  res <- submitTx_ tx
  case res of
    Right (st, finalTx) -> do
      modify $ impNESL . nesEsL . esLStateL .~ st
      pure . Right . TxId . hashAnnotated $ finalTx ^. bodyTxL
    Left err -> pure $ Left err

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
  let trc = TRC (stsEnv, stsState, stsSignal)
      ruleName = symbolVal (Proxy @rule)
  globals <- use $ to impGlobals
  case runShelleyBase globals (applySTSTest @(EraRule rule era) trc) of
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
  , BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , State (EraRule "TICK" era) ~ NewEpochState era
  , NFData (State (EraRule "TICK" era))
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
  ( BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , NFData (State (EraRule "TICK" era))
  , State (EraRule "TICK" era) ~ NewEpochState era
  ) =>
  ImpTestM era ()
passEpoch = do
  startEpoch <- getsNES nesELL
  let
    tickUntilNewEpoch curEpoch = do
      passTick @era
      newEpoch <- getsNES nesELL
      if newEpoch > curEpoch
        then logEntry $ "Entered " <> show newEpoch
        else tickUntilNewEpoch newEpoch
  tickUntilNewEpoch startEpoch

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

withImpState :: ShelleyEraImp era => SpecWith (ImpTestState era) -> Spec
withImpState =
  beforeAll $
    pure
      ImpTestState
        { impNES = emptyImpNES rootCoin
        , impRootTxCoin = rootCoin
        , impRootTxId = TxId (mkDummySafeHash Proxy 0)
        , impSafeHashIdx = 0
        , impKeyPairs = mempty
        , impLastTick = 0
        , impGlobals = testGlobals
        , impLog = mempty
        }
  where
    rootCoin = Coin 1_000_000_000

-- | Returns the @TxWits@ needed for sumbmitting the transaction
mkTxWits ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  TxBody era ->
  ImpTestM era (TxWits era)
mkTxWits txb = do
  nes <- getsNES id
  keyPairs <- gets impKeyPairs
  let
    txbHash = hashAnnotated txb
    witsNeeded =
      impWitsVKeyNeeded nes txb
    mkTxWit kh =
      mkWitnessVKey txbHash
        . fromMaybe (error "Could not find a keypair corresponding to keyhash. Always use `freshKeyHash` to create key hashes.")
        $ Map.lookup kh keyPairs
  pure $
    mkBasicTxWits
      & addrTxWitsL .~ Set.map mkTxWit witsNeeded

-- | Creates a fresh @SafeHash@
freshSafeHash :: Era era => ImpTestM era (SafeHash (EraCrypto era) a)
freshSafeHash = do
  ImpTestState {impSafeHashIdx} <- get
  modify $ \st -> st {impSafeHashIdx = succ impSafeHashIdx}
  pure $ mkDummySafeHash Proxy impSafeHashIdx

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
lookupKeyPair kh = do
  ImpTestState {impKeyPairs} <- get
  maybe (error $ "Could not find keyhash from the keypairs map: " ++ show kh) (pure . coerce) $
    Map.lookup (coerceKeyRole kh) impKeyPairs

-- | Generates a fresh keyhash and stores the corresponding keypair in the
-- ImpTestState. Use @lookupKeyPair@ to look up the keypair corresponding to the
-- keyhash
freshKeyHash :: Era era => ImpTestM era (KeyHash r (EraCrypto era))
freshKeyHash = do
  ImpTestState {impKeyPairs} <- get
  let kp@(KeyPair kh _) = mkKeyPair $ Map.size impKeyPairs
  addKeyPair kp
  pure $ hashKey kh

-- | Modify the current new epoch state with a function
modifyNES :: (NewEpochState era -> NewEpochState era) -> ImpTestM era ()
modifyNES = (impNESL %=)

-- | Get a value from the current new epoch state using the lens
getsNES :: SimpleGetter (NewEpochState era) a -> ImpTestM era a
getsNES l = gets . view $ impNESL . l

submitTx ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  String ->
  Tx era ->
  ImpTestM era (TxId (EraCrypto era))
submitTx msg tx = logEntry msg >> trySubmitTx tx >>= expectRightDeepExpr

registerRewardAccount ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era (RewardAcnt (EraCrypto era))
registerRewardAccount = do
  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  kpSpending <- lookupKeyPair =<< freshKeyHash
  let stakingCredential = KeyHashObj khDelegator
  _ <-
    submitTx ("Register Reward Account: " <> T.unpack (credToText stakingCredential)) $
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
  pure $ RewardAcnt networkId stakingCredential

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: (HasCallStack, EraGov era) => String -> ImpTestM era ()
constitutionShouldBe cUrl = do
  constitution <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . to getConstitution
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    impAnn "Expecting a constitution" $ do
      pure $
        fromMaybe
          (error "No constitution has been set")
          constitution
  anchorUrl `shouldBe` fromJust (textToUrl $ T.pack cUrl)

-- | Performs the action without running the fix-up function on any transactions
withNoFixup :: ImpTestM era a -> ImpTestM era a
withNoFixup = local $ iteDoTxFixupL .~ False
