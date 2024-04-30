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
  SomeSTSEvent (..),
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
  freshKeyPair,
  freshKeyAddr,
  lookupKeyPair,
  freshByronKeyHash,
  freshBootstapAddress,
  lookupByronKeyPair,
  freshSafeHash,
  freshKeyHashVRF,
  submitTx,
  submitTx_,
  submitTxAnn,
  submitTxAnn_,
  submitFailingTx,
  trySubmitTx,
  modifyNES,
  getProtVer,
  getsNES,
  getUTxO,
  impAddNativeScript,
  impAnn,
  runImpRule,
  tryRunImpRule,
  registerRewardAccount,
  getRewardAccountFor,
  lookupReward,
  registerPool,
  registerAndRetirePoolToMakeReward,
  getRewardAccountAmount,
  withImpState,
  withImpStateModified,
  shelleyFixupTx,
  lookupImpRootTxOut,
  sendValueTo,
  sendCoinTo,
  expectRegisteredRewardAddress,
  expectNotRegisteredRewardAddress,
  expectTreasury,
  updateAddrTxWits,
  addNativeScriptTxWits,
  addRootTxIn,
  fixupFees,
  impGetNativeScript,
  impLookupUTxO,

  -- * Logging
  logEntry,
  logToExpr,
  logStakeDistr,
  logFeeMismatch,

  -- * Combinators
  withCustomFixup,
  withFixup,
  withNoFixup,
  withPostFixup,
  withPreFixup,
  impNESL,
  impLastTickG,
  impKeyPairsG,
  impNativeScriptsG,
) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN, seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash, HashAlgorithm)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress (..),
  RewardAccount (..),
  bootstrapKeyHash,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
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
  bootstrapWitKeyHash,
  hashKey,
  makeBootstrapWitness,
  witVKeyHash,
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash, extractHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (sumAdaPots, totalAdaPotsES)
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
  utxosDonationL,
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
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer.Class (MonadWriter (..))
import Control.State.Transition (STS (..), TRC (..), applySTSOptsEither)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  AssertionPolicy (..),
  SingEP (..),
  ValidationPolicy (..),
 )
import Data.Coerce (coerce)
import Data.Data (Proxy (..), type (:~:) (..))
import Data.Default.Class (Default (..))
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Type.Equality (TestEquality (..))
import GHC.Stack (SrcLoc (..), getCallStack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=))
import Numeric.Natural (Natural)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.String (renderString)
import System.Random
import qualified System.Random as Random
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraExpectation)
import Test.Cardano.Ledger.Core.KeyPair (
  ByronKeyPair (..),
  KeyPair (..),
  mkAddr,
  mkKeyHash,
  mkWitnessesVKey,
 )
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals, txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs, ScriptTestContext)
import Test.Cardano.Ledger.Shelley.TreeDiff (Expr (..))
import Test.Cardano.Slotting.Numeric ()
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec.Core.Spec (Example (..), Params, paramsQuickCheckArgs)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (QCGen (..), integerVariant, mkQCGen)
import Type.Reflection (Typeable, typeOf)
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (
  Exception (..),
  SomeException (..),
  catchAny,
  catchAnyDeep,
  evaluateDeep,
  throwIO,
 )

data SomeSTSEvent era
  = forall (rule :: Symbol).
    ( Typeable (Event (EraRule rule era))
    , Eq (Event (EraRule rule era))
    , ToExpr (Event (EraRule rule era))
    ) =>
    SomeSTSEvent (Event (EraRule rule era))

instance Eq (SomeSTSEvent era) where
  SomeSTSEvent x == SomeSTSEvent y
    | Just Refl <- testEquality (typeOf x) (typeOf y) = x == y
    | otherwise = False

instance ToExpr (SomeSTSEvent era) where
  toExpr (SomeSTSEvent ev) = App "SomeSTSEvent" [toExpr ev]

data ImpTestState era = ImpTestState
  { impNES :: !(NewEpochState era)
  , impRootTxIn :: !(TxIn (EraCrypto era))
  , impKeyPairs :: !(forall k. Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
  , impByronKeyPairs :: !(Map (BootstrapAddress (EraCrypto era)) ByronKeyPair)
  , impNativeScripts :: !(Map (ScriptHash (EraCrypto era)) (NativeScript era))
  , impLastTick :: !SlotNo
  , impGlobals :: !Globals
  , impLog :: !(Doc ())
  , impGen :: !QCGen
  , impEvents :: [SomeSTSEvent era]
  }

impLogL :: Lens' (ImpTestState era) (Doc ())
impLogL = lens impLog (\x y -> x {impLog = y})

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

impLastTickL :: Lens' (ImpTestState era) SlotNo
impLastTickL = lens impLastTick (\x y -> x {impLastTick = y})

impLastTickG :: SimpleGetter (ImpTestState era) SlotNo
impLastTickG = impLastTickL

impRootTxInL :: Lens' (ImpTestState era) (TxIn (EraCrypto era))
impRootTxInL = lens impRootTxIn (\x y -> x {impRootTxIn = y})

impKeyPairsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
impKeyPairsG = to impKeyPairs

impNativeScriptsL :: Lens' (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsL = lens impNativeScripts (\x y -> x {impNativeScripts = y})

impNativeScriptsG ::
  SimpleGetter (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (NativeScript era))
impNativeScriptsG = impNativeScriptsL

impEventsL :: Lens' (ImpTestState era) [SomeSTSEvent era]
impEventsL = lens impEvents (\x y -> x {impEvents = y})

class
  ( EraGov era
  , EraUTxO era
  , EraTxOut era
  , EraPParams era
  , ShelleyEraTxCert era
  , ToExpr (Tx era)
  , NFData (Tx era)
  , ToExpr (TxBody era)
  , ToExpr (TxOut era)
  , ToExpr (Value era)
  , ToExpr (PParams era)
  , ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , Show (NewEpochState era)
  , ToExpr (NewEpochState era)
  , ToExpr (GovState era)
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , -- For the LEDGER rule
    STS (EraRule "LEDGER" era)
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , NFData (PredicateFailure (EraRule "LEDGER" era))
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  , EraRuleEvent "LEDGER" era ~ Event (EraRule "LEDGER" era)
  , Eq (EraRuleEvent "LEDGER" era)
  , ToExpr (EraRuleEvent "LEDGER" era)
  , NFData (EraRuleEvent "LEDGER" era)
  , Typeable (EraRuleEvent "LEDGER" era)
  , -- For the TICK rule
    STS (EraRule "TICK" era)
  , BaseM (EraRule "TICK" era) ~ ShelleyBase
  , Signal (EraRule "TICK" era) ~ SlotNo
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Environment (EraRule "TICK" era) ~ ()
  , NFData (PredicateFailure (EraRule "TICK" era))
  , EraRuleEvent "TICK" era ~ Event (EraRule "TICK" era)
  , Eq (EraRuleEvent "TICK" era)
  , ToExpr (EraRuleEvent "TICK" era)
  , NFData (EraRuleEvent "TICK" era)
  , Typeable (EraRuleEvent "TICK" era)
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , -- Necessary Crypto
    DSIGN (EraCrypto era) ~ Ed25519DSIGN
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  , VRF.VRFAlgorithm (VRF (EraCrypto era))
  , HashAlgorithm (HASH (EraCrypto era))
  , DSIGNAlgorithm (DSIGN (EraCrypto era))
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  ShelleyEraImp era
  where
  initImpTestState ::
    (MonadState (ImpTestState era) m, MonadGen m) => m ()

  -- | Try to find a sufficient number of KeyPairs that would satisfy a native script.
  -- Whenever script can't be satisfied, Nothing is returned
  impSatisfyNativeScript ::
    -- | Set of Witnesses that have already been satisfied
    Set.Set (KeyHash 'Witness (EraCrypto era)) ->
    NativeScript era ->
    ImpTestM era (Maybe (Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))))

  -- | This modifer should change not only the current PParams, but also the future
  -- PParams. If the future PParams are not updated, then they will overwrite the
  -- mofication of the current PParams at the next epoch.
  modifyPParams ::
    (PParams era -> PParams era) ->
    ImpTestM era ()
  modifyPParams f = modifyNES $ nesEsL . curPParamsEpochStateL %~ f

  fixupTx :: HasCallStack => Tx era -> ImpTestM era (Tx era)

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

testKeyHash :: Crypto c => KeyHash kd c
testKeyHash = mkKeyHash (-1)

initShelleyImpNES ::
  forall era. ShelleyEraImp era => NewEpochState era
initShelleyImpNES =
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
    utxo = mempty

mkTxId :: Crypto c => Int -> TxId c
mkTxId idx = TxId (mkDummySafeHash Proxy idx)

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (ShelleyEra c)
  where
  initImpTestState = pure ()

  impSatisfyNativeScript providedVKeyHashes script = do
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
        RequireSignature keyHash
          | keyHash `Set.member` providedVKeyHashes -> Just mempty
          | otherwise -> do
              keyPair <- Map.lookup keyHash keyPairs
              Just $ Map.singleton keyHash keyPair
        RequireAllOf ss -> satisfyMOf (length ss) ss
        RequireAnyOf ss -> satisfyMOf 1 ss
        RequireMOf m ss -> satisfyMOf m ss
    pure $ satisfyScript script

  fixupTx = shelleyFixupTx

-- | Figure out all the Byron Addresses that need witnesses as well as all of the
-- KeyHashes for Shelley Key witnesses that are required.
impWitsVKeyNeeded ::
  EraUTxO era =>
  TxBody era ->
  ImpTestM
    era
    ( Set.Set (BootstrapAddress (EraCrypto era)) -- Byron Based Addresses
    , Set.Set (KeyHash 'Witness (EraCrypto era)) -- Shelley Based KeyHashes
    )
impWitsVKeyNeeded txBody = do
  ls <- getsNES (nesEsL . esLStateL)
  utxo <- getUTxO
  let toBootAddr txIn = do
        txOut <- txinLookup txIn utxo
        txOut ^. bootAddrTxOutF
      bootAddrs = Set.fromList $ mapMaybe toBootAddr $ Set.toList (txBody ^. spendableInputsTxBodyF)
      bootKeyHashes = Set.map (coerceKeyRole . bootstrapKeyHash) bootAddrs
      allKeyHashes =
        getWitsVKeyNeeded (ls ^. lsCertStateL) (ls ^. lsUTxOStateL . utxosUtxoL) txBody
  pure (bootAddrs, allKeyHashes Set.\\ bootKeyHashes)

data ImpTestEnv era = ImpTestEnv
  { iteState :: !(IORef (ImpTestState era))
  , iteFixup :: Tx era -> ImpTestM era (Tx era)
  , iteQuickCheckSize :: !Int
  }

iteFixupL :: Lens' (ImpTestEnv era) (Tx era -> ImpTestM era (Tx era))
iteFixupL = lens iteFixup (\x y -> x {iteFixup = y})

newtype ImpTestM era a = ImpTestM {_unImpTestM :: ReaderT (ImpTestEnv era) IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader (ImpTestEnv era)
    )

instance MonadWriter [SomeSTSEvent era] (ImpTestM era) where
  writer (x, evs) = (impEventsL %= (<> evs)) $> x
  listen act = do
    oldEvs <- use impEventsL
    impEventsL .= mempty
    res <- act
    newEvs <- use impEventsL
    impEventsL .= oldEvs
    pure (res, newEvs)
  pass act = do
    ((a, f), evs) <- listen act
    writer (a, f evs)

instance MonadFail (ImpTestM era) where
  fail = assertFailure

instance MonadState (ImpTestState era) (ImpTestM era) where
  get = ImpTestM $ do
    liftIO . readIORef . iteState =<< ask
  put x = ImpTestM $ do
    liftIO . flip writeIORef x . iteState =<< ask

instance ShelleyEraImp era => Example (ImpTestM era ()) where
  type Arg (ImpTestM era ()) = ImpTestState era

  evaluateExample impTest params =
    evaluateExample (\s -> uncurry evalImpTestM (applyParamsQCGen params s) impTest) params

instance (ShelleyEraImp era, Arbitrary a, Show a) => Example (a -> ImpTestM era ()) where
  type Arg (a -> ImpTestM era ()) = ImpTestState era

  evaluateExample impTest params =
    evaluateExample (\s -> property $ uncurry evalImpTestM (applyParamsQCGen params s) . impTest) params

instance MonadGen (ImpTestM era) where
  liftGen (MkGen f) = do
    qcSize <- iteQuickCheckSize <$> ask
    StateGen qcGen <- subState split
    pure $ f qcGen qcSize
  variant n action = do
    subState (\(StateGen qcGen) -> ((), StateGen (integerVariant (toInteger n) qcGen)))
    action
  sized f = do
    qcSize <- iteQuickCheckSize <$> ask
    f qcSize
  resize n = local (\env -> env {iteQuickCheckSize = n})
  choose r = subState (Random.randomR r)

instance HasStatefulGen (StateGenM (ImpTestState era)) (ImpTestM era) where
  askStatefulGen = pure StateGenM

instance HasSubState (ImpTestState era) where
  type SubState (ImpTestState era) = StateGen QCGen
  getSubState = StateGen . impGen
  setSubState s (StateGen g) = s {impGen = g}

applyParamsQCGen :: Params -> ImpTestState era -> (Maybe Int, ImpTestState era)
applyParamsQCGen params impTestState =
  case replay (paramsQuickCheckArgs params) of
    Nothing -> (Nothing, impTestState)
    Just (qcGen, qcSize) -> (Just qcSize, mixinCurrentGen impTestState qcGen)

-- | Instead of reqplacing the curren QC generator in the state, we use the current and
-- the supplied to make the new generator
mixinCurrentGen :: ImpTestState era -> QCGen -> ImpTestState era
mixinCurrentGen impTestState qcGen =
  impTestState {impGen = integerVariant (fst (Random.random (impGen impTestState))) qcGen}

evalImpTestGenM :: ShelleyEraImp era => ImpTestState era -> ImpTestM era b -> Gen (IO b)
evalImpTestGenM impState = fmap (fmap fst) . runImpTestGenM impState

evalImpTestM ::
  ShelleyEraImp era => Maybe Int -> ImpTestState era -> ImpTestM era b -> IO b
evalImpTestM qc impState = fmap fst . runImpTestM qc impState

execImpTestGenM ::
  ShelleyEraImp era => ImpTestState era -> ImpTestM era b -> Gen (IO (ImpTestState era))
execImpTestGenM impState = fmap (fmap snd) . runImpTestGenM impState

execImpTestM ::
  ShelleyEraImp era =>
  Maybe Int ->
  ImpTestState era ->
  ImpTestM era b ->
  IO (ImpTestState era)
execImpTestM qcSize impState = fmap snd . runImpTestM qcSize impState

runImpTestGenM_ :: ShelleyEraImp era => ImpTestState era -> ImpTestM era b -> Gen (IO ())
runImpTestGenM_ impState = fmap void . runImpTestGenM impState

runImpTestM_ ::
  ShelleyEraImp era => Maybe Int -> ImpTestState era -> ImpTestM era b -> IO ()
runImpTestM_ qcSize impState = void . runImpTestM qcSize impState

runImpTestGenM ::
  ShelleyEraImp era => ImpTestState era -> ImpTestM era b -> Gen (IO (b, ImpTestState era))
runImpTestGenM impState m =
  MkGen $ \qcGen qcSz -> runImpTestM (Just qcSz) (mixinCurrentGen impState qcGen) m

runImpTestM ::
  ShelleyEraImp era =>
  Maybe Int ->
  ImpTestState era ->
  ImpTestM era b ->
  IO (b, ImpTestState era)
runImpTestM mQCSize impState (ImpTestM m) = do
  let qcSize = fromMaybe 30 mQCSize
  ioRef <- newIORef impState
  let
    env =
      ImpTestEnv
        { iteState = ioRef
        , iteFixup = fixupTx
        , iteQuickCheckSize = qcSize
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
  impNativeScriptsL %= Map.insert scriptHash nativeScript
  pure scriptHash

impNativeScriptsRequired ::
  EraUTxO era =>
  Tx era ->
  ImpTestM era (Map (ScriptHash (EraCrypto era)) (NativeScript era))
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
  ImpTestM era (Tx era)
addNativeScriptTxWits tx = impAnn "addNativeScriptTxWits" $ do
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
updateAddrTxWits tx = impAnn "updateAddrTxWits" $ do
  let txBody = tx ^. bodyTxL
      txBodyHash = hashAnnotated txBody
  (bootAddrs, witsVKeyNeeded) <- impWitsVKeyNeeded txBody
  -- Shelley Based Addr Witnesses
  let curAddrWitHashes = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  extraKeyPairs <- mapM lookupKeyPair $ Set.toList (witsVKeyNeeded Set.\\ curAddrWitHashes)
  let extraAddrVKeyWits = mkWitnessesVKey txBodyHash extraKeyPairs
      addrWitHashes = curAddrWitHashes <> Set.map witVKeyHash extraAddrVKeyWits
  -- Shelley Based Native Script Witnesses
  scriptsRequired <- impNativeScriptsRequired tx
  nativeScriptsKeyPairs <- mapM (impSatisfyNativeScript addrWitHashes) (Map.elems scriptsRequired)
  let extraNativeScriptVKeyWits =
        mkWitnessesVKey txBodyHash $ Map.elems (mconcat (catMaybes nativeScriptsKeyPairs))
  -- Byron Based Witessed
  let curBootAddrWitHashes = Set.map bootstrapWitKeyHash $ tx ^. witsTxL . bootAddrTxWitsL
      bootAddrWitsNeeded =
        [ bootAddr
        | bootAddr <- Set.toList bootAddrs
        , not (coerceKeyRole (bootstrapKeyHash bootAddr) `Set.member` curBootAddrWitHashes)
        ]
  extraBootAddrWits <- forM bootAddrWitsNeeded $ \bootAddr@(BootstrapAddress byronAddr) -> do
    ByronKeyPair _ signingKey <- lookupByronKeyPair bootAddr
    let attrs = Byron.addrAttributes byronAddr
    pure $ makeBootstrapWitness (extractHash txBodyHash) signingKey attrs
  pure $
    tx
      & witsTxL . addrTxWitsL <>~ extraAddrVKeyWits <> extraNativeScriptVKeyWits
      & witsTxL . bootAddrTxWitsL <>~ Set.fromList extraBootAddrWits

-- | This fixup step ensures that there are enough funds in the transaction.
addRootTxIn ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
addRootTxIn tx = impAnn "addRootTxIn" $ do
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
      curAddrWits = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  keyPairs <- mapM (impSatisfyNativeScript curAddrWits) nativeScripts
  pure . mconcat $ catMaybes keyPairs

fixupFees ::
  (ShelleyEraImp era, HasCallStack) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupFees tx = impAnn "fixupFees" $ do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  (_, kpSpending) <- freshKeyPair
  (_, kpStaking) <- freshKeyPair
  nativeScriptKeyPairs <- impNativeScriptKeyPairs tx
  let
    nativeScriptKeyWits = Map.keysSet nativeScriptKeyPairs
    consumedValue = consumed pp certState utxo (tx ^. bodyTxL)
    producedValue = produced pp certState (tx ^. bodyTxL)
    ensureNonNegativeCoin v
      | pointwise (<=) zero v = pure v
      | otherwise = do
          logEntry $ "Failed to validate coin: " <> show v
          pure zero
  logEntry "Validating changeBeforeFee"
  changeBeforeFee <- ensureNonNegativeCoin $ coin consumedValue <-> coin producedValue
  logToExpr changeBeforeFee
  let
    changeBeforeFeeTxOut =
      mkBasicTxOut
        (mkAddr (kpSpending, kpStaking))
        (inject changeBeforeFee)
    txNoWits = tx & bodyTxL . outputsTxBodyL %~ (:|> changeBeforeFeeTxOut)
    outsBeforeFee = tx ^. bodyTxL . outputsTxBodyL
    fee = calcMinFeeTxNativeScriptWits utxo pp txNoWits nativeScriptKeyWits
  logEntry "Validating change"
  change <- ensureNonNegativeCoin $ changeBeforeFeeTxOut ^. coinTxOutL <-> fee
  logToExpr change
  let
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
            & bodyTxL . feeTxBodyL .~ (fee <> change)
  pure txWithFee

shelleyFixupTx ::
  forall era.
  (ShelleyEraImp era, HasCallStack) =>
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
  forall era.
  ( ShelleyEraImp era
  , HasCallStack
  ) =>
  Tx era ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (Tx era))
trySubmitTx tx = do
  txFixed <- asks iteFixup >>= ($ tx)
  logToExpr txFixed
  st <- gets impNES
  lEnv <- impLedgerEnv st
  ImpTestState {impRootTxIn} <- get
  res <- tryRunImpRule @"LEDGER" lEnv (st ^. nesEsL . esLStateL) txFixed
  case res of
    Left predFailures -> do
      -- Verify that produced predicate failures are ready for the node-to-client protocol
      liftIO $ forM_ predFailures $ roundTripEraExpectation @era
      pure $ Left predFailures
    Right (st', events) -> do
      let txId = TxId . hashAnnotated $ txFixed ^. bodyTxL
          outsSize = SSeq.length $ txFixed ^. bodyTxL . outputsTxBodyL
          rootIndex
            | outsSize > 0 = outsSize - 1
            | otherwise = error ("Expected at least 1 output after submitting tx: " <> show txId)
      tell $ fmap (SomeSTSEvent @era @"LEDGER") events
      modify $ impNESL . nesEsL . esLStateL .~ st'
      UTxO utxo <- getUTxO
      -- This TxIn is in the utxo, and thus can be the new root, only if the transaction
      -- was phase2-valid.  Otherwise, no utxo with this id would have been created, and
      -- so we need to set the new root to what it was before the submission.
      let assumedNewRoot = TxIn txId (mkTxIxPartial (fromIntegral rootIndex))
      let newRoot
            | Map.member assumedNewRoot utxo = assumedNewRoot
            | Map.member impRootTxIn utxo = impRootTxIn
            | otherwise = error "Root not found in UTxO"
      impRootTxInL .= newRoot
      pure $ Right txFixed

-- | Submit a transaction that is expected to be rejected. The inputs and
-- outputs are automatically balanced.
submitFailingTx ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx era ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingTx tx expectedFailure = trySubmitTx tx >>= (`shouldBeLeftExpr` expectedFailure)

tryRunImpRule ::
  forall rule era.
  (STS (EraRule rule era), BaseM (EraRule rule era) ~ ShelleyBase) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (State (EraRule rule era), [Event (EraRule rule era)])
    )
tryRunImpRule stsEnv stsState stsSignal = do
  let trc = TRC (stsEnv, stsState, stsSignal)
  globals <- use $ to impGlobals
  let
    stsOpts =
      ApplySTSOpts
        { asoValidation = ValidateAll
        , asoEvents = EPReturn
        , asoAssertions = AssertionsAll
        }
  pure $ runShelleyBase globals (applySTSOptsEither @(EraRule rule era) stsOpts trc)

runImpRule ::
  forall rule era.
  ( HasCallStack
  , KnownSymbol rule
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , NFData (State (EraRule rule era))
  , NFData (Event (EraRule rule era))
  , ToExpr (Event (EraRule rule era))
  , Eq (Event (EraRule rule era))
  , Typeable (Event (EraRule rule era))
  ) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM era (State (EraRule rule era))
runImpRule stsEnv stsState stsSignal = do
  let ruleName = symbolVal (Proxy @rule)
  (res, ev) <-
    tryRunImpRule @rule stsEnv stsState stsSignal >>= \case
      Left fs ->
        assertFailure $
          unlines $
            ("Failed to run " <> ruleName <> ":") : map show (toList fs)
      Right res -> evaluateDeep res
  tell $ fmap (SomeSTSEvent @era @rule) ev
  pure res

-- | Runs the TICK rule once
passTick ::
  forall era.
  ( HasCallStack
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
  preNES <- gets impNES

  tickUntilNewEpoch startEpoch
  gets impNES >>= epochBoundaryCheck preNES

epochBoundaryCheck ::
  (EraTxOut era, EraGov era) =>
  NewEpochState era ->
  NewEpochState era ->
  ImpTestM era ()
epochBoundaryCheck preNES postNES = do
  impAnn "Checking ADA preservation at the epoch boundary" $ do
    let preSum = tot preNES
        postSum = tot postNES
    logEntry $ diffExpr preSum postSum
    unless (preSum == postSum) . expectationFailure $
      "Total ADA in the epoch state is not preserved\n\tpost - pre = "
        <> show (postSum <-> preSum)
  where
    tot nes =
      (<+>)
        (sumAdaPots (totalAdaPotsES (nes ^. nesEsL)))
        (nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosDonationL)

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
  ShelleyEraImp era =>
  SpecWith (ImpTestState era) ->
  Spec
withImpState = withImpStateModified id

withImpStateModified ::
  ShelleyEraImp era =>
  (ImpTestState era -> ImpTestState era) ->
  SpecWith (ImpTestState era) ->
  Spec
withImpStateModified f =
  beforeAll $
    execImpTestM Nothing (f impTestState0) $
      addRootTxOut >> initImpTestState
  where
    impTestState0 =
      ImpTestState
        { impNES = initShelleyImpNES
        , impRootTxIn = rootTxIn
        , impKeyPairs = mempty
        , impByronKeyPairs = mempty
        , impNativeScripts = mempty
        , impLastTick = 0
        , impGlobals = testGlobals
        , impLog = mempty
        , impGen = mkQCGen 2024
        , impEvents = mempty
        }
    rootCoin = Coin 1_000_000_000
    rootTxIn = TxIn (mkTxId 0) minBound
    addRootTxOut = do
      (rootKeyHash, _) <- freshKeyPair
      let rootAddr = Addr Testnet (KeyHashObj rootKeyHash) StakeRefNull
          rootTxOut = mkBasicTxOut rootAddr $ inject rootCoin
      impNESL . nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
        %= (<> UTxO (Map.singleton rootTxIn rootTxOut))

-- | Creates a fresh @SafeHash@
freshSafeHash :: Era era => ImpTestM era (SafeHash (EraCrypto era) a)
freshSafeHash = arbitrary

freshKeyHashVRF ::
  Era era =>
  ImpTestM era (Hash (HASH (EraCrypto era)) (VerKeyVRF (EraCrypto era)))
freshKeyHashVRF = arbitrary

-- | Adds a key pair to the keyhash lookup map
addKeyPair ::
  (Era era, MonadState (ImpTestState era) m) =>
  KeyPair r (EraCrypto era) ->
  m (KeyHash r (EraCrypto era))
addKeyPair keyPair@(KeyPair vk _) = do
  ImpTestState {impKeyPairs} <- get
  let keyHash = hashKey vk
  modify $ \st ->
    st
      { impKeyPairs =
          Map.insert
            (coerceKeyRole keyHash)
            (coerce keyPair)
            impKeyPairs
      }
  pure keyHash

-- | Looks up the `KeyPair` corresponding to the `KeyHash`. The `KeyHash` must be
-- created with `freshKeyHash` for this to work.
lookupKeyPair ::
  HasCallStack => KeyHash r (EraCrypto era) -> ImpTestM era (KeyPair r (EraCrypto era))
lookupKeyPair keyHash = do
  keyPairs <- gets impKeyPairs
  case Map.lookup keyHash keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show keyHash
          ++ "\nAlways use `freshKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `KeyPair` in the
-- ImpTestState. If you also need the `KeyPair` consider using `freshKeyPair` for
-- generation or `lookupKeyPair` to look up the `KeyPair` corresponding to the `KeyHash`
freshKeyHash :: Era era => ImpTestM era (KeyHash r (EraCrypto era))
freshKeyHash = fst <$> freshKeyPair

-- | Generate a random KeyPair and add it to the known keys in the Imp state
freshKeyPair ::
  (Era era, MonadState (ImpTestState era) m, MonadGen m) =>
  m (KeyHash r (EraCrypto era), KeyPair r (EraCrypto era))
freshKeyPair = do
  keyPair <- arbitrary
  keyHash <- addKeyPair keyPair
  pure (keyHash, keyPair)

freshKeyAddr :: Era era => ImpTestM era (KeyHash r (EraCrypto era), Addr (EraCrypto era))
freshKeyAddr = do
  keyHash <- freshKeyHash
  pure (coerceKeyRole keyHash, Addr Testnet (KeyHashObj keyHash) StakeRefNull)

-- | Looks up the keypair corresponding to the `BootstrapAddress`. The `BootstrapAddress`
-- must be created with `freshBootstrapAddess` for this to work.
lookupByronKeyPair :: HasCallStack => BootstrapAddress (EraCrypto era) -> ImpTestM era ByronKeyPair
lookupByronKeyPair bootAddr = do
  keyPairs <- gets impByronKeyPairs
  case Map.lookup bootAddr keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show bootAddr
          ++ "\nAlways use `freshByronKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `ByronKeyPair` in the
-- ImpTestState. If you also need the `ByronKeyPair` consider using `freshByronKeyPair` for
-- generation or `lookupByronKeyPair` to look up the `ByronKeyPair` corresponding to the `KeyHash`
freshByronKeyHash :: Era era => ImpTestM era (KeyHash r (EraCrypto era))
freshByronKeyHash = coerceKeyRole . bootstrapKeyHash <$> freshBootstapAddress

freshBootstapAddress :: ImpTestM era (BootstrapAddress (EraCrypto era))
freshBootstapAddress = do
  ImpTestState {impByronKeyPairs} <- get
  keyPair@(ByronKeyPair verificationKey _) <- arbitrary
  payload <-
    oneof
      [ pure Nothing
      , Just . Byron.HDAddressPayload <$> (uniformByteStringM =<< choose (0, 63))
      ]
  let asd = Byron.VerKeyASD verificationKey
      attrs = Byron.AddrAttributes payload (Byron.NetworkTestnet 0)
      bootAddr = BootstrapAddress $ Byron.makeAddress asd attrs
  modify $ \st ->
    st {impByronKeyPairs = Map.insert bootAddr keyPair impByronKeyPairs}
  pure bootAddr

sendCoinTo ::
  (ShelleyEraImp era, HasCallStack) =>
  Addr (EraCrypto era) ->
  Coin ->
  ImpTestM era (TxIn (EraCrypto era))
sendCoinTo addr = sendValueTo addr . inject

sendValueTo ::
  (ShelleyEraImp era, HasCallStack) =>
  Addr (EraCrypto era) ->
  Value era ->
  ImpTestM era (TxIn (EraCrypto era))
sendValueTo addr amount = do
  tx <-
    submitTxAnn
      ("Giving " <> show amount <> " to " <> show addr)
      $ mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr amount)
  pure $ txInAt (0 :: Int) tx

-- | Modify the current new epoch state with a function
modifyNES :: (NewEpochState era -> NewEpochState era) -> ImpTestM era ()
modifyNES = (impNESL %=)

-- | Get a value from the current new epoch state using the lens
getsNES :: SimpleGetter (NewEpochState era) a -> ImpTestM era a
getsNES l = gets . view $ impNESL . l

getUTxO :: ImpTestM era (UTxO era)
getUTxO = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL

getProtVer :: EraGov era => ImpTestM era ProtVer
getProtVer = getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

submitTxAnn ::
  (HasCallStack, ShelleyEraImp era) =>
  String ->
  Tx era ->
  ImpTestM era (Tx era)
submitTxAnn msg tx = impAnn msg (trySubmitTx tx >>= expectRightDeepExpr)

submitTxAnn_ ::
  (HasCallStack, ShelleyEraImp era) => String -> Tx era -> ImpTestM era ()
submitTxAnn_ msg = void . submitTxAnn msg

getRewardAccountFor ::
  Credential 'Staking (EraCrypto era) ->
  ImpTestM era (RewardAccount (EraCrypto era))
getRewardAccountFor stakingC = do
  networkId <- use (to impGlobals . to networkId)
  pure $ RewardAccount networkId stakingC

registerRewardAccount ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era (RewardAccount (EraCrypto era))
registerRewardAccount = do
  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  (_, kpSpending) <- freshKeyPair
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

registerAndRetirePoolToMakeReward ::
  ShelleyEraImp era =>
  Credential 'Staking (EraCrypto era) ->
  ImpTestM era ()
registerAndRetirePoolToMakeReward stakingC = do
  poolKH <- freshKeyHash
  networkId <- use (to impGlobals . to networkId)
  vrfKH <- freshKeyHashVRF
  Positive pledge <- arbitrary
  Positive cost <- arbitrary
  let poolParams =
        PoolParams
          { ppVrf = vrfKH
          , ppId = poolKH
          , ppRewardAccount = RewardAccount networkId stakingC
          , ppPledge = Coin pledge
          , ppCost = Coin cost
          , ppOwners = mempty
          , ppMetadata = SNothing
          , ppMargin = def
          , ppRelays = mempty
          }
  submitTxAnn_ "Registering a temporary stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert poolParams)
  passEpoch
  currentEpochNo <- getsNES nesELL
  submitTxAnn_ "Retiring the temporary stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (RetirePoolTxCert poolKH $ addEpochInterval currentEpochNo $ EpochInterval 2)
  passEpoch

-- | Compose given function with the configured fixup
withCustomFixup ::
  ((Tx era -> ImpTestM era (Tx era)) -> Tx era -> ImpTestM era (Tx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withCustomFixup f = local $ iteFixupL %~ f

-- | Replace all fixup with the given function
withFixup ::
  (Tx era -> ImpTestM era (Tx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withFixup f = withCustomFixup (const f)

-- | Performs the action without running the fix-up function on any transactions
withNoFixup :: ImpTestM era a -> ImpTestM era a
withNoFixup = withFixup pure

-- | Apply given fixup function before the configured fixup
withPreFixup ::
  (Tx era -> ImpTestM era (Tx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withPreFixup f = withCustomFixup (f >=>)

-- | Apply given fixup function after the configured fixup
withPostFixup ::
  (Tx era -> ImpTestM era (Tx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withPostFixup f = withCustomFixup (>=> f)

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

impGetNativeScript :: ScriptHash (EraCrypto era) -> ImpTestM era (Maybe (NativeScript era))
impGetNativeScript sh = Map.lookup sh <$> gets impNativeScripts

impLookupUTxO :: ShelleyEraImp era => TxIn (EraCrypto era) -> ImpTestM era (TxOut era)
impLookupUTxO txIn = impAnn "Looking up TxOut" $ do
  utxo <- getUTxO
  case txinLookup txIn utxo of
    Just txOut -> pure txOut
    Nothing -> error $ "Failed to get TxOut for " <> show txIn
