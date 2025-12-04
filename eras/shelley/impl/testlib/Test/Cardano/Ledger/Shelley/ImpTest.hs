{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestM,
  LedgerSpec,
  EraSpecificSpec (..),
  SomeSTSEvent (..),
  ImpTestState,
  ImpTestEnv (..),
  ImpException (..),
  ShelleyEraImp (..),
  PlutusArgs,
  ScriptTestContext,
  iteFixupL,
  itePostSubmitTxHookL,
  itePostEpochBoundaryHookL,
  impWitsVKeyNeeded,
  modifyPrevPParams,
  passEpoch,
  passNEpochs,
  passNEpochsChecking,
  passTick,
  freshKeyAddr,
  freshKeyAddr_,
  freshKeyAddrNoPtr,
  freshKeyAddrNoPtr_,
  freshKeyHash,
  freshKeyPair,
  getKeyPair,
  freshByronKeyHash,
  freshBootstapAddress,
  getByronKeyPair,
  freshSafeHash,
  freshKeyHashVRF,
  submitTx,
  submitTx_,
  submitTxAnn,
  submitTxAnn_,
  submitFailingTx,
  submitFailingTxM,
  trySubmitTx,
  submitBlock_,
  submitBlock,
  submitFailingBlock,
  submitFailingBlockM,
  withTxsInBlock_,
  withTxsInBlock,
  withTxsInFailingBlock,
  withTxsInFailingBlockM,
  withTxsInBlockEither,
  tryTxsInBlock,
  impShelleyExpectTxSuccess,
  modifyNES,
  getProtVer,
  getsNES,
  getUTxO,
  impAddNativeScript,
  impAnn,
  impAnnDoc,
  impLogToExpr,
  runImpRule,
  tryRunImpRule,
  tryRunImpRuleNoAssertions,
  delegateStake,
  registerRewardAccount,
  registerStakeCredential,
  expectNotDelegatedToAnyPool,
  expectNotDelegatedToPool,
  expectStakeCredRegistered,
  expectStakeCredNotRegistered,
  expectDelegatedToPool,
  getRewardAccountFor,
  freshPoolParams,
  registerPool,
  registerPoolWithRewardAccount,
  registerAndRetirePoolToMakeReward,
  getBalance,
  lookupBalance,
  getAccountBalance,
  lookupAccountBalance,
  shelleyFixupTx,
  getImpRootTxOut,
  sendValueTo,
  sendValueTo_,
  sendCoinTo,
  sendCoinTo_,
  expectUTxOContent,
  expectRegisteredRewardAddress,
  expectNotRegisteredRewardAddress,
  expectTreasury,
  disableTreasuryExpansion,
  updateAddrTxWits,
  addNativeScriptTxWits,
  addRootTxIn,
  fixupTxOuts,
  fixupFees,
  fixupAuxDataHash,
  impLookupNativeScript,
  impGetUTxO,
  defaultInitNewEpochState,
  defaultInitImpTestState,
  impEraStartEpochNo,
  impSetSeed,
  shelleyModifyImpInitProtVer,
  modifyImpInitPostSubmitTxHook,
  disableImpInitPostSubmitTxHook,
  modifyImpInitPostEpochBoundaryHook,
  disableImpInitPostEpochBoundaryHook,
  disableInConformanceIt,
  minorFollow,
  majorFollow,
  cantFollow,
  whenMajorVersion,
  whenMajorVersionAtLeast,
  whenMajorVersionAtMost,
  unlessMajorVersion,
  getsPParams,
  withEachEraVersion,
  impSatisfyMNativeScripts,
  impSatisfySignature,
  shelleyGenRegTxCert,
  shelleyGenUnRegTxCert,
  shelleyDelegStakeTxCert,

  -- * Logging
  Doc,
  AnsiStyle,
  logDoc,
  logText,
  logString,
  logToExpr,
  logInstantStake,
  logFeeMismatch,

  -- * Combinators
  withCustomFixup,
  withFixup,
  withNoFixup,
  withPostFixup,
  withPreFixup,
  impEventsFrom,
  impRecordSubmittedTxs,
  impNESL,
  impGlobalsL,
  impCurSlotNoG,
  impKeyPairsG,
  impNativeScriptsG,
  produceScript,
  advanceToPointOfNoReturn,
  simulateThenRestore,

  -- * ImpSpec re-exports
  ImpM,
  ImpInit,
) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron (empty)
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress (..),
  RewardAccount (..),
  bootstrapKeyHash,
 )
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeReference (..), credToText)
import Cardano.Ledger.Genesis
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  asWitness,
  bootstrapWitKeyHash,
  makeBootstrapWitness,
  witVKeyHash,
 )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation (translateToShelleyLedgerStateFromUtxo)
import Cardano.Ledger.Shelley.API.Validation (BlockTransitionError (..), applyBlockEither)
import Cardano.Ledger.Shelley.AdaPots (sumAdaPots, totalAdaPotsES)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Genesis (
  ShelleyGenesis (..),
  describeValidationErr,
  fromNominalDiffTimeMicro,
  mkShelleyGlobals,
  validateGenesis,
 )
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  NewEpochState (..),
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  prevPParamsEpochStateL,
  produced,
  utxosDonationL,
 )
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  LedgerEnv (..),
  ShelleyBbodyState (..),
  ShelleyDelegPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  epochFromSlot,
 )
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Translation (toFromByronTranslationContext)
import Cardano.Ledger.Slot (epochInfoFirst, getTheSlotOfNoReturn)
import Cardano.Ledger.Tools (
  calcMinFeeTxNativeScriptWits,
  ensureMinCoinTxOut,
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Control.Monad (forM, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State.Strict (MonadState (..), evalStateT, get, gets, modify, modify', put)
import Control.Monad.Trans.Fail.String (errorFail)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer.Class (MonadWriter (..))
import Control.State.Transition (STS (..), TRC (..), applySTSOptsEither)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  AssertionPolicy (..),
  SingEP (..),
  ValidationPolicy (..),
 )
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Data (Proxy (..), type (:~:) (..))
import Data.Default (Default (..))
import Data.Foldable (fold, toList, traverse_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.TreeDiff (ansiWlExpr)
import Data.Type.Equality (TestEquality (..))
import Data.Void
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, symbolVal, type (<=))
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=))
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.Random.Stateful as R
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraExpectation)
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair (..), mkStakeRef, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs, ScriptTestContext)
import Test.Cardano.Ledger.Shelley.Era
import Test.Cardano.Ledger.Shelley.TreeDiff (Expr (..))
import Test.Cardano.Slotting.Numeric ()
import Test.ImpSpec
import Type.Reflection (Typeable, typeOf)
import UnliftIO (evaluateDeep)

type ImpTestM era = ImpM (LedgerSpec era)

data LedgerSpec era

instance ShelleyEraImp era => ImpSpec (LedgerSpec era) where
  type ImpSpecEnv (LedgerSpec era) = ImpTestEnv era
  type ImpSpecState (LedgerSpec era) = ImpTestState era
  impInitIO qcGen = do
    ioGen <- R.newIOGenM qcGen
    initState <- evalStateT (runReaderT initImpTestState ioGen) (mempty :: ImpPrepState)
    pure $
      ImpInit
        { impInitEnv =
            ImpTestEnv
              { iteFixup = fixupTx
              , itePostSubmitTxHook = \_ _ _ -> pure ()
              , itePostEpochBoundaryHook = \_ _ _ -> pure ()
              }
        , impInitState = initState
        }

  -- There is an important step here of running TICK rule. This is necessary as a final
  -- step of `era` initialization, because on the very first TICK of an era the
  -- `futurePParams` are applied and the epoch number is updated to the first epoch
  -- number of the current era
  impPrepAction = passTick

class EraTest era => EraSpecificSpec era where
  eraSpecificSpec :: SpecWith (ImpInit (LedgerSpec era))
  eraSpecificSpec = pure ()

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
  , impRootTxIn :: !TxIn
  , impKeyPairs :: !(Map (KeyHash Witness) (KeyPair Witness))
  , impByronKeyPairs :: !(Map BootstrapAddress ByronKeyPair)
  , impNativeScripts :: !(Map ScriptHash (NativeScript era))
  , impCurSlotNo :: !SlotNo
  , impGlobals :: !Globals
  , impEvents :: Seq (SomeSTSEvent era)
  , impRecordedTxs :: !(StrictMaybe (StrictSeq (Tx TopTx era)))
  -- ^ When this is set to `SNothing` transactions are not being recorded.
  -- This should never be switched to `Just` outside of simulations.
  }

-- | This is a preliminary state that is used to prepare the actual `ImpTestState`
data ImpPrepState = ImpPrepState
  { impPrepKeyPairs :: !(Map (KeyHash Witness) (KeyPair Witness))
  , impPrepByronKeyPairs :: !(Map BootstrapAddress ByronKeyPair)
  }

instance Semigroup ImpPrepState where
  (<>) ips1 ips2 =
    ImpPrepState
      { impPrepKeyPairs = impPrepKeyPairs ips1 <> impPrepKeyPairs ips2
      , impPrepByronKeyPairs = impPrepByronKeyPairs ips1 <> impPrepByronKeyPairs ips2
      }

instance Monoid ImpPrepState where
  mempty =
    ImpPrepState
      { impPrepKeyPairs = mempty
      , impPrepByronKeyPairs = mempty
      }

class HasKeyPairs t where
  keyPairsL :: Lens' t (Map (KeyHash Witness) (KeyPair Witness))
  keyPairsByronL :: Lens' t (Map BootstrapAddress ByronKeyPair)

instance Era era => HasKeyPairs (ImpTestState era) where
  keyPairsL = lens impKeyPairs (\x y -> x {impKeyPairs = y})
  keyPairsByronL = lens impByronKeyPairs (\x y -> x {impByronKeyPairs = y})

instance HasKeyPairs ImpPrepState where
  keyPairsL = lens impPrepKeyPairs (\x y -> x {impPrepKeyPairs = y})
  keyPairsByronL = lens impPrepByronKeyPairs (\x y -> x {impPrepByronKeyPairs = y})

impGlobalsL :: Lens' (ImpTestState era) Globals
impGlobalsL = lens impGlobals (\x y -> x {impGlobals = y})

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

impCurSlotNoL :: Lens' (ImpTestState era) SlotNo
impCurSlotNoL = lens impCurSlotNo (\x y -> x {impCurSlotNo = y})

impCurSlotNoG :: SimpleGetter (ImpTestState era) SlotNo
impCurSlotNoG = impCurSlotNoL

impRootTxInL :: Lens' (ImpTestState era) TxIn
impRootTxInL = lens impRootTxIn (\x y -> x {impRootTxIn = y})

impKeyPairsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (KeyHash Witness) (KeyPair Witness))
impKeyPairsG = to impKeyPairs

impNativeScriptsL :: Lens' (ImpTestState era) (Map ScriptHash (NativeScript era))
impNativeScriptsL = lens impNativeScripts (\x y -> x {impNativeScripts = y})

impNativeScriptsG ::
  SimpleGetter (ImpTestState era) (Map ScriptHash (NativeScript era))
impNativeScriptsG = impNativeScriptsL

impEventsL :: Lens' (ImpTestState era) (Seq (SomeSTSEvent era))
impEventsL = lens impEvents (\x y -> x {impEvents = y})

impRecordedTxsL :: Lens' (ImpTestState era) (StrictMaybe (StrictSeq (Tx TopTx era)))
impRecordedTxsL = lens impRecordedTxs (\x y -> x {impRecordedTxs = y})

class
  ( ShelleyEraTest era
  , -- For the BBODY rule
    STS (EraRule "BBODY" era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , Eq (Event (EraRule "BBODY" era))
  , ToExpr (Event (EraRule "BBODY" era))
  , Typeable (Event (EraRule "BBODY" era))
  , NFData (BlockBody era)
  , ToExpr (BlockBody era)
  , NFData (PredicateFailure (EraRule "BBODY" era))
  , ToExpr (PredicateFailure (EraRule "BBODY" era))
  , EncCBOR (PredicateFailure (EraRule "BBODY" era))
  , DecCBOR (PredicateFailure (EraRule "BBODY" era))
  , -- For the LEDGERS rule
    Eq (Event (EraRule "LEDGERS" era))
  , ToExpr (Event (EraRule "LEDGERS" era))
  , Typeable (Event (EraRule "LEDGERS" era))
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , -- For the LEDGER rule
    STS (EraRule "LEDGER" era)
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
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
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  , EraRuleFailure "BBODY" era ~ PredicateFailure (EraRule "BBODY" era)
  , EraRuleFailure "LEDGER" era ~ PredicateFailure (EraRule "LEDGER" era)
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyPoolPredFailure era
  , InjectRuleFailure "BBODY" ShelleyPoolPredFailure era
  ) =>
  ShelleyEraImp era
  where
  initGenesis ::
    (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadFail m) =>
    m (Genesis era)
  default initGenesis ::
    (Monad m, Genesis era ~ NoGenesis era) =>
    m (Genesis era)
  initGenesis = pure NoGenesis

  initNewEpochState ::
    (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadFail m) =>
    m (NewEpochState era)
  default initNewEpochState ::
    ( HasKeyPairs s
    , MonadState s m
    , HasStatefulGen g m
    , MonadFail m
    , ShelleyEraImp (PreviousEra era)
    , TranslateEra era NewEpochState
    , TranslationError era NewEpochState ~ Void
    , TranslationContext era ~ Genesis era
    ) =>
    m (NewEpochState era)
  initNewEpochState = defaultInitNewEpochState id

  initImpTestState ::
    ( HasKeyPairs s
    , MonadState s m
    , HasStatefulGen g m
    , MonadFail m
    ) =>
    m (ImpTestState era)
  initImpTestState = initNewEpochState >>= defaultInitImpTestState

  -- | Try to find a sufficient number of KeyPairs that would satisfy a native script.
  -- Whenever script can't be satisfied, Nothing is returned
  impSatisfyNativeScript ::
    -- | Set of Witnesses that have already been satisfied
    Set.Set (KeyHash Witness) ->
    -- | The transaction body that the script will be applied to
    TxBody l era ->
    NativeScript era ->
    ImpTestM era (Maybe (Map (KeyHash Witness) (KeyPair Witness)))

  -- | This modifer should change not only the current PParams, but also the future
  -- PParams. If the future PParams are not updated, then they will overwrite the
  -- mofication of the current PParams at the next epoch.
  modifyPParams ::
    (PParams era -> PParams era) ->
    ImpTestM era ()
  modifyPParams f = modifyNES $ nesEsL . curPParamsEpochStateL %~ f

  modifyImpInitProtVer ::
    ShelleyEraImp era =>
    Version ->
    SpecWith (ImpInit (LedgerSpec era)) ->
    SpecWith (ImpInit (LedgerSpec era))

  fixupTx :: HasCallStack => Tx TopTx era -> ImpTestM era (Tx TopTx era)

  expectTxSuccess :: HasCallStack => Tx TopTx era -> ImpTestM era ()

  genRegTxCert :: Credential Staking -> ImpTestM era (TxCert era)

  genUnRegTxCert :: Credential Staking -> ImpTestM era (TxCert era)

  delegStakeTxCert :: Credential Staking -> KeyHash StakePool -> TxCert era

impSatisfySignature ::
  KeyHash Witness ->
  Set.Set (KeyHash Witness) ->
  ImpTestM era (Maybe (Map (KeyHash Witness) (KeyPair Witness)))
impSatisfySignature keyHash providedVKeyHashes = do
  if keyHash `Set.member` providedVKeyHashes
    then
      pure $ Just mempty
    else do
      keyPairs <- gets impKeyPairs
      pure $ Map.singleton keyHash <$> Map.lookup keyHash keyPairs

impSatisfyMNativeScripts ::
  ShelleyEraImp era =>
  Set.Set (KeyHash Witness) ->
  -- | Set of Witnesses that have already been satisfied
  TxBody l era ->
  -- | The transaction body that the scripts will be applied to
  Int ->
  -- | Number of scripts to satisfy
  StrictSeq (NativeScript era) ->
  -- | List of scripts that can be satisfied
  ImpTestM era (Maybe (Map (KeyHash Witness) (KeyPair Witness)))
impSatisfyMNativeScripts providedVKeyHashes txBody =
  go mempty
  where
    go !acc m Empty
      | m <= 0 = pure $ Just acc
      | otherwise = pure Nothing
    go !acc m (x :<| xs) = do
      satisifed <- impSatisfyNativeScript providedVKeyHashes txBody x
      case satisifed of
        Nothing -> go acc m xs
        Just kps -> go (kps <> acc) (m - 1) xs

defaultInitNewEpochState ::
  forall era g s m.
  ( MonadState s m
  , HasKeyPairs s
  , HasStatefulGen g m
  , MonadFail m
  , ShelleyEraImp era
  , ShelleyEraImp (PreviousEra era)
  , TranslateEra era NewEpochState
  , TranslationError era NewEpochState ~ Void
  , TranslationContext era ~ Genesis era
  ) =>
  (NewEpochState (PreviousEra era) -> NewEpochState (PreviousEra era)) ->
  m (NewEpochState era)
defaultInitNewEpochState modifyPrevEraNewEpochState = do
  genesis <- initGenesis @era
  nes <- initNewEpochState @(PreviousEra era)
  let majProtVer = eraProtVerLow @era
      -- We need to set the protocol version for the current era and for debugging
      -- purposes we start the era at the epoch number that matches the protocol version
      -- times a 100. However, because this is the NewEpochState from the previous era, we
      -- initialize it with futurePParams preset and epoch number that is one behind the
      -- beginning of this era. Note that all imp tests will start with a TICK, in order
      -- for theses changes to be applied.
      prevEraNewEpochState =
        nes
          & nesEsL . curPParamsEpochStateL . ppProtocolVersionL .~ ProtVer majProtVer 0
          & nesELL .~ pred (impEraStartEpochNo @era)
  pure $ translateEra' genesis $ modifyPrevEraNewEpochState prevEraNewEpochState

-- | For debugging purposes we start the era at the epoch number that matches the starting
-- protocol version for the era times a 100
impEraStartEpochNo :: forall era. Era era => EpochNo
impEraStartEpochNo = EpochNo (getVersion majProtVer * 100)
  where
    majProtVer = eraProtVerLow @era

defaultInitImpTestState ::
  forall era s g m.
  ( EraGov era
  , EraTxOut era
  , HasKeyPairs s
  , MonadState s m
  , HasStatefulGen g m
  , MonadFail m
  ) =>
  NewEpochState era ->
  m (ImpTestState era)
defaultInitImpTestState nes = do
  shelleyGenesis <- initGenesis @ShelleyEra
  rootKeyHash <- freshKeyHash @Payment
  let
    rootAddr :: Addr
    rootAddr = mkAddr rootKeyHash StakeRefNull
    rootTxOut :: TxOut era
    rootTxOut = mkBasicTxOut rootAddr $ inject rootCoin
    rootCoin = Coin (toInteger (sgMaxLovelaceSupply shelleyGenesis))
    rootTxIn :: TxIn
    rootTxIn = TxIn (mkTxId 0) minBound
    nesWithRoot = nes & utxoL <>~ UTxO (Map.singleton rootTxIn rootTxOut)
  prepState <- get
  let epochInfoE =
        fixedEpochInfo
          (sgEpochLength shelleyGenesis)
          (mkSlotLength . fromNominalDiffTimeMicro $ sgSlotLength shelleyGenesis)
      globals = mkShelleyGlobals shelleyGenesis epochInfoE
      epochNo = nesWithRoot ^. nesELL
      slotNo = epochInfoFirst (epochInfoPure globals) epochNo
  pure $
    ImpTestState
      { impNES = nesWithRoot
      , impRootTxIn = rootTxIn
      , impKeyPairs = prepState ^. keyPairsL
      , impByronKeyPairs = prepState ^. keyPairsByronL
      , impNativeScripts = mempty
      , impCurSlotNo = slotNo
      , impGlobals = globals
      , impEvents = mempty
      , impRecordedTxs = mempty
      }

withEachEraVersion ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era)) ->
  Spec
withEachEraVersion specWith =
  withImpInit @(LedgerSpec era) $ do
    forM_ (eraProtVersions @era) $ \protVer ->
      describe ("Protocol " <> show protVer) $
        modifyImpInitProtVer protVer specWith

shelleyModifyImpInitProtVer ::
  forall era.
  ShelleyEraImp era =>
  Version ->
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
shelleyModifyImpInitProtVer ver =
  modifyImpInit $ \impInit ->
    impInit
      { impInitState =
          impInitState impInit
            & impNESL . nesEsL . curPParamsEpochStateL . ppProtocolVersionL .~ ProtVer ver 0
      }

modifyImpInitPostSubmitTxHook ::
  forall era.
  ( forall t.
    Globals ->
    TRC (EraRule "LEDGER" era) ->
    Either
      (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
      (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
    ImpM t ()
  ) ->
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
modifyImpInitPostSubmitTxHook f =
  modifyImpInit $ \impInit ->
    impInit
      { impInitEnv =
          impInitEnv impInit
            & itePostSubmitTxHookL .~ f
      }

disableImpInitPostSubmitTxHook ::
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
disableImpInitPostSubmitTxHook =
  modifyImpInitPostSubmitTxHook $ \_ _ _ -> pure ()

modifyImpInitPostEpochBoundaryHook ::
  forall era.
  ( forall t.
    Globals ->
    TRC (EraRule "NEWEPOCH" era) ->
    State (EraRule "NEWEPOCH" era) ->
    ImpM t ()
  ) ->
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
modifyImpInitPostEpochBoundaryHook f = modifyImpInit $ \impInit ->
  impInit
    { impInitEnv =
        impInitEnv impInit
          & itePostEpochBoundaryHookL .~ f
    }

disableImpInitPostEpochBoundaryHook ::
  SpecWith (ImpInit (LedgerSpec era)) ->
  SpecWith (ImpInit (LedgerSpec era))
disableImpInitPostEpochBoundaryHook =
  modifyImpInitPostEpochBoundaryHook $ \_ _ _ -> pure ()

disableInConformanceIt ::
  ShelleyEraImp era =>
  String ->
  ImpTestM era () ->
  SpecWith (ImpInit (LedgerSpec era))
disableInConformanceIt s =
  disableImpInitPostSubmitTxHook
    . disableImpInitPostEpochBoundaryHook
    . it (s ++ " [disabled in conformance]")

impLedgerEnv :: EraGov era => NewEpochState era -> ImpTestM era (LedgerEnv era)
impLedgerEnv nes = do
  slotNo <- gets impCurSlotNo
  epochNo <- runShelleyBase $ epochFromSlot slotNo
  pure
    LedgerEnv
      { ledgerSlotNo = slotNo
      , ledgerEpochNo = Just epochNo
      , ledgerPp = nes ^. nesEsL . curPParamsEpochStateL
      , ledgerIx = TxIx 0
      , ledgerAccount = nes ^. chainAccountStateL
      }

-- | Modify the previous PParams in the current state with the given function. For current
-- and future PParams, use `modifyPParams`
modifyPrevPParams ::
  EraGov era =>
  (PParams era -> PParams era) ->
  ImpTestM era ()
modifyPrevPParams f = modifyNES $ nesEsL . prevPParamsEpochStateL %~ f

-- | Logs the current stake distribution
logInstantStake :: ToExpr (InstantStake era) => HasCallStack => ImpTestM era ()
logInstantStake = do
  stakeDistr <- getsNES instantStakeG
  logDoc $ "Instant Stake: " <> ansiExpr stakeDistr

mkTxId :: Int -> TxId
mkTxId idx = TxId (mkDummySafeHash idx)

instance
  ShelleyEraScript ShelleyEra =>
  ShelleyEraImp ShelleyEra
  where
  initGenesis = do
    let
      gen =
        ShelleyGenesis
          { sgSystemStart = errorFail $ iso8601ParseM "2017-09-23T21:44:51Z"
          , sgNetworkMagic = 123_456 -- Mainnet value: 764824073
          , sgNetworkId = Testnet
          , sgActiveSlotsCoeff = 20 %! 100 -- Mainnet value: 5 %! 100
          , sgSecurityParam = knownNonZeroBounded @108 -- Mainnet value: 2160
          , sgEpochLength = 4320 -- Mainnet value: 432000
          , sgSlotsPerKESPeriod = 129_600
          , sgMaxKESEvolutions = 62
          , sgSlotLength = 1
          , sgUpdateQuorum = 5
          , sgMaxLovelaceSupply = 45_000_000_000_000_000
          , sgProtocolParams =
              emptyPParams
                & ppMinFeeAL .~ CoinPerByte (Coin 44)
                & ppMinFeeBL .~ Coin 155_381
                & ppMaxBBSizeL .~ 65_536
                & ppMaxTxSizeL .~ 16_384
                & ppKeyDepositL .~ Coin 2_000_000
                & ppPoolDepositL .~ Coin 500_000_000
                & ppEMaxL .~ EpochInterval 18
                & ppNOptL .~ 150
                & ppA0L .~ (3 %! 10)
                & ppRhoL .~ (3 %! 1000)
                & ppTauL .~ (2 %! 10)
                & ppDL .~ (1 %! 1)
                & ppExtraEntropyL .~ NeutralNonce
                & ppMinUTxOValueL .~ Coin 2_000_000
                & ppMinPoolCostL .~ Coin 340_000_000
          , -- TODO: Add a top level definition and add private keys to ImpState:
            sgGenDelegs = mempty
          , sgInitialFunds = mempty
          , sgStaking = mempty
          }
    case validateGenesis gen of
      Right () -> pure gen
      Left errs -> fail . T.unpack . T.unlines $ map describeValidationErr errs

  initNewEpochState = do
    shelleyGenesis <- initGenesis @ShelleyEra
    let transContext = toFromByronTranslationContext shelleyGenesis
        startEpochNo = impEraStartEpochNo @ShelleyEra
    pure $ translateToShelleyLedgerStateFromUtxo transContext startEpochNo Byron.empty

  impSatisfyNativeScript providedVKeyHashes txBody script = do
    case script of
      RequireSignature keyHash -> impSatisfySignature keyHash providedVKeyHashes
      RequireAllOf ss -> impSatisfyMNativeScripts providedVKeyHashes txBody (length ss) ss
      RequireAnyOf ss -> do
        m <- frequency [(9, pure 1), (1, choose (1, length ss))]
        impSatisfyMNativeScripts providedVKeyHashes txBody m ss
      RequireMOf m ss -> impSatisfyMNativeScripts providedVKeyHashes txBody m ss
      _ -> error "Impossible: All NativeScripts should have been accounted for"

  fixupTx = shelleyFixupTx
  expectTxSuccess = impShelleyExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer
  genRegTxCert = shelleyGenRegTxCert
  genUnRegTxCert = shelleyGenUnRegTxCert
  delegStakeTxCert = shelleyDelegStakeTxCert

-- | Figure out all the Byron Addresses that need witnesses as well as all of the
-- KeyHashes for Shelley Key witnesses that are required.
impWitsVKeyNeeded ::
  EraUTxO era =>
  TxBody l era ->
  ImpTestM
    era
    ( Set.Set BootstrapAddress -- Byron Based Addresses
    , Set.Set (KeyHash Witness) -- Shelley Based KeyHashes
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
        getWitsVKeyNeeded (ls ^. lsCertStateL) (ls ^. utxoL) txBody
  pure (bootAddrs, allKeyHashes Set.\\ bootKeyHashes)

data ImpTestEnv era = ImpTestEnv
  { iteFixup :: Tx TopTx era -> ImpTestM era (Tx TopTx era)
  , itePostSubmitTxHook ::
      forall t.
      Globals ->
      TRC (EraRule "LEDGER" era) ->
      Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
        (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
      ImpM t ()
  , itePostEpochBoundaryHook ::
      forall t.
      Globals ->
      TRC (EraRule "NEWEPOCH" era) ->
      State (EraRule "NEWEPOCH" era) ->
      ImpM t ()
  }

iteFixupL :: Lens' (ImpTestEnv era) (Tx TopTx era -> ImpTestM era (Tx TopTx era))
iteFixupL = lens iteFixup (\x y -> x {iteFixup = y})

itePostSubmitTxHookL ::
  forall era.
  Lens'
    (ImpTestEnv era)
    ( forall t.
      Globals ->
      TRC (EraRule "LEDGER" era) ->
      Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
        (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
      ImpM t ()
    )
itePostSubmitTxHookL = lens itePostSubmitTxHook (\x y -> x {itePostSubmitTxHook = y})

itePostEpochBoundaryHookL ::
  forall era.
  Lens'
    (ImpTestEnv era)
    ( forall t.
      Globals ->
      TRC (EraRule "NEWEPOCH" era) ->
      State (EraRule "NEWEPOCH" era) ->
      ImpM t ()
    )
itePostEpochBoundaryHookL = lens itePostEpochBoundaryHook (\x y -> x {itePostEpochBoundaryHook = y})

instance MonadWriter (Seq (SomeSTSEvent era)) (ImpTestM era) where
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

impEventsFrom ::
  ImpTestM era () ->
  ImpTestM era [SomeSTSEvent era]
impEventsFrom = fmap (toList . snd) . listen

-- | Returns fixed up versions of all transactions that have been submitted by the supplied action.
-- Will result in a runtime exception if invoked again anywhere within the supplied action.
impRecordSubmittedTxs ::
  ShelleyEraImp era =>
  ImpTestM era () ->
  ImpTestM era (StrictSeq (Tx TopTx era))
impRecordSubmittedTxs act = do
  mTxsPrev <- use impRecordedTxsL
  forM_ mTxsPrev $ \txsPrev -> do
    logToExpr txsPrev
    assertFailure "Detected a recursive attempt to record transactions"
  impRecordedTxsL .= SJust mempty
  act
  mTxsDuring <- use impRecordedTxsL
  impRecordedTxsL .= mTxsPrev
  pure $ fold mTxsDuring

runShelleyBase :: ShelleyBase a -> ImpTestM era a
runShelleyBase act = do
  globals <- use impGlobalsL
  pure $ runIdentity $ runReaderT act globals

lookupBalance :: EraCertState era => Credential Staking -> ImpTestM era (Maybe Coin)
lookupBalance cred = do
  accountsMap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
  pure $
    (\accountState -> fromCompact (accountState ^. balanceAccountStateL))
      <$> Map.lookup cred accountsMap

lookupAccountBalance ::
  (HasCallStack, EraCertState era) => RewardAccount -> ImpTestM era (Maybe Coin)
lookupAccountBalance ra@RewardAccount {raNetwork, raCredential} = do
  networkId <- use (impGlobalsL . to networkId)
  when (raNetwork /= networkId) $
    error $
      "Reward Account with an unexpected NetworkId: " ++ show ra
  lookupBalance raCredential

getBalance :: (HasCallStack, EraCertState era) => Credential Staking -> ImpTestM era Coin
getBalance cred =
  lookupBalance cred >>= \case
    Nothing ->
      assertFailure $
        "Expected a registered account: "
          ++ show cred
          ++ ". Use `registerRewardAccount` to register a new account in ImpSpec"
    Just balance -> pure balance

getAccountBalance :: (HasCallStack, EraCertState era) => RewardAccount -> ImpTestM era Coin
getAccountBalance ra =
  lookupAccountBalance ra >>= \case
    Nothing ->
      assertFailure $
        "Expected a registered account: "
          ++ show ra
          ++ ". Use `registerRewardAccount` to register a new account in ImpSpec"
    Just balance -> pure balance

getImpRootTxOut :: ImpTestM era (TxIn, TxOut era)
getImpRootTxOut = do
  ImpTestState {impRootTxIn} <- get
  utxo <- getUTxO
  case txinLookup impRootTxIn utxo of
    Nothing -> error "Root txId no longer points to an existing unspent output"
    Just rootTxOut -> pure (impRootTxIn, rootTxOut)

impAddNativeScript ::
  forall era.
  EraScript era =>
  NativeScript era ->
  ImpTestM era ScriptHash
impAddNativeScript nativeScript = do
  let script = fromNativeScript nativeScript
      scriptHash = hashScript @era script
  impNativeScriptsL %= Map.insert scriptHash nativeScript
  pure scriptHash

impNativeScriptsRequired ::
  EraUTxO era =>
  Tx l era ->
  ImpTestM era (Map ScriptHash (NativeScript era))
impNativeScriptsRequired tx = do
  utxo <- getUTxO
  ImpTestState {impNativeScripts} <- get
  let needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      hashesNeeded = getScriptsHashesNeeded needed
  pure $ impNativeScripts `Map.restrictKeys` hashesNeeded

-- | Modifies transaction by adding necessary scripts
addNativeScriptTxWits ::
  ShelleyEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
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
  Tx l era ->
  ImpTestM era (Tx l era)
updateAddrTxWits tx = impAnn "updateAddrTxWits" $ do
  let txBody = tx ^. bodyTxL
      txBodyHash = hashAnnotated txBody
  (bootAddrs, witsVKeyNeeded) <- impWitsVKeyNeeded txBody
  -- Shelley Based Addr Witnesses
  let curAddrWitHashes = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  extraKeyPairs <- mapM getKeyPair $ Set.toList (witsVKeyNeeded Set.\\ curAddrWitHashes)
  let extraAddrVKeyWits = mkWitnessesVKey txBodyHash extraKeyPairs
      addrWitHashes = curAddrWitHashes <> Set.map witVKeyHash extraAddrVKeyWits
  -- Shelley Based Native Script Witnesses
  scriptsRequired <- impNativeScriptsRequired tx
  nativeScriptsKeyPairs <-
    mapM (impSatisfyNativeScript addrWitHashes txBody) (Map.elems scriptsRequired)
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
    ByronKeyPair _ signingKey <- getByronKeyPair bootAddr
    let attrs = Byron.addrAttributes byronAddr
    pure $ makeBootstrapWitness (extractHash txBodyHash) signingKey attrs
  pure $
    tx
      & witsTxL . addrTxWitsL <>~ extraAddrVKeyWits <> extraNativeScriptVKeyWits
      & witsTxL . bootAddrTxWitsL <>~ Set.fromList extraBootAddrWits

-- | This fixup step ensures that there are enough funds in the transaction.
addRootTxIn ::
  ShelleyEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
addRootTxIn tx = impAnn "addRootTxIn" $ do
  rootTxIn <- fst <$> getImpRootTxOut
  pure $
    tx
      & bodyTxL . inputsTxBodyL %~ Set.insert rootTxIn

impNativeScriptKeyPairs ::
  ShelleyEraImp era =>
  Tx l era ->
  ImpTestM era (Map (KeyHash Witness) (KeyPair Witness))
impNativeScriptKeyPairs tx = do
  scriptsRequired <- impNativeScriptsRequired tx
  let nativeScripts = Map.elems scriptsRequired
      curAddrWits = Set.map witVKeyHash $ tx ^. witsTxL . addrTxWitsL
  keyPairs <- mapM (impSatisfyNativeScript curAddrWits $ tx ^. bodyTxL) nativeScripts
  pure . mconcat $ catMaybes keyPairs

fixupTxOuts :: (ShelleyEraImp era, HasCallStack) => Tx TopTx era -> ImpTestM era (Tx TopTx era)
fixupTxOuts tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let
    txOuts = tx ^. bodyTxL . outputsTxBodyL
  fixedUpTxOuts <- forM txOuts $ \txOut -> do
    if txOut ^. coinTxOutL == zero
      then do
        amount <- arbitrary
        let txOut' = ensureMinCoinTxOut pp (txOut & coinTxOutL .~ amount)
        logDoc $
          "Fixed up the amount in the TxOut to " <> ansiExpr (txOut' ^. coinTxOutL)
        pure txOut'
      else do
        pure txOut
  pure $ tx & bodyTxL . outputsTxBodyL .~ fixedUpTxOuts

fixupFees ::
  (ShelleyEraImp era, HasCallStack) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
fixupFees txOriginal = impAnn "fixupFees" $ do
  -- Fee will be overwritten later on, unless it wasn't set to zero to begin with:
  let tx = txOriginal & bodyTxL . feeTxBodyL .~ zero
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  addr <- freshKeyAddr_
  nativeScriptKeyPairs <- impNativeScriptKeyPairs tx
  let
    nativeScriptKeyWits = Map.keysSet nativeScriptKeyPairs
    consumedValue = consumed pp certState utxo (tx ^. bodyTxL)
    producedValue = produced pp certState (tx ^. bodyTxL)
    ensureNonNegativeCoin v
      | pointwise (<=) zero v = pure v
      | otherwise = do
          logDoc $ "Failed to validate coin: " <> ansiExpr v
          pure zero
  logString "Validating changeBeforeFee"
  changeBeforeFee <- ensureNonNegativeCoin $ coin consumedValue <-> coin producedValue
  logToExpr changeBeforeFee
  let
    changeBeforeFeeTxOut = mkBasicTxOut addr (inject changeBeforeFee)
    txNoWits = tx & bodyTxL . outputsTxBodyL %~ (:|> changeBeforeFeeTxOut)
    outsBeforeFee = tx ^. bodyTxL . outputsTxBodyL
    suppliedFee = txOriginal ^. bodyTxL . feeTxBodyL
    fee0
      | suppliedFee == zero = calcMinFeeTxNativeScriptWits utxo pp txNoWits nativeScriptKeyWits
      | otherwise = suppliedFee
    fee = rationalToCoinViaCeiling $ coinToRational fee0 * (11 % 10)
  logString "Validating change"
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

-- | Adds an auxiliary data hash if auxiliary data present, while the hash of it is not.
fixupAuxDataHash :: (EraTx era, Applicative m) => Tx l era -> m (Tx l era)
fixupAuxDataHash tx
  | SNothing <- tx ^. bodyTxL . auxDataHashTxBodyL
  , SJust auxData <- tx ^. auxDataTxL =
      pure (tx & bodyTxL . auxDataHashTxBodyL .~ SJust (TxAuxDataHash (hashAnnotated auxData)))
  | otherwise = pure tx

shelleyFixupTx ::
  forall era.
  (ShelleyEraImp era, HasCallStack) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
shelleyFixupTx =
  addNativeScriptTxWits
    >=> fixupAuxDataHash
    >=> addRootTxIn
    >=> fixupTxOuts
    >=> fixupFees
    >=> updateAddrTxWits
    >=> (\tx -> logFeeMismatch tx $> tx)

impShelleyExpectTxSuccess ::
  forall era.
  (ShelleyEraImp era, HasCallStack) =>
  Tx TopTx era ->
  ImpTestM era ()
impShelleyExpectTxSuccess tx = do
  utxo <- getsNES utxoL
  let inputs = tx ^. bodyTxL . inputsTxBodyL
      outputs = Map.toList . unUTxO . txouts $ tx ^. bodyTxL
  impAnn "Inputs should be gone from UTxO" $
    expectUTxOContent utxo [(txIn, isNothing) | txIn <- Set.toList inputs]
  impAnn "Outputs should be in UTxO" $
    expectUTxOContent utxo [(txIn, (== Just txOut)) | (txIn, txOut) <- outputs]

logFeeMismatch :: (EraGov era, EraUTxO era, HasCallStack) => Tx TopTx era -> ImpTestM era ()
logFeeMismatch tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getsNES utxoL
  let Coin feeUsed = tx ^. bodyTxL . feeTxBodyL
      Coin feeMin = getMinFeeTxUtxo pp tx utxo
  when (feeUsed /= feeMin) $ do
    logDoc $
      "Estimated fee " <> ansiExpr feeUsed <> " while required fee is " <> ansiExpr feeMin

-- * Submitting transactions

submitTx_ :: (HasCallStack, ShelleyEraImp era) => Tx TopTx era -> ImpTestM era ()
submitTx_ = void . submitTx

submitTx :: (HasCallStack, ShelleyEraImp era) => Tx TopTx era -> ImpTestM era (Tx TopTx era)
submitTx tx = trySubmitTx tx >>= expectRightDeepExpr . first fst

trySubmitTx ::
  forall era.
  ( ShelleyEraImp era
  , HasCallStack
  ) =>
  Tx TopTx era ->
  ImpTestM
    era
    (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era)), Tx TopTx era) (Tx TopTx era))
trySubmitTx tx = do
  txFixed <- asks iteFixup >>= ($ tx)
  logToExpr txFixed

  st <- gets impNES
  lEnv <- impLedgerEnv st
  res <- tryRunImpRule @"LEDGER" lEnv (st ^. nesEsL . esLStateL) txFixed

  -- Check for conformance
  globals <- use impGlobalsL
  let trc = TRC (lEnv, st ^. nesEsL . esLStateL, txFixed)
  asks itePostSubmitTxHook >>= (\f -> f globals trc res)

  case res of
    Left predFailures -> do
      -- Verify that produced predicate failures are ready for the node-to-client protocol
      liftIO $ forM_ predFailures $ roundTripEraExpectation @era
      pure $ Left (predFailures, txFixed)
    Right (newState, events) -> do
      impNESL . nesEsL . esLStateL .= newState
      tell . Seq.fromList $ SomeSTSEvent @era @"LEDGER" <$> events

      modify' $ impRecordedTxsL %~ fmap (SSeq.|> txFixed)

      ImpTestState {impRootTxIn} <- get
      UTxO utxo <- getUTxO
      let
        txId = TxId . hashAnnotated $ txFixed ^. bodyTxL
        outsSize = SSeq.length $ txFixed ^. bodyTxL . outputsTxBodyL
        rootIndex
          | outsSize > 0 = outsSize - 1
          | otherwise = error ("Expected at least 1 output after submitting tx: " <> show txId)
        -- This TxIn is in the utxo, and thus can be the new root, only if the transaction
        -- was phase2-valid.  Otherwise, no utxo with this id would have been created, and
        -- so we need to set the new root to what it was before the submission.
        assumedNewRoot = TxIn txId (mkTxIxPartial (fromIntegral rootIndex))
        newRoot
          | Map.member assumedNewRoot utxo = assumedNewRoot
          | Map.member impRootTxIn utxo = impRootTxIn
          | otherwise = error "Root not found in UTxO"
      impRootTxInL .= newRoot

      expectTxSuccess txFixed

      pure $ Right txFixed

-- | Submit a transaction that is expected to be rejected with the given predicate failures.
-- The inputs and outputs are automatically balanced.
submitFailingTx ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx TopTx era ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingTx tx = submitFailingTxM tx . const . pure

-- | Submit a transaction that is expected to be rejected, and compute
-- the expected predicate failures from the fixed-up tx using the supplied action.
-- The inputs and outputs are automatically balanced.
submitFailingTxM ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx TopTx era ->
  (Tx TopTx era -> ImpTestM era (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))) ->
  ImpTestM era ()
submitFailingTxM tx mkExpectedFailures = do
  (predFailures, fixedUpTx) <- expectLeftDeepExpr =<< trySubmitTx tx
  expectedFailures <- mkExpectedFailures fixedUpTx
  predFailures `shouldBeExpr` expectedFailures

-- * Submitting blocks

-- | Submit a list of transactions as a block that's expected to succeed.
-- The inputs and outputs are automatically balanced.
submitBlock_ ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  [Tx TopTx era] ->
  ImpTestM era ()
submitBlock_ = withTxsInBlock_ . traverse_ submitTx_

-- | Submit a list of transactions as a block that's expected to succeed.
-- The inputs and outputs are automatically balanced.
submitBlock ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  [Tx TopTx era] ->
  ImpTestM era (Block BHeaderView era)
submitBlock = withTxsInBlock . traverse_ submitTx_

-- | Submit a list of transactions as a block that's expected to fail
-- with the given predicate failures.
-- The inputs and outputs are automatically balanced.
submitFailingBlock ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  [Tx TopTx era] ->
  NonEmpty (PredicateFailure (EraRule "BBODY" era)) ->
  ImpTestM era ()
submitFailingBlock = withTxsInFailingBlock . traverse_ submitTx_

-- | Submit a list of transactions as a block that's expected to be rejected,
-- and compute the expected predicate failures from the created block using the supplied action.
-- The inputs and outputs are automatically balanced.
submitFailingBlockM ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  [Tx TopTx era] ->
  (Block BHeaderView era -> ImpTestM era (NonEmpty (PredicateFailure (EraRule "BBODY" era)))) ->
  ImpTestM era ()
submitFailingBlockM = withTxsInFailingBlockM . traverse_ submitTx_

-- | Gather all the txs submitted by @act@ and resubmit them as a block that's expected to succeed.
withTxsInBlock_ ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era a ->
  ImpTestM era ()
withTxsInBlock_ = void . withTxsInBlock . void

-- | Gather all the txs submitted by @act@ and resubmit them as a block that's expected to succeed.
withTxsInBlock ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era () ->
  ImpTestM era (Block BHeaderView era)
withTxsInBlock = expectRightDeepExpr <=< withTxsInBlockEither

-- | Gather all the txs submitted by @act@ and resubmit them as a block
-- that's expected to fail with the given predicate failures.
withTxsInFailingBlock ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era () ->
  NonEmpty (PredicateFailure (EraRule "BBODY" era)) ->
  ImpTestM era ()
withTxsInFailingBlock act = withTxsInFailingBlockM act . const . pure

-- | Gather all the txs submitted by @act@ and resubmit them as a block that's expected to fail
-- and compute the expected predicate failures from the created block using the supplied action.
withTxsInFailingBlockM ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era () ->
  (Block BHeaderView era -> ImpTestM era (NonEmpty (PredicateFailure (EraRule "BBODY" era)))) ->
  ImpTestM era ()
withTxsInFailingBlockM act mkExpectedFailures = do
  (predFailures, block) <- expectLeftDeepExpr <=< withTxsInBlockEither $ act
  expectedFailures <- mkExpectedFailures block
  predFailures `shouldBeExpr` expectedFailures

-- | Given an action that submits transactions, try to resubmit the transactions as a block.
-- Return the block that was created using the transactions and any predicate
-- failures that are produced.
withTxsInBlockEither ::
  forall era.
  ShelleyEraImp era =>
  ImpTestM era () ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule "BBODY" era)), Block BHeaderView era)
        (Block BHeaderView era)
    )
withTxsInBlockEither act = do
  stateBefore <- get
  txs <- impRecordSubmittedTxs act
  stateAfter <- get
  put stateBefore
  tryTxsInBlock txs stateAfter

-- | Given a sequence of fixed-up transactions and an expected final test state,
-- try to submit the transactions as a block.
-- Return the block that was created using the transactions and any predicate
-- failures that are produced.
tryTxsInBlock ::
  forall era.
  ShelleyEraImp era =>
  StrictSeq (Tx TopTx era) ->
  ImpTestState era ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule "BBODY" era)), Block BHeaderView era)
        (Block BHeaderView era)
    )
tryTxsInBlock txs finalState = do
  blockIssuer <- freshKeyHash
  slotNo <- use impCurSlotNoG

  let
    blockBody = mkBasicBlockBody @era & txSeqBlockBodyL .~ txs
    blockHeader =
      BHeaderView
        { bhviewID = blockIssuer
        , bhviewBSize = fromIntegral $ bBodySize (ProtVer (eraProtVerLow @era) 0) blockBody
        , bhviewHSize = 0
        , bhviewBHash = hashBlockBody blockBody
        , bhviewSlot = slotNo
        , bhviewPrevEpochNonce = Nothing
        }
    block = Block {blockHeader, blockBody}

  globals <- use impGlobalsL
  nes <- use impNESL

  let res = applyBlockEither EPReturn ValidateAll globals nes block

  case res of
    Left (BlockTransitionError predFailures) -> do
      -- Verify that produced predicate failures are ready for the node-to-client protocol
      liftIO $ forM_ predFailures $ roundTripEraExpectation @era
      pure $ Left (predFailures, block)
    Right (blockNes, events) -> do
      previousEvents <- use impEventsL
      let newEvents = SomeSTSEvent @era @"BBODY" <$> Seq.fromList events
          blockEvents = previousEvents <> newEvents
      put $
        finalState
          & impNESL .~ blockNes
          & impEventsL .~ blockEvents

      pure $ Right block

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
tryRunImpRule = tryRunImpRule' @rule AssertionsAll

tryRunImpRuleNoAssertions ::
  forall rule era.
  ( STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  ) =>
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (State (EraRule rule era), [Event (EraRule rule era)])
    )
tryRunImpRuleNoAssertions = tryRunImpRule' @rule AssertionsOff

tryRunImpRule' ::
  forall rule era.
  (STS (EraRule rule era), BaseM (EraRule rule era) ~ ShelleyBase) =>
  AssertionPolicy ->
  Environment (EraRule rule era) ->
  State (EraRule rule era) ->
  Signal (EraRule rule era) ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule rule era)))
        (State (EraRule rule era), [Event (EraRule rule era)])
    )
tryRunImpRule' assertionPolicy stsEnv stsState stsSignal = do
  let trc = TRC (stsEnv, stsState, stsSignal)
  let
    stsOpts =
      ApplySTSOpts
        { asoValidation = ValidateAll
        , asoEvents = EPReturn
        , asoAssertions = assertionPolicy
        }
  runShelleyBase (applySTSOptsEither @(EraRule rule era) stsOpts trc)

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
runImpRule env st sig = do
  let ruleName = symbolVal (Proxy @rule)
  (res, ev) <-
    tryRunImpRule @rule env st sig >>= \case
      Left fs ->
        assertFailure $
          unlines $
            ("Failed to run " <> ruleName <> ":") : map show (toList fs)
      Right res -> evaluateDeep res
  tell . Seq.fromList $ SomeSTSEvent @era @rule <$> ev
  pure res

-- | Runs the TICK rule once
passTick ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era ()
passTick = do
  impCurSlotNo <- gets impCurSlotNo
  curNES <- getsNES id
  nes <- runImpRule @"TICK" () curNES impCurSlotNo
  impCurSlotNoL += 1
  impNESL .= nes

-- | Win with supplied probability
drawBoolWithProbability ::
  HasStatefulGen g m =>
  -- | Probability with which this action should produce `True`
  UnitInterval ->
  m Bool
drawBoolWithProbability probability = do
  let p = unboundRational probability
  n <- uniformRM (1, denominator p)
  pure (n <= numerator p)

-- | Runs the TICK rule until the next epoch is reached
passEpoch ::
  forall era.
  (ShelleyEraImp era, HasCallStack) =>
  ImpTestM era ()
passEpoch = do
  globals <- use impGlobalsL
  preNES <- gets impNES
  let
    curEpochNo = preNES ^. nesELL
    ticksPerSlot =
      positiveUnitIntervalRelaxToUnitInterval (activeSlotVal (activeSlotCoeff globals))
    tickUntilNewEpoch = do
      tickHasBlock <- drawBoolWithProbability ticksPerSlot
      if tickHasBlock
        then do
          oldNES <- getsNES id
          passTick @era
          newEpochNo <- getsNES nesELL
          if newEpochNo > curEpochNo
            then do
              newNES <- getsNES id
              asks itePostEpochBoundaryHook >>= (\f -> f globals (TRC ((), oldNES, newEpochNo)) newNES)
            else tickUntilNewEpoch
        else do
          impCurSlotNoL += 1
          tickUntilNewEpoch
  logDoc $ "Entering " <> ansiExpr (succ curEpochNo)
  tickUntilNewEpoch
  gets impNES >>= epochBoundaryCheck preNES

epochBoundaryCheck ::
  (EraTxOut era, EraGov era, HasCallStack, EraCertState era) =>
  NewEpochState era ->
  NewEpochState era ->
  ImpTestM era ()
epochBoundaryCheck preNES postNES = do
  impAnn "Checking ADA preservation at the epoch boundary" $ do
    let preSum = tot preNES
        postSum = tot postNES
    logDoc $ diffExpr preSum postSum
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
passNEpochs n =
  impAnn ("Passing " <> show n <> " epochs") $
    forM_ ([1 .. n] :: [Natural]) $ \i ->
      impAnn ("Passing epoch (" <> show i <> ")") $ passEpoch

-- | Runs the TICK rule until the `n` epochs are passed, running the `checks`
-- each time.
passNEpochsChecking ::
  forall era.
  ShelleyEraImp era =>
  Natural ->
  ImpTestM era () ->
  ImpTestM era ()
passNEpochsChecking n checks =
  replicateM_ (fromIntegral n) $ passEpoch >> checks

-- | Adds a ToExpr to the log, which is only shown if the test fails
logToExpr :: (HasCallStack, ToExpr a) => a -> ImpM t ()
logToExpr = logWithCallStack ?callStack . ansiWlExpr . toExpr

-- | Adds the result of an action to the log, which is only shown if the test fails
impLogToExpr :: (HasCallStack, ToExpr a) => ImpTestM era a -> ImpTestM era a
impLogToExpr action = do
  e <- action
  logWithCallStack ?callStack . ansiWlExpr . toExpr $ e
  pure e

-- | Creates a fresh @SafeHash@
freshSafeHash :: ImpTestM era (SafeHash a)
freshSafeHash = arbitrary

freshKeyHashVRF ::
  ImpTestM era (VRFVerKeyHash (r :: KeyRoleVRF))
freshKeyHashVRF = arbitrary

-- | Adds a key pair to the keyhash lookup map
addKeyPair ::
  (HasKeyPairs s, MonadState s m) =>
  KeyPair r ->
  m (KeyHash r)
addKeyPair keyPair@(KeyPair vk _) = do
  let keyHash = hashKey vk
  modify $ keyPairsL %~ Map.insert (coerceKeyRole keyHash) (coerce keyPair)
  pure keyHash

-- | Looks up the `KeyPair` corresponding to the `KeyHash`. The `KeyHash` must be
-- created with `freshKeyHash` for this to work.
getKeyPair ::
  (HasCallStack, HasKeyPairs s, MonadState s m) =>
  KeyHash r ->
  m (KeyPair r)
getKeyPair keyHash = do
  keyPairs <- use keyPairsL
  case Map.lookup (asWitness keyHash) keyPairs of
    Just keyPair -> pure $ coerce keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show keyHash
          ++ "\nAlways use `freshKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `KeyPair` in the
-- ImpTestState. If you also need the `KeyPair` consider using `freshKeyPair` for
-- generation or `getKeyPair` to look up the `KeyPair` corresponding to the `KeyHash`
freshKeyHash ::
  forall r s g m.
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r)
freshKeyHash = fst <$> freshKeyPair

-- | Generate a random `KeyPair` and add it to the known keys in the Imp state
freshKeyPair ::
  forall r s g m.
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r, KeyPair r)
freshKeyPair = do
  keyPair <- uniformM
  keyHash <- addKeyPair keyPair
  pure (keyHash, keyPair)

-- | Generate a random `Addr` that uses a `KeyHash`, and add the corresponding `KeyPair`
-- to the known keys in the Imp state.
freshKeyAddr_ ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) => m Addr
freshKeyAddr_ = snd <$> freshKeyAddr

-- | Generate a random `Addr` that uses a `KeyHash`, add the corresponding `KeyPair`
-- to the known keys in the Imp state, and return the `KeyHash` as well as the `Addr`.
freshKeyAddrPtr ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  Bool ->
  m (KeyHash Payment, Addr)
freshKeyAddrPtr ptrAllowed = do
  paymentKeyHash <- freshKeyHash @Payment
  stakingKeyHash <-
    oneof $
      [Just . mkStakeRef <$> freshKeyHash @Staking, pure Nothing]
        <> [Just . mkStakeRef @Ptr <$> arbitrary | ptrAllowed]
  pure (paymentKeyHash, mkAddr paymentKeyHash stakingKeyHash)

freshKeyAddr ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  m (KeyHash Payment, Addr)
freshKeyAddr = freshKeyAddrPtr True

-- | Generate a random `Addr` that uses a `KeyHash`, add the corresponding `KeyPair`
-- to the known keys in the Imp state, and return the `KeyHash` as well as the `Addr`.
-- It will never generate a Ptr stake reference.
freshKeyAddrNoPtr ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  m (KeyHash Payment, Addr)
freshKeyAddrNoPtr = freshKeyAddrPtr False

freshKeyAddrNoPtr_ ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m, MonadGen m) =>
  m Addr
freshKeyAddrNoPtr_ = snd <$> freshKeyAddrNoPtr

-- | Looks up the keypair corresponding to the `BootstrapAddress`. The `BootstrapAddress`
-- must be created with `freshBootstrapAddess` for this to work.
getByronKeyPair ::
  (HasCallStack, HasKeyPairs s, MonadState s m) =>
  BootstrapAddress ->
  m ByronKeyPair
getByronKeyPair bootAddr = do
  keyPairs <- use keyPairsByronL
  case Map.lookup bootAddr keyPairs of
    Just keyPair -> pure keyPair
    Nothing ->
      error $
        "Could not find a keypair corresponding to: "
          ++ show bootAddr
          ++ "\nAlways use `freshByronKeyHash` to create key hashes."

-- | Generates a fresh `KeyHash` and stores the corresponding `ByronKeyPair` in the
-- ImpTestState. If you also need the `ByronKeyPair` consider using `freshByronKeyPair` for
-- generation or `getByronKeyPair` to look up the `ByronKeyPair` corresponding to the `KeyHash`
freshByronKeyHash ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m) =>
  m (KeyHash r)
freshByronKeyHash = coerceKeyRole . bootstrapKeyHash <$> freshBootstapAddress

freshBootstapAddress ::
  (HasKeyPairs s, MonadState s m, HasStatefulGen g m) =>
  m BootstrapAddress
freshBootstapAddress = do
  keyPair@(ByronKeyPair verificationKey _) <- uniformM
  hasPayload <- uniformM
  payload <-
    if hasPayload
      then Just . Byron.HDAddressPayload <$> (uniformByteStringM =<< uniformRM (0, 63))
      else pure Nothing
  let asd = Byron.VerKeyASD verificationKey
      attrs = Byron.AddrAttributes payload (Byron.NetworkTestnet 0)
      bootAddr = BootstrapAddress $ Byron.makeAddress asd attrs
  modify $ keyPairsByronL %~ Map.insert bootAddr keyPair
  pure bootAddr

sendCoinTo :: (ShelleyEraImp era, HasCallStack) => Addr -> Coin -> ImpTestM era TxIn
sendCoinTo addr = sendValueTo addr . inject

sendCoinTo_ :: (ShelleyEraImp era, HasCallStack) => Addr -> Coin -> ImpTestM era ()
sendCoinTo_ addr = void . sendCoinTo addr

sendValueTo :: (ShelleyEraImp era, HasCallStack) => Addr -> Value era -> ImpTestM era TxIn
sendValueTo addr amount = do
  tx <-
    submitTxAnn
      ("Giving " <> show amount <> " to " <> show addr)
      $ mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr amount)
  pure $ txInAt 0 tx

sendValueTo_ :: (ShelleyEraImp era, HasCallStack) => Addr -> Value era -> ImpTestM era ()
sendValueTo_ addr = void . sendValueTo addr

-- | Modify the current new epoch state with a function
modifyNES :: (NewEpochState era -> NewEpochState era) -> ImpTestM era ()
modifyNES = (impNESL %=)

-- | Get a value from the current new epoch state using the lens
getsNES :: SimpleGetter (NewEpochState era) a -> ImpTestM era a
getsNES l = gets . view $ impNESL . l

getUTxO :: ImpTestM era (UTxO era)
getUTxO = getsNES utxoL

getProtVer :: EraGov era => ImpTestM era ProtVer
getProtVer = getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL

submitTxAnn ::
  (HasCallStack, ShelleyEraImp era) =>
  String ->
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
submitTxAnn msg tx = impAnn msg (trySubmitTx tx >>= expectRightDeepExpr)

submitTxAnn_ ::
  (HasCallStack, ShelleyEraImp era) => String -> Tx TopTx era -> ImpTestM era ()
submitTxAnn_ msg = void . submitTxAnn msg

getRewardAccountFor ::
  Credential Staking ->
  ImpTestM era RewardAccount
getRewardAccountFor stakingC = do
  networkId <- use (impGlobalsL . to networkId)
  pure $ RewardAccount networkId stakingC

registerStakeCredential ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Credential Staking ->
  ImpTestM era RewardAccount
registerStakeCredential cred = do
  regTxCert <- genRegTxCert cred
  submitTxAnn_ ("Register Reward Account: " <> T.unpack (credToText cred)) $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList [regTxCert]
  networkId <- use (impGlobalsL . to networkId)
  pure $ RewardAccount networkId cred

delegateStake ::
  ShelleyEraImp era =>
  Credential Staking ->
  KeyHash StakePool ->
  ImpTestM era ()
delegateStake cred poolKH = do
  submitTxAnn_ ("Delegate Staking Credential: " <> T.unpack (credToText cred)) $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ [delegStakeTxCert cred poolKH]

expectStakeCredRegistered ::
  (HasCallStack, ShelleyEraImp era) =>
  Credential Staking ->
  ImpTestM era ()
expectStakeCredRegistered cred = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  accountState <- expectJust $ lookupAccountState cred accounts
  impAnn (show cred <> " expected to be in Accounts with the correct deposit") $ do
    accountState ^. depositAccountStateL `shouldBe` compactCoinOrError expectedDeposit

expectStakeCredNotRegistered ::
  (HasCallStack, ShelleyEraImp era) =>
  Credential Staking ->
  ImpTestM era ()
expectStakeCredNotRegistered cred = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  impAnn (show cred <> " expected to not be in Accounts") $ do
    expectNothingExpr $ lookupAccountState cred accounts

expectDelegatedToPool ::
  (HasCallStack, ShelleyEraImp era) =>
  Credential Staking ->
  KeyHash StakePool ->
  ImpTestM era ()
expectDelegatedToPool cred poolKh = do
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  let accounts = certState ^. certDStateL . accountsL
  let pools = certState ^. certPStateL . psStakePoolsL
  impAnn (show cred <> " expected to have delegated to " <> show poolKh) $ do
    accountState <- expectJust $ lookupAccountState cred accounts
    accountState ^. stakePoolDelegationAccountStateL `shouldBe` Just poolKh
    case Map.lookup poolKh pools of
      Nothing ->
        assertFailure $
          "Expected stake pool state for: " <> show poolKh
      Just poolState ->
        assertBool
          ("Expected pool delegations to contain the stake credential: " <> show cred)
          (cred `Set.member` (poolState ^. spsDelegatorsL))

expectNotDelegatedToAnyPool ::
  (HasCallStack, ShelleyEraImp era) =>
  Credential Staking ->
  ImpTestM era ()
expectNotDelegatedToAnyPool cred = do
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  let accounts = certState ^. certDStateL . accountsL
  let pools = certState ^. certPStateL . psStakePoolsL
  impAnn (show cred <> " expected to not have delegated to a stake pool") $ do
    forM_ (lookupAccountState cred accounts) $ \accountState ->
      expectNothingExpr (accountState ^. stakePoolDelegationAccountStateL)
    assertBool
      ("Expected no stake pool state delegation to contain the stake credential: " <> show cred)
      (all (Set.notMember cred . spsDelegators) pools)

expectNotDelegatedToPool ::
  (HasCallStack, ShelleyEraImp era) =>
  Credential Staking ->
  KeyHash StakePool ->
  ImpTestM era ()
expectNotDelegatedToPool cred pool = do
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  let accounts = certState ^. certDStateL . accountsL
  let pools = certState ^. certPStateL . psStakePoolsL
  impAnn (show cred <> " expected to not have delegated to a stake pool") $ do
    forM_ (lookupAccountState cred accounts) $ \accountState ->
      accountState ^. stakePoolDelegationAccountStateL `shouldNotBe` Just pool
    assertBool
      ("Expected stake pool state delegation to not contain the stake credential: " <> show cred)
      (maybe True (Set.notMember cred . spsDelegators) (Map.lookup pool pools))

registerRewardAccount ::
  forall era.
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  ImpTestM era RewardAccount
registerRewardAccount = freshKeyHash >>= registerStakeCredential . KeyHashObj

freshPoolParams ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  RewardAccount ->
  ImpTestM era StakePoolParams
freshPoolParams khPool rewardAccount = do
  vrfHash <- freshKeyHashVRF
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let minCost = pp ^. ppMinPoolCostL
  poolCostExtra <- uniformRM (Coin 0, Coin 100_000_000)
  pledge <- uniformRM (Coin 0, Coin 100_000_000)
  pure
    StakePoolParams
      { sppVrf = vrfHash
      , sppRewardAccount = rewardAccount
      , sppRelays = mempty
      , sppPledge = pledge
      , sppOwners = mempty
      , sppMetadata = SNothing
      , sppMargin = def
      , sppId = khPool
      , sppCost = minCost <> poolCostExtra
      }

registerPool ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  ImpTestM era ()
registerPool khPool = registerRewardAccount >>= registerPoolWithRewardAccount khPool

registerPoolWithRewardAccount ::
  ShelleyEraImp era =>
  KeyHash StakePool ->
  RewardAccount ->
  ImpTestM era ()
registerPoolWithRewardAccount khPool rewardAccount = do
  pps <- freshPoolParams khPool rewardAccount
  submitTxAnn_ "Registering a new stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert pps)

registerAndRetirePoolToMakeReward ::
  ShelleyEraImp era =>
  Credential Staking ->
  ImpTestM era ()
registerAndRetirePoolToMakeReward stakingCred = do
  poolId <- freshKeyHash
  registerPoolWithRewardAccount poolId =<< getRewardAccountFor stakingCred
  passEpoch
  curEpochNo <- getsNES nesELL
  let poolLifetime = 2
      poolExpiry = addEpochInterval curEpochNo $ EpochInterval poolLifetime
  submitTxAnn_ "Retiring the temporary stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL .~ SSeq.singleton (RetirePoolTxCert poolId poolExpiry)
  passNEpochs $ fromIntegral poolLifetime

-- | Compose given function with the configured fixup
withCustomFixup ::
  ((Tx TopTx era -> ImpTestM era (Tx TopTx era)) -> Tx TopTx era -> ImpTestM era (Tx TopTx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withCustomFixup f = local $ iteFixupL %~ f

-- | Replace all fixup with the given function
withFixup ::
  (Tx TopTx era -> ImpTestM era (Tx TopTx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withFixup f = withCustomFixup (const f)

-- | Performs the action without running the fix-up function on any transactions
withNoFixup :: ImpTestM era a -> ImpTestM era a
withNoFixup = withFixup pure

-- | Apply given fixup function before the configured fixup
withPreFixup ::
  (Tx TopTx era -> ImpTestM era (Tx TopTx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withPreFixup f = withCustomFixup (f >=>)

-- | Apply given fixup function after the configured fixup
withPostFixup ::
  (Tx TopTx era -> ImpTestM era (Tx TopTx era)) ->
  ImpTestM era a ->
  ImpTestM era a
withPostFixup f = withCustomFixup (>=> f)

expectUTxOContent ::
  (HasCallStack, ToExpr (TxOut era)) =>
  UTxO era ->
  [(TxIn, Maybe (TxOut era) -> Bool)] ->
  ImpTestM era ()
expectUTxOContent utxo = traverse_ $ \(txIn, test) -> do
  let result = txIn `Map.lookup` unUTxO utxo
  unless (test result) $
    expectationFailure $
      "UTxO content failed predicate:\n" <> ansiExprString txIn <> " -> " <> ansiExprString result

expectRegisteredRewardAddress ::
  (HasCallStack, EraCertState era) => RewardAccount -> ImpTestM era ()
expectRegisteredRewardAddress ra@RewardAccount {raNetwork, raCredential} = do
  networkId <- use (impGlobalsL . to networkId)
  unless (raNetwork == networkId) $
    assertFailure $
      "Reward Account with an unexpected NetworkId: " ++ show ra
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  unless (isAccountRegistered raCredential accounts) $
    assertFailure $
      "Expected account "
        ++ show ra
        ++ " to be registered, but it is not."

expectNotRegisteredRewardAddress ::
  (HasCallStack, EraCertState era) => RewardAccount -> ImpTestM era ()
expectNotRegisteredRewardAddress ra@RewardAccount {raNetwork, raCredential} = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  networkId <- use (impGlobalsL . to networkId)
  when (raNetwork == networkId && isAccountRegistered raCredential accounts) $
    assertFailure $
      "Expected account "
        ++ show ra
        ++ " to not be registered, but it is."

expectTreasury :: HasCallStack => Coin -> ImpTestM era ()
expectTreasury c =
  impAnn "Checking treasury amount" $ do
    treasuryAmount <- getsNES treasuryL
    c `shouldBe` treasuryAmount

-- Ensure no fees reach the treasury since that complicates withdrawal checks
disableTreasuryExpansion :: ShelleyEraImp era => ImpTestM era ()
disableTreasuryExpansion = modifyPParams $ ppTauL .~ (0 %! 1)

impLookupNativeScript :: ScriptHash -> ImpTestM era (Maybe (NativeScript era))
impLookupNativeScript sh = Map.lookup sh <$> gets impNativeScripts

impGetUTxO :: ShelleyEraImp era => TxIn -> ImpTestM era (TxOut era)
impGetUTxO txIn = impAnn "Looking up TxOut" $ do
  utxo <- getUTxO
  case txinLookup txIn utxo of
    Just txOut -> pure txOut
    Nothing -> error $ "Failed to get TxOut for " <> show txIn

produceScript ::
  (ShelleyEraImp era, HasCallStack) =>
  ScriptHash ->
  ImpTestM era TxIn
produceScript scriptHash = do
  let addr = mkAddr scriptHash StakeRefNull
  let tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr mempty)
  logString $ "Produced script: " <> show scriptHash
  txInAt 0 <$> submitTx tx

advanceToPointOfNoReturn :: ImpTestM era ()
advanceToPointOfNoReturn = do
  impCurSlotNo <- gets impCurSlotNo
  (_, slotOfNoReturn, _) <- runShelleyBase $ getTheSlotOfNoReturn impCurSlotNo
  impCurSlotNoL .= slotOfNoReturn

-- | A legal ProtVer that differs in the minor Version
minorFollow :: ProtVer -> ProtVer
minorFollow (ProtVer x y) = ProtVer x (y + 1)

-- | A legal ProtVer that moves to the next major Version
majorFollow :: ProtVer -> ProtVer
majorFollow pv@(ProtVer x _) = case succVersion x of
  Just x' -> ProtVer x' 0
  Nothing -> error ("The last major version can't be incremented. " ++ show pv)

-- | An illegal ProtVer that skips 3 minor versions
cantFollow :: ProtVer -> ProtVer
cantFollow (ProtVer x y) = ProtVer x (y + 3)

whenMajorVersion ::
  forall (v :: Natural) era.
  ( EraGov era
  , KnownNat v
  , MinVersion <= v
  , v <= MaxVersion
  ) =>
  ImpTestM era () ->
  ImpTestM era ()
whenMajorVersion a = do
  pv <- getProtVer
  when (pvMajor pv == natVersion @v) a

whenMajorVersionAtLeast ::
  forall (v :: Natural) era.
  ( EraGov era
  , KnownNat v
  , MinVersion <= v
  , v <= MaxVersion
  ) =>
  ImpTestM era () ->
  ImpTestM era ()
whenMajorVersionAtLeast a = do
  pv <- getProtVer
  when (pvMajor pv >= natVersion @v) a

whenMajorVersionAtMost ::
  forall (v :: Natural) era.
  ( EraGov era
  , KnownNat v
  , MinVersion <= v
  , v <= MaxVersion
  ) =>
  ImpTestM era () ->
  ImpTestM era ()
whenMajorVersionAtMost a = do
  pv <- getProtVer
  when (pvMajor pv <= natVersion @v) a

unlessMajorVersion ::
  forall (v :: Natural) era.
  ( EraGov era
  , KnownNat v
  , MinVersion <= v
  , v <= MaxVersion
  ) =>
  ImpTestM era () ->
  ImpTestM era ()
unlessMajorVersion a = do
  pv <- getProtVer
  unless (pvMajor pv == natVersion @v) a

getsPParams :: EraGov era => Lens' (PParams era) a -> ImpTestM era a
getsPParams f = getsNES $ nesEsL . curPParamsEpochStateL . f

-- | Runs a simulation action and then restores the ledger state to what it was
-- before the simulation started.
-- This is useful for testing or running actions whose effects on the ledger
-- state should not persist. The return value of the simulation is preserved,
-- but any changes to the internal state (e.g., the UTxO set, protocol parameters,
-- etc.) are discarded and replaced with the original snapshot.
simulateThenRestore ::
  ImpTestM era a ->
  ImpTestM era a
simulateThenRestore sim = do
  snapshot <- get
  result <- sim
  put snapshot
  pure result

shelleyGenRegTxCert ::
  ShelleyEraTxCert era =>
  Credential Staking ->
  ImpTestM era (TxCert era)
shelleyGenRegTxCert = pure . RegTxCert

shelleyGenUnRegTxCert ::
  ShelleyEraTxCert era =>
  Credential Staking ->
  ImpTestM era (TxCert era)
shelleyGenUnRegTxCert = pure . UnRegTxCert

shelleyDelegStakeTxCert ::
  ShelleyEraTxCert era =>
  Credential Staking ->
  KeyHash StakePool ->
  TxCert era
shelleyDelegStakeTxCert = DelegStakeTxCert
