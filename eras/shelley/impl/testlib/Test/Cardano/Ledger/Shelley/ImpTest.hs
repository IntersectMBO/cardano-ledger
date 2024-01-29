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
  ImpTestState,
  ImpTestEnv (..),
  ImpException (..),
  ShelleyEraImp (..),
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
  constitutionShouldBe,
  withImpState,
  fixupFees,
  fixupTx,
  lookupImpRootTxOut,
  sendValueTo,
  sendCoinTo,
  dispenseIndex,
  expectRegisteredRewardAddress,
  expectNotRegisteredRewardAddress,
  expectTreasury,

  -- * Logging
  logEntry,
  logToExpr,
  logStakeDistr,

  -- * Combinators
  withNoFixup,

  -- * Lenses

  -- We only export getters, because internal state should not be accessed during testing
  impNESG,
  impLastTickG,
  impKeyPairsG,
  impScriptsG,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash, HashAlgorithm)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
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
  inject,
  textToUrl,
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
  utxosGovStateL,
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
import Control.State.Transition (STS (..), TRC (..))
import Control.State.Transition.Trace (applySTSTest)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Default.Class (Default (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
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
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkKeyHash, mkKeyPair, mkWitnessesVKey)
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
  , impRootTxId :: !(TxId (EraCrypto era))
  , impFreshIdx :: !Integer
  , impKeyPairs :: !(forall k. Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
  , impScripts :: !(Map (ScriptHash (EraCrypto era)) (Script era))
  , impLastTick :: !SlotNo
  , impGlobals :: !Globals
  , impLog :: !(Doc ())
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

impRootTxIdL :: Lens' (ImpTestState era) (TxId (EraCrypto era))
impRootTxIdL = lens impRootTxId (\x y -> x {impRootTxId = y})

impKeyPairsG ::
  SimpleGetter
    (ImpTestState era)
    (Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
impKeyPairsG = to impKeyPairs

impScriptsL :: Lens' (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (Script era))
impScriptsL = lens impScripts (\x y -> x {impScripts = y})

impScriptsG :: SimpleGetter (ImpTestState era) (Map (ScriptHash (EraCrypto era)) (Script era))
impScriptsG = impScriptsL

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
    testKeyHash = mkKeyHash (-1)
    rootTxId = TxId (mkDummySafeHash Proxy 0)
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

newtype ImpTestM era a = ImpTestM (ReaderT (ImpTestEnv era) IO a)
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

getRewardAccountAmount :: RewardAccount (EraCrypto era) -> ImpTestM era Coin
getRewardAccountAmount rewardAcount = do
  umap <- getsNES $ nesEsL . epochStateUMapL
  let cred = raCredential rewardAcount
  case UMap.lookup cred (RewDepUView umap) of
    Nothing -> assertFailure $ "Expected a reward account: " ++ show cred
    Just RDPair {rdReward} -> pure $ fromCompact rdReward

lookupImpRootTxOut :: ImpTestM era (TxIn (EraCrypto era), TxOut era)
lookupImpRootTxOut = do
  ImpTestState {impRootTxId} <- get
  let rootTxIn = TxIn impRootTxId $ TxIx 0
  utxo <- getUTxO
  case txinLookup rootTxIn utxo of
    Nothing -> error "Root txId no longer points to an existing unspent output"
    Just rootTxOut -> pure (rootTxIn, rootTxOut)

impAddNativeScript ::
  forall era.
  EraScript era =>
  NativeScript era ->
  ImpTestM era (ScriptHash (EraCrypto era))
impAddNativeScript nativeScript = do
  let script = fromNativeScript nativeScript
      scriptHash = hashScript @era script
  ImpTestState {impScripts} <- get
  impScriptsL .= Map.insert scriptHash script impScripts
  pure scriptHash

-- | Modifies transaction by adding necessary scripts
addScriptTxWits ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era, Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era)))
addScriptTxWits tx = do
  utxo <- getUTxO
  ImpTestState {impScripts} <- get
  -- TODO: Add proper support for Plutus Scripts
  let needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      ScriptsProvided provided = getScriptsProvided utxo tx
      hashesNeeded = getScriptsHashesNeeded needed
      scriptsRequired = impScripts `Map.restrictKeys` hashesNeeded
      scriptsToAdd = scriptsRequired Map.\\ provided
      nativeScripts = mapMaybe getNativeScript $ Map.elems scriptsRequired
  keyPairs <- mapM impSatisfyNativeScript nativeScripts
  pure (tx & witsTxL . scriptTxWitsL <>~ scriptsToAdd, mconcat $ catMaybes keyPairs)

-- | Adds @TxWits@ that will satisfy all of the required key witnesses
addAddrTxWits ::
  ( HasCallStack
  , ShelleyEraImp era
  ) =>
  Tx era ->
  [KeyPair 'Witness (EraCrypto era)] ->
  ImpTestM era (Tx era)
addAddrTxWits tx nativeScriptsKeyPairs = do
  let txBody = tx ^. bodyTxL
      txBodyHash = hashAnnotated txBody
  witsVKeyNeeded <- impWitsVKeyNeeded txBody
  keyPairs <- mapM lookupKeyPair $ Set.toList witsVKeyNeeded
  pure $
    tx & witsTxL . addrTxWitsL <>~ mkWitnessesVKey txBodyHash (keyPairs ++ nativeScriptsKeyPairs)

fixupFees ::
  ShelleyEraImp era =>
  Tx era ->
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  ImpTestM era (Tx era)
fixupFees tx nativeScriptKeyWits = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  kpSpending <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  rootTxIn <- fst <$> lookupImpRootTxOut
  let
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
      txWithRoot & bodyTxL . outputsTxBodyL %~ (changeBeforeFeeTxOut :<|)
    outsBeforeFee = txWithRoot ^. bodyTxL . outputsTxBodyL
    fee = calcMinFeeTxNativeScriptWits utxo pp txNoWits nativeScriptKeyWits
    change = changeBeforeFeeTxOut ^. coinTxOutL <-> fee
    changeTxOut = changeBeforeFeeTxOut & coinTxOutL .~ change
    -- If the remainder is sufficently big we add it to outputs, otherwise we add the
    -- extraneous coin to the fee and discard the remainder TxOut
    txWithFee
      | change >= getMinCoinTxOut pp changeTxOut =
          txNoWits
            & bodyTxL . outputsTxBodyL .~ (changeTxOut :<| outsBeforeFee)
            & bodyTxL . feeTxBodyL .~ fee
      | otherwise =
          txNoWits
            & bodyTxL . outputsTxBodyL .~ outsBeforeFee
            & bodyTxL . feeTxBodyL .~ (fee <> changeTxOut ^. coinTxOutL)
  pure txWithFee

fixupTx ::
  ShelleyEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupTx txRaw = do
  (txWithScriptWits, nativeScriptKeyPairs) <- addScriptTxWits txRaw
  txWithFee <- fixupFees txWithScriptWits (Map.keysSet nativeScriptKeyPairs)
  txFixed <- addAddrTxWits txWithFee (Map.elems nativeScriptKeyPairs)
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let Coin feeUsed = txFixed ^. bodyTxL . feeTxBodyL
      Coin feeMin = getMinFeeTx pp txFixed
  when (feeUsed /= feeMin) $ do
    logEntry $
      "Estimated fee " <> show feeUsed <> " while required fee is " <> show feeMin
  pure txFixed

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
  forM res $ \st' -> do
    modify $ impNESL . nesEsL . esLStateL .~ st'
    let txId = TxId . hashAnnotated $ txFixed ^. bodyTxL
    impRootTxIdL .= txId
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

withImpState :: ShelleyEraImp era => SpecWith (ImpTestState era) -> Spec
withImpState =
  beforeAll $
    pure
      ImpTestState
        { impNES = initImpNES rootCoin
        , impRootTxId = TxId (mkDummySafeHash Proxy 0)
        , impFreshIdx = 1000 -- Added some leeway to prevent collisions with values in the initial state
        , impKeyPairs = mempty
        , impScripts = mempty
        , impLastTick = 0
        , impGlobals = testGlobals
        , impLog = mempty
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
  ImpTestState {impKeyPairs} <- get
  let kp@(KeyPair kh _) = mkKeyPair $ Map.size impKeyPairs
  addKeyPair kp
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
  anchorUrl `shouldBe` fromJust (textToUrl 64 $ T.pack cUrl)

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
