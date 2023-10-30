{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
  ImpException (..),
  EraImpTest (..),
  logStakeDistr,
  emptyShelleyImpNES,
  shelleyImpWitsVKeyNeeded,
  modifyPrevPParams,
  passEpoch,
  passTick,
  itM,
  freshKeyHash,
  freshSafeHash,
  lookupKeyPair,
  submitTx,
  submitFailingTx,
  trySubmitTx,
  impExpectFailure,
  impExpectSuccess,
  logEntry,
  modifyNES,
  getsNES,
  impIO,
  impIOMsg,
  mkTxWits,
  impNESL,
  constitutionShouldBe,
  predicateFailureShouldBe,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Address (Addr (..))
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
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  Era (..),
  EraIndependentTxBody,
  EraRule,
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  PParams,
  ProtVerAtMost,
  emptyPParams,
  setMinFeeTx,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
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
import Cardano.Ledger.Shelley.Core (Constitution (..), EraGov (..), ShelleyEraTxBody)
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
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), sumAllCoin)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Control.Exception (Exception (..), SomeException (..), throwIO)
import Control.Monad (void)
import Control.Monad.State.Strict (
  MonadState (..),
  MonadTrans (..),
  StateT (..),
  execStateT,
  gets,
  modify,
 )
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Control.Monad.Trans.State.Strict as M
import Control.State.Transition (STS (..), TRC (..))
import Control.State.Transition.Trace (applySTSTest)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Default.Class (Default (..))
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Set as Set
import qualified Data.Text as T
import Lens.Micro (Lens', SimpleGetter, lens, to, (%~), (&), (.~), (^.))
import Lens.Micro.Mtl (view, (%=), (+=), (.=))
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutPretty, line)
import Prettyprinter.Render.String (renderString)
import Test.Cardano.Ledger.Binary.TreeDiff (showExpr)
import Test.Cardano.Ledger.Common (HasCallStack, Spec, it, shouldBe)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkKeyHash, mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals)
import UnliftIO (catchAnyDeep)

data ImpTestState era = ImpTestState
  { impNES :: !(NewEpochState era)
  , impRootTxCoin :: !Coin
  , impRootTxId :: !(TxId (EraCrypto era))
  , impSafeHashIdx :: !Int
  , impKeyPairs :: !(forall k. Map (KeyHash k (EraCrypto era)) (KeyPair k (EraCrypto era)))
  , impLastTick :: !SlotNo
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
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , STS (EraRule "LEDGER" era)
  , Signable
      (DSIGN (EraCrypto era))
      (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , EraUTxO era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , EraGov era
  , BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , STS (EraRule "TICK" era)
  , NFData (PredicateFailure (EraRule "TICK" era))
  , NFData (StashedAVVMAddresses era)
  ) =>
  EraImpTest era
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
  EraImpTest (ShelleyEra c)
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

newtype ImpTestM era a = ImpTestM (M.StateT (ImpTestState era) IO a)
  deriving (Functor, Applicative, Monad, MonadState (ImpTestState era))

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

fixupFees ::
  forall era.
  EraImpTest era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupFees tx = do
  ImpTestState {impRootTxId, impRootTxCoin} <- get
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  kpSpending <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  let
    outputsTotalCoin = sumAllCoin $ tx ^. bodyTxL . outputsTxBodyL
    remainingCoin = impRootTxCoin <-> outputsTotalCoin
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
  EraImpTest era =>
  Tx era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (State (EraRule "LEDGER" era), Tx era)
    )
submitTx_ tx = do
  st <- gets impNES
  txFixed <- fixupFees tx
  logEntry $ showExpr txFixed
  lEnv <- impLedgerEnv st
  let
    trc =
      TRC
        ( lEnv
        , st ^. nesEsL . esLStateL
        , txFixed
        )
  pure $ (,txFixed) <$> runShelleyBase (applySTSTest @(EraRule "LEDGER" era) trc)

trySubmitTx ::
  EraImpTest era =>
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
  , EraImpTest era
  ) =>
  Tx era ->
  ImpTestM era ()
submitFailingTx tx = trySubmitTx tx >>= impExpectFailure

-- | Runs the TICK rule once
passTick ::
  forall era.
  ( HasCallStack
  , BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , State (EraRule "TICK" era) ~ NewEpochState era
  , NFData (PredicateFailure (EraRule "TICK" era))
  , NFData (State (EraRule "TICK" era))
  ) =>
  ImpTestM era ()
passTick = do
  impLastTick <- gets impLastTick
  curNES <- getsNES id
  let
    trc = TRC ((), curNES, impLastTick)
  res <-
    impIOMsg "Failed to run TICK" $
      pure $
        runShelleyBase (applySTSTest @(EraRule "TICK" era) trc)
  case res of
    Right !x -> do
      impLastTickL += 1
      impNESL .= x
    Left e ->
      impIO . error $
        unlines $
          "Failed to run TICK:" : map show e

-- | Runs the TICK rule until the next epoch is reached
passEpoch ::
  forall era.
  ( BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , NFData (PredicateFailure (EraRule "TICK" era))
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
data ImpException e = ImpException
  { ieLog :: Doc ()
  -- ^ Log entries up to the point where the test failed
  , ieDescription :: String
  -- ^ Description of the IO action that caused the failure
  , ieThrownException :: e
  -- ^ Exception that caused the test to fail
  }

instance Exception e => Show (ImpException e) where
  show (ImpException msgLog msg e) =
    "Log:\n"
      <> renderString (layoutPretty defaultLayoutOptions msgLog)
      <> "\nFailed at:\n\t"
      <> msg
      <> "\nException:\n\t"
      <> displayException e

instance Exception e => Exception (ImpException e)

-- | Adds a string to the log, which is only shown if the test fails
logEntry :: String -> ImpTestM era ()
logEntry e = impLogL %= (<> pretty e <> line)

-- | Make the `ImpTestM` into a Spec item with the given description
itM ::
  forall era a.
  EraImpTest era =>
  String ->
  ImpTestM era a ->
  Spec
itM desc (ImpTestM m) =
  it desc . void . execStateT m $
    ImpTestState
      { impNES = emptyImpNES rootCoin
      , impRootTxCoin = rootCoin
      , impRootTxId = TxId (mkDummySafeHash Proxy 0)
      , impSafeHashIdx = 0
      , impKeyPairs = mempty
      , impLastTick = 0
      , impLog = mempty
      }
  where
    rootCoin = Coin 1_000_000_000

-- | Returns the @TxWits@ needed for sumbmitting the transaction
mkTxWits ::
  ( HasCallStack
  , EraImpTest era
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

-- | Runs an IO action and throws an @ImpException@ with the given message on
-- failure
impIOMsg :: NFData a => String -> IO a -> ImpTestM era a
impIOMsg msg m = ImpTestM $ do
  logs <- gets impLog
  lift . catchAnyDeep m $ \(SomeException e) -> throwIO $ ImpException logs msg e

-- | Runs an IO action and throws an @ImpException@ with a generic message on
-- failure
impIO :: NFData a => IO a -> ImpTestM era a
impIO = impIOMsg ""

submitTx ::
  ( HasCallStack
  , EraImpTest era
  ) =>
  String ->
  Tx era ->
  ImpTestM era (TxId (EraCrypto era))
submitTx msg tx = logEntry msg >> trySubmitTx tx >>= impExpectSuccess

impExpectSuccess :: (HasCallStack, NFData a, ToExpr e, Show e) => Either e a -> ImpTestM era a
impExpectSuccess (Right x) = pure x
impExpectSuccess (Left err) =
  impIO . error $
    "Expected success, got:\n" <> showExpr err <> "\n" <> show err

impExpectFailure :: HasCallStack => Either a b -> ImpTestM era ()
impExpectFailure (Right _) = impIO . error $ "Expected a failure, but got a success"
impExpectFailure (Left _) = pure ()

predicateFailureShouldBe ::
  ( Eq (PredicateFailure (EraRule rule era))
  , Show (PredicateFailure (EraRule rule era))
  ) =>
  PredicateFailure (EraRule rule era) ->
  Either [PredicateFailure (EraRule rule era)] right ->
  ImpTestM era ()
predicateFailureShouldBe pf = \case
  Right _ -> impIO $ error "Expected a predicate failure, but got success"
  Left [pf'] -> impIO $ pf `shouldBe` pf'
  Left _ -> impIO $ error "Expected a single predicate failure, but got more"

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: (HasCallStack, EraGov era) => String -> ImpTestM era ()
constitutionShouldBe cUrl = do
  constitution <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . to getConstitution
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    impIOMsg "Expecting a constitution" $ do
      pure $
        fromMaybe
          (error "No constitution has been set")
          constitution
  impIO $ anchorUrl `shouldBe` fromJust (textToUrl $ T.pack cUrl)
