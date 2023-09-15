{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.ImpTest (
  ImpTestM,
  ImpException (..),
  passEpoch,
  passTick,
  itM,
  freshKeyHash,
  freshSafeHash,
  lookupKeyPair,
  submitTx,
  submitFailingTx,
  submitBasicConwayTx,
  logEntry,
  modifyNES,
  getsNES,
  impIO,
  impIOMsg,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF.Class as VRF
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..), Datum (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochSize (..),
  Globals (..),
  Network (..),
  ShelleyBase,
  SlotNo,
  mkActiveSlotCoeff,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (
  Era (..),
  EraGov (..),
  EraIndependentTxBody,
  EraPParams (..),
  EraRule,
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  emptyPParams,
  ppMaxValSizeL,
  setMinFeeTx,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppDRepActivityL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Conway.Rules (conwayWitsVKeyNeeded)
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash,
  KeyRole (..),
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  esLStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  prevPParamsEpochStateL,
  smartUTxOState,
  startStep,
  utxosDepositedL,
  utxosGovStateL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Cardano.Ledger.Shelley.UTxO (sumAllCoin)
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.DeepSeq (NFData)
import Control.Exception (Exception (..), SomeException (..), throwIO)
import Control.Monad (void)
import Control.Monad.State.Strict (MonadState (..), MonadTrans (..), StateT (..), execStateT, gets, modify)
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
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, (%~), (&), (.~), (^.))
import Lens.Micro.Mtl (view, (%=), (+=), (.=))
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Binary.TreeDiff (showExpr)
import Test.Cardano.Ledger.Common (HasCallStack, Spec, it)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkKeyHash, mkKeyPair, mkWitnessVKey)
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import UnliftIO (catchAnyDeep)

-- | Stores ledger state and some other bookkeeping information to make it more
-- convenient to write tests in the ImpTestM monad
data ImpTestState era = ImpTestState
  { impNES :: !(NewEpochState era)
  , impSafeHashIdx :: !Int
  , impKeyPairs ::
      Map
        (KeyHash 'Witness (EraCrypto era))
        (KeyPair 'Witness (EraCrypto era))
  , impRootTxId :: TxId (EraCrypto era)
  , impRootTxCoin :: Coin
  , impLog :: [String]
  , impLastTick :: SlotNo
  }
  deriving (Generic)

impRootTxCoinL :: Lens' (ImpTestState era) Coin
impRootTxCoinL = lens impRootTxCoin (\x y -> x {impRootTxCoin = y})

impLastTickL :: Lens' (ImpTestState era) SlotNo
impLastTickL = lens impLastTick (\x y -> x {impLastTick = y})

instance
  ( ConwayEraPParams era
  , Default (StashedAVVMAddresses era)
  , EraTxOut era
  , EraGov era
  ) =>
  Default (ImpTestState era)
  where
  def =
    ImpTestState
      { impNES =
          nes
            & nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL .~ zero
      , impSafeHashIdx = 0
      , impKeyPairs = Map.singleton (coerce $ hashKey vk) (coerce kp)
      , impRootTxId = rootTxId
      , impRootTxCoin = rootCoin
      , impLog = mempty
      , impLastTick = 0
      }
    where
      rootTxId = TxId (mkDummySafeHash 1)
      rootCoin = Coin 1000000000000
      pp =
        emptyPParams
          & ppMaxValSizeL .~ 1000000000
          & ppDRepActivityL .~ 100
          & ppGovActionLifetimeL .~ 30
      addr :: Addr (EraCrypto era)
      addr =
        Addr
          Testnet
          (KeyHashObj $ hashKey vk)
          (StakeRefBase (KeyHashObj testKeyHash))
      testKeyHash = mkKeyHash (-1)
      epochState :: EpochState era
      epochState =
        EpochState
          { esAccountState =
              AccountState
                { asTreasury = Coin 10000
                , asReserves = Coin 1000
                }
          , esSnapshots = emptySnapShots
          , esLState =
              LedgerState
                { lsUTxOState =
                    smartUTxOState
                      pp
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
      nes =
        NewEpochState
          { stashedAVVMAddresses = def
          , nesRu =
              SJust $
                startStep @era
                  (EpochSize 432000)
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
                        (hashVerKeyVRF . VRF.deriveVerKeyVRF . VRF.genKeyVRF . mkSeedFromBytes $ BS.replicate seedSize 1)
                    )
                  ]
          , nesEs = epochState
          , nesEL = 0
          , nesBprev = BlocksMade (Map.singleton testKeyHash 10)
          , nesBcur = BlocksMade mempty
          }
      kp@(KeyPair vk _) = mkKeyPair 0
      seedSize = fromIntegral . seedSizeDSIGN $ Proxy @(DSIGN (EraCrypto era))
      utxo =
        UTxO $
          Map.fromList
            [
              ( TxIn rootTxId minBound
              , mkBasicTxOut @era addr $ inject rootCoin
              )
            ]

mkDummySafeHash :: forall c a. Crypto c => Int -> SafeHash c a
mkDummySafeHash = unsafeMakeSafeHash . mkDummyHash @(HASH c)

deriving instance
  ( EraTxOut era
  , Show (StashedAVVMAddresses era)
  , Show (GovState era)
  ) =>
  Show (ImpTestState era)

instance
  ( EraPParams era
  , ToExpr (GovState era)
  , ToExpr (TxOut era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (KeyPair 'Witness (EraCrypto era))
  ) =>
  ToExpr (ImpTestState era)

impNESL :: Lens' (ImpTestState era) (NewEpochState era)
impNESL = lens impNES (\x y -> x {impNES = y})

newtype ImpTestM era a = ImpTestM (M.StateT (ImpTestState era) IO a)
  deriving (Functor, Applicative, Monad, MonadState (ImpTestState era))

slotsPerEpoch :: Word64
slotsPerEpoch = 100

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedEpochInfo (EpochSize slotsPerEpoch) (mkSlotLength 1)
    , slotsPerKESPeriod = 20
    , stabilityWindow = 33
    , randomnessStabilisationWindow = 33
    , securityParameter = 10
    , maxKESEvo = 10
    , quorum = 5
    , maxMajorPV = maxBound
    , maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , networkId = Testnet
    , systemStart = SystemStart $ posixSecondsToUTCTime 0
    }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

submitTx_ ::
  forall era.
  ( EraGov era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , STS (EraRule "LEDGER" era)
  ) =>
  Tx era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (State (EraRule "LEDGER" era))
    )
submitTx_ tx = do
  ImpTestState {..} <- get
  let
    ls = impNES ^. nesEsL . esLStateL
    pp = ls ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
    trc =
      TRC
        ( LedgerEnv 0 minBound pp def
        , ls
        , tx
        )
  pure $ runShelleyBase (applySTSTest @(EraRule "LEDGER" era) trc)

-- | Submit a transaction that is expected to be accepted. The inputs and
-- outputs are automatically balanced.
submitTx ::
  ( HasCallStack
  , EraGov era
  , EraTx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , ToExpr (PredicateFailure (EraRule "LEDGER" era))
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , STS (EraRule "LEDGER" era)
  ) =>
  String ->
  Tx era ->
  ImpTestM era (TxId (EraCrypto era))
submitTx msg tx = do
  res <- submitTx_ tx
  case res of
    Right x -> do
      modify $ impNESL . nesEsL . esLStateL .~ x
      pure . TxId $ hashAnnotated (tx ^. bodyTxL)
    Left es ->
      impIO . error $
        "Failed to submit transaction "
          <> show msg
          <> ":\n"
          <> unlines (showExpr <$> es)

-- | Submit a transaction that is expected to be rejected. The inputs and
-- outputs are automatically balanced.
submitFailingTx ::
  ( HasCallStack
  , EraGov era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , STS (EraRule "LEDGER" era)
  ) =>
  Tx era ->
  ImpTestM era ()
submitFailingTx tx = do
  res <- submitTx_ tx
  case res of
    Right _ -> error "Expected transaction to fail, but it succeeded"
    Left [] -> error "Failed with no errors"
    Left _ -> pure ()

-- | Runs the TICK rule once
passTick ::
  forall era.
  ( HasCallStack
  , BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , EraTxOut era
  , NFData (GovState era)
  , NFData (StashedAVVMAddresses era)
  , NFData (PredicateFailure (EraRule "TICK" era))
  ) =>
  ImpTestM era ()
passTick = do
  ImpTestState {impLastTick} <- get
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
      modifyNES $ const x
    Left e ->
      impIO . error $
        "Failed to run TICK:\n" <> unlines (show <$> e)

-- | Runs the TICK rule until the next epoch is reached
passEpoch ::
  forall era.
  ( BaseM (EraRule "TICK" era) ~ ReaderT Globals Identity
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , STS (EraRule "TICK" era)
  , Signal (EraRule "TICK" era) ~ SlotNo
  , EraTxOut era
  , NFData (GovState era)
  , NFData (StashedAVVMAddresses era)
  , NFData (PredicateFailure (EraRule "TICK" era))
  ) =>
  ImpTestM era ()
passEpoch = do
  startEpoch <- getsNES nesELL
  let
    tickUntilNewEpoch curEpoch = do
      passTick
      newEpoch <- getsNES nesELL
      if newEpoch > curEpoch
        then logEntry $ "Entered " <> show newEpoch
        else tickUntilNewEpoch newEpoch
  tickUntilNewEpoch startEpoch

-- | Stores extra information about the failure of the unit test
data ImpException e = ImpException
  { ieLog :: [String]
  -- ^ Log entries up to the point where the test failed
  , ieDescription :: String
  -- ^ Description of the IO action that caused the failure
  , ieThrownException :: e
  -- ^ Exception that caused the test to fail
  }

instance Exception e => Show (ImpException e) where
  show (ImpException msgLog msg e) =
    "Log:\n"
      <> unlines (('\t' :) <$> reverse msgLog)
      <> "\nFailed at:\n\t"
      <> msg
      <> "\nException:\n\t"
      <> displayException e

instance Exception e => Exception (ImpException e)

-- | Adds a string to the log, which is only shown if the test fails
logEntry :: String -> ImpTestM era ()
logEntry msg = do
  ImpTestState {impLog} <- get
  modify $ \st ->
    st
      { impLog = msg : impLog
      }

-- | Make the `ImpTestM` into a Spec item with the given description
itM ::
  forall era a.
  ( Default (StashedAVVMAddresses era)
  , EraTxOut era
  , EraGov era
  , ConwayEraPParams era
  ) =>
  String ->
  ImpTestM era a ->
  Spec
itM desc (ImpTestM m) = it desc . void $ execStateT m def

-- | Returns the @TxWits@ needed for sumbmitting the transaction
mkTxWits ::
  ( HasCallStack
  , Signable
      (DSIGN (EraCrypto era))
      (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraTx era
  , ConwayEraTxBody era
  ) =>
  TxBody era ->
  ImpTestM era (TxWits era)
mkTxWits txb = do
  ImpTestState {impNES, impKeyPairs} <- get
  let
    utxo = impNES ^. nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
    txbHash = hashAnnotated txb
    witsNeeded = conwayWitsVKeyNeeded utxo txb
    mkTxWit kh =
      mkWitnessVKey txbHash
        . fromMaybe (error "Could not find a keypair corresponding to keyhash. Always use `freshKeyHash` to create key hashes.")
        $ Map.lookup kh impKeyPairs
  pure $
    mkBasicTxWits
      & addrTxWitsL .~ Set.map mkTxWit witsNeeded

-- | Modifies the transaction to make it valid. Handles things such as balancing
-- produced and consumed values and ensuring fees are correct.
fixupTx ::
  forall era.
  ( EraTx era
  , EraGov era
  , ConwayEraTxBody era
  , Signable
      (DSIGN (EraCrypto era))
      (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , TxOut era ~ BabbageTxOut era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupTx tx = do
  ImpTestState {impNES, impRootTxId, impRootTxCoin} <- get
  let
    utxos = impNES ^. nesEsL . esLStateL . lsUTxOStateL
    pp = utxos ^. utxosGovStateL . curPParamsGovStateL
  kpSpending <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  modify $ \st ->
    st
      { impRootTxId = TxId $ hashAnnotated (tx ^. bodyTxL)
      }
  let
    outputsTotalCoin = sumAllCoin $ tx ^. bodyTxL . outputsTxBodyL
    remainingCoin = impRootTxCoin <-> outputsTotalCoin
    remainingTxOut =
      BabbageTxOut @era
        (mkAddr (kpSpending, kpStaking))
        (inject remainingCoin)
        NoDatum
        SNothing
  impRootTxCoinL .= remainingCoin
  let
    balancedTx =
      setMinFeeTx pp tx
        & bodyTxL . inputsTxBodyL %~ Set.insert (mkTxInPartial impRootTxId 0)
        & bodyTxL . outputsTxBodyL %~ (remainingTxOut :<|)
  wits <- mkTxWits (balancedTx ^. bodyTxL)
  pure $ balancedTx & witsTxL .~ wits

-- | Constructs and submits a Conway era transaction. Takes a message that's
-- shown when the transaction fails and a function that constructs a transaction
-- from the basic transaction. The function does not have to balance the
-- produced and consumed values, this is done automatically.
submitBasicConwayTx :: String -> (Tx Conway -> Tx Conway) -> ImpTestM Conway (TxId StandardCrypto)
submitBasicConwayTx desc f = do
  let
    initialTx = mkBasicTx mkBasicTxBody
  fixedTx <- fixupTx $ f initialTx
  txId <- submitTx desc fixedTx
  modify $ \st ->
    st
      { impRootTxId = txId
      }
  pure txId

-- | Creates a fresh @SafeHash@
freshSafeHash :: Era era => ImpTestM era (SafeHash (EraCrypto era) a)
freshSafeHash = do
  ImpTestState {impSafeHashIdx} <- get
  modify $ \st -> st {impSafeHashIdx = succ impSafeHashIdx}
  pure $ mkDummySafeHash impSafeHashIdx

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
