{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (
  incrStakeComputationTest,
  incrStakeComparisonTest,
  stakeDistr,
  aggregateUtxoCoinByCredential,
) where

import Cardano.Ledger.BaseTypes (Globals (Globals, networkId), Network (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeReference (StakeRefBase, StakeRefPtr))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  curPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.Rules (Identity, LedgerEnv)
import Cardano.Ledger.Shelley.State
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.State.Transition (STS (..))
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.VMap as VMap
import Lens.Micro hiding (ix)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rewards (mkSnapShot)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  forAllChainTrace,
  ledgerTraceFromBlock,
  longTraceLen,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  runShelleyBase,
 )
import Test.Cardano.Ledger.TerseTools (tersemapdiffs)
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  sourceSignalTargets,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

incrStakeComputationTest ::
  forall era.
  ( EraGen era
  , EraStake era
  , ShelleyEraAccounts era
  , InstantStake era ~ ShelleyInstantStake era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , STS (EraRule "LEDGER" era)
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  ) =>
  TestTree
incrStakeComputationTest =
  testProperty "instant stake calculation" $
    forAllChainTrace @era longTraceLen defaultConstants $ \tr -> do
      conjoin $ incrStakeComp @era <$> sourceSignalTargets tr

incrStakeComp ::
  forall era.
  ( ChainProperty era
  , InstantStake era ~ ShelleyInstantStake era
  , BaseM (EraRule "LEDGER" era) ~ ReaderT Globals Identity
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , STS (EraRule "LEDGER" era)
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , ShelleyEraAccounts era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
incrStakeComp SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map checkIncrStakeComp $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    checkIncrStakeComp :: SourceSignalTarget (EraRule "LEDGER" era) -> Property
    checkIncrStakeComp
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosInstantStake = is} dp
        , signal = tx
        , target = LedgerState UTxOState {utxosUtxo = u', utxosInstantStake = is'} dp'
        } =
        counterexample
          ( unlines
              [ "\nDetails:"
              , "\ntx"
              , show tx
              , "size original utxo"
              , show (Map.size $ unUTxO u)
              , "original utxo"
              , show u
              , "original instantStake"
              , show is
              , "final utxo"
              , show u'
              , "final instantStake"
              , show is'
              , "original ptrs"
              , show ptrs
              , "final ptrs"
              , show ptrs'
              ]
          )
          $ utxoBalanace === fromCompact instantStakeBalanace
        where
          utxoBalanace = sumCoinUTxO u'
          instantStakeBalanace = fold (sisCredentialStake is') <> fold (sisPtrStake is')
          ptrs = dp ^. certDStateL . accountsL . accountsPtrsMapG
          ptrs' = dp' ^. certDStateL . accountsL . accountsPtrsMapG

incrStakeComparisonTest ::
  forall era.
  ( EraGen era
  , EraGov era
  , EraStake era
  , ShelleyEraAccounts era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  Proxy era ->
  TestTree
incrStakeComparisonTest Proxy = do
  let network = runShelleyBase $ ask >>= \Globals {networkId} -> pure networkId
  testProperty "Incremental stake distribution at epoch boundaries agrees" $
    forAllChainTrace traceLen defaultConstants $ \tr ->
      conjoin
        $ map
          (\(SourceSignalTarget _ target _) -> checkIncrementalStake @era network ((nesEs . chainNes) target))
        $ filter (not . sameEpoch) (sourceSignalTargets tr)
  where
    sameEpoch SourceSignalTarget {source, target} = epoch source == epoch target
    epoch = nesEL . chainNes

checkIncrementalStake ::
  forall era.
  (EraGov era, EraTxOut era, EraStake era, EraCertState era, ShelleyEraAccounts era) =>
  Network ->
  EpochState era ->
  Property
checkIncrementalStake network es =
  let
    LedgerState (UTxOState utxo _ _ _ instantStake _) certState = esLState es
    dstate = certState ^. certDStateL
    pstate = certState ^. certPStateL
    stake = stakeDistr @era utxo dstate pstate
    snapShot = snapShotFromInstantStake instantStake dstate pstate network
    _pp = es ^. curPParamsEpochStateL
   in
    counterexample
      ( "\nIncremental stake distribution does not match old style stake distribution"
          ++ tersediffincremental
            "differences: Old vs Incremental"
            (ssActiveStake stake)
            (ssActiveStake snapShot)
      )
      (stake === snapShot)

tersediffincremental :: String -> Stake -> Stake -> String
tersediffincremental message (Stake a) (Stake c) =
  tersemapdiffs (message ++ " " ++ "hashes") (mp a) (mp c)
  where
    mp = Map.map fromCompact . VMap.toMap

-- | Compute the current Stake Distribution. This was called at the Epoch boundary in the Snap Rule.
--   Now it is called in the tests to see that its incremental analog 'incrementalStakeDistr' agrees.
stakeDistr ::
  forall era.
  (EraTxOut era, ShelleyEraAccounts era) =>
  UTxO era ->
  DState era ->
  PState era ->
  SnapShot
stakeDistr u ds PState {psStakePools} =
  mkSnapShot activeStake (VMap.fromMap delegs) poolParams
  where
    activeStake = Stake $ VMap.fromMap (stakeRelation `Map.intersection` activeDelegs)
    accountsMap = ds ^. accountsL . accountsMapL
    rewards' :: Map.Map (Credential Staking) (CompactForm Coin)
    rewards' = Map.map (^. balanceAccountStateL) accountsMap
    delegs :: Map.Map (Credential Staking) (KeyHash StakePool)
    delegs = Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap
    ptrs' = ds ^. accountsL . accountsPtrsMapG
    stakeRelation :: Map (Credential Staking) (CompactForm Coin)
    stakeRelation = aggregateUtxoCoinByCredential ptrs' u rewards'
    activeDelegs :: Map.Map (Credential Staking) (KeyHash StakePool)
    activeDelegs = Map.filterWithKey (\k v -> Map.member k rewards' && Map.member v psStakePools) delegs
    poolParams = VMap.fromMap $ Map.mapWithKey (`stakePoolStateToStakePoolParams` Testnet) psStakePools

-- | Sum up all the Coin for each staking Credential. This function has an
--   incremental analog. See 'incrementalAggregateUtxoCoinByCredential'
aggregateUtxoCoinByCredential ::
  forall era.
  EraTxOut era =>
  Map Ptr (Credential Staking) ->
  UTxO era ->
  Map (Credential Staking) (CompactForm Coin) ->
  Map (Credential Staking) (CompactForm Coin)
aggregateUtxoCoinByCredential ptrs (UTxO u) initial =
  Map.foldl' accum (Map.filter (/= mempty) initial) u
  where
    accum ans out =
      let c = out ^. compactCoinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p)
              | Just cred <- Map.lookup p ptrs -> Map.insertWith (<>) cred c ans
            Addr _ _ (StakeRefBase hk) -> Map.insertWith (<>) hk c ans
            _other -> ans
