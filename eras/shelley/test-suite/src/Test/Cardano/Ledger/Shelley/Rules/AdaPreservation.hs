{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.Rules.AdaPreservation (
  adaPreservationProps,
  tests,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Block (
  Block (..),
  bbody,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Internal (compareAdaPots)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  completeRupd,
  curPParamsEpochStateL,
  deltaF,
  deltaR,
  deltaT,
  esLStateL,
  lsCertStateL,
  nesEsL,
  prevPParamsEpochStateL,
  rs,
 )
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules (votedFuturePParams)
import Cardano.Ledger.Shelley.Rules.Reports (
  showCred,
  showIR,
  showKeyHash,
  showListy,
  showMap,
  showWithdrawal,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.UMap (sumRewardsUView)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Protocol.TPraos.BHeader (BHeader (..))
import Data.Foldable as F (fold, foldl', toList)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.TreeDiff.QuickCheck (ediffEq)
import Lens.Micro hiding (ix)
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..), totalAda, totalAdaPots)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  forAllChainTrace,
  ledgerTraceFromBlock,
  ledgerTraceFromBlockWithRestrictedUTxO,
  longTraceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  runShelleyBase,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  Trace (..),
  sourceSignalTargets,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  counterexample,
  noShrinking,
  (.&&.),
  (.||.),
  (===),
 )
import Test.QuickCheck.Property (withMaxSuccess)
import Test.Tasty (TestTree)
import qualified Test.Tasty.QuickCheck as TQC

tests ::
  forall era.
  ( EraGen era
  , ShelleyEraImp era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  , GovState era ~ ShelleyGovState era
  ) =>
  Int ->
  TestTree
tests n =
  TQC.testProperty
    "total amount of Ada is preserved (Chain)"
    (noShrinking $ withMaxSuccess n (adaPreservationProps @era))

-- | Various preservation properties
adaPreservationProps ::
  forall era.
  ( EraGen era
  , ShelleyEraImp era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  , GovState era ~ ShelleyGovState era
  ) =>
  Property
adaPreservationProps =
  forAllChainTrace @era longTraceLen defaultConstants $ \tr -> do
    let ssts :: [SourceSignalTarget (CHAIN era)]
        -- In this test, the STS Signal has this definition
        -- Signal(CHAIN era) = Block (BHeader MockCrypto) era
        ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts
        justBoundarySsts = filter (not . sameEpoch) ssts

    conjoin . concat $
      [ -- preservation properties
        zipWith (checkPreservation @era) justBoundarySsts [0 ..]
      , map (potsSumIncreaseWithdrawalsPerTx @era) ssts
      , map (potsSumIncreaseByRewardsPerTx @era) ssts
      , map (preserveBalance @era) ssts
      , map (preserveBalanceRestricted @era) ssts
      , map (preserveOutputsTx @era) ssts
      , map (potsRewardsDecreaseByWithdrawalsPerTx @era) ssts
      , map (canRestrictUTxO @era) ssts
      , -- well formed deposits
        map nonNegativeDeposits ssts
      , -- non-epoch-boundary preservation properties
        map checkWithdrawalBound noEpochBoundarySsts
      , map (utxoDepositsIncreaseByFeesWithdrawals @era) noEpochBoundarySsts
      , map potsSumIncreaseWithdrawalsPerBlock noEpochBoundarySsts
      , map feesNonDecreasing noEpochBoundarySsts
      ]

infoRetire :: Map (KeyHash 'StakePool) Coin -> KeyHash 'StakePool -> String
infoRetire deposits keyhash = showKeyHash keyhash ++ extra
  where
    extra = case Map.lookup keyhash deposits of
      Nothing -> " (?)"
      Just coin -> " (" ++ show coin ++ ")"

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation ::
  forall era.
  ( EraSegWits era
  , GovState era ~ ShelleyGovState era
  , EraGov era
  , EraCertState era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Int ->
  Property
checkPreservation SourceSignalTarget {source, target, signal} count =
  counterexample
    ( mconcat
        ( [ "\ncount = " ++ show count ++ "\n"
          , compareAdaPots "before" (totalAdaPots source) "after" (totalAdaPots target)
          , "\n\nTotal lovelace before block\n"
          , show sourceTotal
          , "\n\nTotal lovelace after block\n"
          , show targetTotal
          , "\n\nEpoch before block\n"
          , show (nesEL . chainNes $ source)
          , "\n\nEpoch after block\n"
          , show (nesEL . chainNes $ target)
          , "\n\nCurrent protocol parameters\n"
          , show currPP
          , "\nReward Accounts before update\n"
          , showMap (take 10 . showCred) show (UM.unUnify oldRAs)
          , "\nReward Accounts after update\n"
          , showMap (take 10 . showCred) show (UM.unUnify newRAs)
          , "\nRetiring pools before update\n"
          , showMap (infoRetire oldPoolDeposit) show oldRetire
          , "\nRetiring pools after update\n"
          , showMap (infoRetire newPoolDeposit) show newRetire
          , "\nMIR\n"
          , showIR mir
          , "\n\nRegistered Reserves MIR total "
          , show (fold regMirRes)
          , "\n\nUnregistered Reserves MIR total "
          , show (fold unRegMirRes)
          , "\n\nRegistered Treasury MIR total "
          , show (fold regMirTre)
          , "\n\nUnregistered Treasury MIR total "
          , show (fold unRegMirTre)
          , "\n\nPools Retiring This epoch\n"
          , showMap
              (infoRetire oldPoolDeposit)
              show
              (Map.filter (\e -> e == (nesEL . chainNes $ target)) oldRetire)
          ]
            ++ obligationMsgs
            ++ rewardUpdateMsgs
            ++ ["\n\ntransactions"]
            ++ txs
        )
    )
    $ sourceTotal === targetTotal
  where
    sourceTotal = totalAda source
    targetTotal = totalAda target

    currPP = view curPParamsEpochStateL . nesEs . chainNes $ source
    prevPP = view ppProtocolVersionL . view prevPParamsEpochStateL . nesEs . chainNes $ source

    ru' = nesRu . chainNes $ source
    lsOld = esLState . nesEs . chainNes $ source
    lsNew = esLState . nesEs . chainNes $ target
    oldRAs = rewards $ lsOld ^. lsCertStateL . certDStateL
    newRAs = rewards $ lsNew ^. lsCertStateL . certDStateL
    oldCertState = lsCertState lsOld
    oldRetire = lsOld ^. lsCertStateL . certPStateL . psRetiringL
    newRetire = lsNew ^. lsCertStateL . certPStateL . psRetiringL
    oldPoolDeposit = lsOld ^. lsCertStateL . certPStateL . psDepositsL
    newPoolDeposit = lsNew ^. lsCertStateL . certPStateL . psDepositsL

    proposal = votedFuturePParams (sgsCurProposals . utxosGovState $ lsUTxOState lsOld) currPP 5
    obligationMsgs = case proposal of
      Nothing -> []
      Just proposal' ->
        [ "\n\nProposed protocol parameter update\n"
        , show proposal'
        ]

    mir = lsOld ^. lsCertStateL . certDStateL . dsIRewardsL
    isRegistered kh _ = UM.member kh oldRAs
    (regMirRes, unRegMirRes) = Map.partitionWithKey isRegistered (iRReserves mir)
    (regMirTre, unRegMirTre) = Map.partitionWithKey isRegistered (iRTreasury mir)

    rewardUpdateMsgs = case ru' of
      SNothing -> []
      SJust ru'' ->
        let (ru, _rewevent) = runShelleyBase (completeRupd ru'')
            regRewards = Map.filterWithKey (\kh _ -> UM.member kh oldRAs) (rs ru)
         in [ "\n\nSum of new rewards "
            , show (sumRewards prevPP (rs ru))
            , "\n\nNew rewards "
            , show (rs ru)
            , "\n\nSum of new registered rewards "
            , show (sumRewards prevPP regRewards)
            , "\n\nChange in Fees "
            , show (deltaF ru)
            , "\n\nChange in Treasury "
            , show (deltaT ru)
            , "\n\nChange in Reserves "
            , show (deltaR ru)
            , "\n\nNet effect of reward update "
            , show $
                deltaT ru
                  <> deltaF ru
                  <> deltaR ru
                  <> toDeltaCoin (sumRewards prevPP (rs ru))
            ]

    txs' = toList $ (fromTxSeq @era . bbody) signal
    txs = zipWith dispTx txs' [0 :: Int ..]

    dispTx tx ix =
      "\n\n******** Transaction "
        ++ show ix
        ++ " "
        ++ show (hashAnnotated (tx ^. bodyTxL))
        ++ "\nfee :"
        ++ show (tx ^. bodyTxL . feeTxBodyL)
        ++ "\nwithdrawals:"
        ++ showWithdrawal (tx ^. bodyTxL . withdrawalsTxBodyL)
        ++ "\ncerts:"
        ++ showListy (("   " ++) . show) (toList $ tx ^. bodyTxL . certsTxBodyL)
        ++ "total deposits "
        ++ show (certsTotalDepositsTxBody currPP oldCertState (tx ^. bodyTxL))
        ++ "\ntotal refunds "
        ++ show (certsTotalRefundsTxBody currPP oldCertState (tx ^. bodyTxL))

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawalBound :: EraGen era => SourceSignalTarget (CHAIN era) -> Property
checkWithdrawalBound SourceSignalTarget {source, signal, target} =
  counterexample "checkWithdrawalBound" $
    rewardDelta === withdrawals signal
  where
    rewardDelta :: Coin
    rewardDelta =
      fromCompact
        (sumRewardsUView (rewards $ (chainNes source) ^. nesEsL . esLStateL . lsCertStateL . certDStateL))
        <-> fromCompact
          (sumRewardsUView (rewards $ (chainNes target) ^. nesEsL . esLStateL . lsCertStateL . certDStateL))

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits)
-- increases by Withdrawals minus Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  counterexample "utxoDepositsIncreaseByFeesWithdrawals" $
    circulation target
      <-> circulation source
      === withdrawals signal
        <-> txFees ledgerTr
  where
    us = lsUTxOState . esLState . nesEs . chainNes
    circulation chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d} = us chainSt
       in sumCoinUTxO u <+> d
    (_, ledgerTr) = ledgerTraceFromBlock @era source signal

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWithdrawalsPerBlock ::
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWithdrawalsPerBlock SourceSignalTarget {source, signal, target} =
  counterexample
    "potsSumIncreaseWithdrawalsPerBlock"
    $ potsSum target <-> potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in sumCoinUTxO u <+> d <+> f

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals in a transaction
potsSumIncreaseWithdrawalsPerTx ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWithdrawalsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseWithdrawalsPerTx" $
    conjoin $
      map sumIncreaseWithdrawals $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    sumIncreaseWithdrawals :: SourceSignalTarget (EraRule "LEDGER" era) -> Property
    sumIncreaseWithdrawals
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} _
        , signal = tx
        , target = LedgerState UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'} _
        } =
        property (hasFailedScripts tx)
          .||. (sumCoinUTxO u' <+> d' <+> f')
            <-> (sumCoinUTxO u <+> d <+> f)
            === fold (unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL))

-- | (Utxo + Deposits + Fees) increases by the reward delta
potsSumIncreaseByRewardsPerTx ::
  forall era.
  ( ChainProperty era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseByRewardsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseByRewardsPerTx" $
    conjoin $
      map sumIncreaseRewards $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    sumIncreaseRewards
      SourceSignalTarget
        { source =
          LedgerState
            UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f}
            cState1
        , target =
          LedgerState
            UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'}
            cState2
        } =
        (sumCoinUTxO u' <+> d' <+> f')
          <-> (sumCoinUTxO u <+> d <+> f)
          === UM.fromCompact (sumRewardsUView (UM.RewDepUView (cState1 ^. certDStateL . dsUnifiedL)))
            <-> UM.fromCompact (sumRewardsUView (UM.RewDepUView (cState2 ^. certDStateL . dsUnifiedL)))

-- | The Rewards pot decreases by the sum of withdrawals in a transaction
potsRewardsDecreaseByWithdrawalsPerTx ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsRewardsDecreaseByWithdrawalsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsRewardsDecreaseByWithdrawalsPerTx" $
    conjoin $
      map rewardsDecreaseByWithdrawals $
        sourceSignalTargets ledgerTr
  where
    rewardsSum certState = UM.fromCompact . sumRewardsUView . rewards $ certState ^. certDStateL
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    rewardsDecreaseByWithdrawals
      SourceSignalTarget
        { source = LedgerState _ dpstate
        , signal = tx
        , target = LedgerState _ dpstate'
        } =
        let totalRewards = rewardsSum dpstate
            totalRewards' = rewardsSum dpstate'
            txWithdrawals = fold (unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL))
         in conjoin
              [ counterexample
                  "A transaction should not increase the Rewards pot"
                  (totalRewards >= totalRewards')
              , counterexample
                  "Withdrawals should be non-negative"
                  (txWithdrawals >= Coin 0)
              , counterexample
                  "Rewards should increase by withdrawals"
                  (hasFailedScripts tx || totalRewards <-> totalRewards' == txWithdrawals)
              ]

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalance SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalance" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    pp_ = (view curPParamsEpochStateL . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = ledgerSt, signal = tx, target = ledgerSt'} =
      counterexample
        "preserveBalance created /= consumed ... "
        (failedScripts .||. ediffEq created consumed_)
      where
        failedScripts = property $ hasFailedScripts tx
        LedgerState (UTxOState {utxosUtxo = u}) certState = ledgerSt
        LedgerState (UTxOState {utxosUtxo = u'}) _ = ledgerSt'
        txb = tx ^. bodyTxL
        created =
          sumCoinUTxO u'
            <+> (txb ^. feeTxBodyL)
            <+> certsTotalDepositsTxBody pp_ certState txb
        consumed_ =
          sumCoinUTxO u
            <+> certsTotalRefundsTxBody pp_ certState txb
            <+> fold (unWithdrawals (txb ^. withdrawalsTxBodyL))

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  forall era.
  ( ChainProperty era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalanceRestricted SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalanceRestricted" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    pp_ = (view curPParamsEpochStateL . nesEs . chainNes) tickedChainSt

    createdIsConsumed
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = utxo}) certState
        , signal = tx
        } =
        inps === outs
        where
          txb = tx ^. bodyTxL
          inps =
            sumCoinUTxO @era (txInsFilter utxo (txb ^. inputsTxBodyL))
              <> certsTotalRefundsTxBody pp_ certState txb
              <> fold (unWithdrawals (txb ^. withdrawalsTxBodyL))
          outs =
            sumCoinUTxO (txouts @era txb)
              <> (txb ^. feeTxBodyL)
              <> certsTotalDepositsTxBody pp_ certState txb

preserveOutputsTx ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveOutputsTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveOutputsTx" $
    conjoin $
      map outputPreserved $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era chainSt block
    outputPreserved
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = UTxO utxo}) _
        , signal = tx
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
         in property $
              hasFailedScripts tx
                .||. counterexample "TxOuts are not a subset of UTxO" (outs `Map.isSubmapOf` utxo)

canRestrictUTxO ::
  forall era.
  ( ChainProperty era
  , ShelleyEraImp era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
canRestrictUTxO SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "canRestrictUTxO" $
    conjoin $
      zipWith
        outputPreserved
        (sourceSignalTargets ledgerTrFull)
        (sourceSignalTargets ledgerTrRestr)
  where
    (_, ledgerTrFull) = ledgerTraceFromBlock @era chainSt block
    (UTxO irrelevantUTxO, ledgerTrRestr) =
      ledgerTraceFromBlockWithRestrictedUTxO @era chainSt block
    outputPreserved
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uFull}) _}
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uRestr}) _} =
        counterexample
          (unlines ["non-disjoint:", show uRestr, show irrelevantUTxO])
          (uRestr `Map.disjoint` irrelevantUTxO)
          .&&. uFull
            === (uRestr `Map.union` irrelevantUTxO)

withdrawals ::
  forall era.
  EraGen era =>
  Block (BHeader MockCrypto) era ->
  Coin
withdrawals (Block _ txseq) =
  F.foldl'
    ( \c tx ->
        let wdrls = unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
         in if hasFailedScripts tx then c else c <> fold wdrls
    )
    (Coin 0)
    $ fromTxSeq @era txseq

txFees ::
  forall era.
  (EraGen era, ShelleyEraImp era) =>
  Trace (EraRule "LEDGER" era) ->
  Coin
txFees ledgerTr =
  F.foldl' f (Coin 0) (sourceSignalTargets ledgerTr)
  where
    f
      c
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = utxo} _
        , signal = tx
        } = c <> feeOrCollateral tx utxo

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {utxosDeposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Checks that the fees are non-decreasing when not at an epoch boundary
feesNonDecreasing ::
  SourceSignalTarget (CHAIN era) ->
  Property
feesNonDecreasing SourceSignalTarget {source, target} =
  counterexample ("feesNonDecreasing: " <> show (fees_ source) <> " <= " <> show (fees_ target)) $
    fees_ source <= fees_ target
  where
    fees_ chainSt =
      let UTxOState {utxosFees = fees} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in fees

sameEpoch ::
  forall era.
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes
