{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.Rules.TestChain (
  delegTraceFromBlock,
  forAllChainTrace,
  ledgerTraceFromBlock,
  ledgerTraceFromBlockWithRestrictedUTxO,
  chainSstWithTick,
  poolTraceFromBlock,
  TestingLedger,
  splitTrace,
  forEachEpochTrace,
  traceLen,
  longTraceLen,
  shortChainTrace,
) where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Block (
  Block (..),
  bheader,
  neededTxInsForBlock,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Ledger.Shelley.API (ApplyBlock, CertState (..), ShelleyDELEG)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  curPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  DelegEnv (..),
  LedgerEnv (..),
  PoolEnv (..),
  ShelleyPOOL,
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader (
  BHeader (..),
  bhbody,
  bheaderSlotNo,
 )
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import Data.Foldable (toList)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.Constants (Constants)
import Test.Cardano.Ledger.Shelley.Generator.Block (tickChainState)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Preset (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  runShelleyBase,
  testGlobals,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  Trace (..),
  sourceSignalTargets,
  splitTrace,
 )
import qualified Test.Control.State.Transition.Trace as Trace
import Test.Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  withMaxSuccess,
 )

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

longTraceLen :: Word64
longTraceLen = 150

type TestingLedger era ledger =
  ( BaseM ledger ~ ReaderT Globals Identity
  , Environment ledger ~ LedgerEnv era
  , State ledger ~ LedgerState era
  , Signal ledger ~ Tx era
  , Embed (EraRule "DELEGS" era) ledger
  , Embed (EraRule "UTXOW" era) ledger
  , STS ledger
  )

-- ===================================================

-- | Properties on really short chains, with only 100 successes
shortChainTrace ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGov era
  ) =>
  Constants ->
  (SourceSignalTarget (CHAIN era) -> Property) ->
  Property
shortChainTrace constants f = withMaxSuccess 100 $ forAllChainTrace @era 10 constants $ \tr -> conjoin (map f (sourceSignalTargets tr))

----------------------------------------------------------------------
-- Projections of CHAIN Trace
----------------------------------------------------------------------

-- | Reconstruct a LEDGER trace from the transactions in a Block and ChainState
ledgerTraceFromBlock ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace ledger)
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt
  , runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0 txs
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block

-- | This function is nearly the same as ledgerTraceFromBlock, but
-- it restricts the UTxO state to only those needed by the block.
-- It also returns the unused UTxO for comparison later.
ledgerTraceFromBlockWithRestrictedUTxO ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (UTxO era, Trace ledger)
ledgerTraceFromBlockWithRestrictedUTxO chainSt block =
  ( UTxO irrelevantUTxO
  , runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0' txs
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    txIns = neededTxInsForBlock block
    LedgerState utxoSt delegationSt = ledgerSt0
    utxo = unUTxO . utxosUtxo $ utxoSt
    (relevantUTxO, irrelevantUTxO) = Map.partitionWithKey (const . (`Set.member` txIns)) utxo
    ledgerSt0' = LedgerState (utxoSt {utxosUtxo = UTxO relevantUTxO}) delegationSt

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
poolTraceFromBlock ::
  forall era.
  ( ChainProperty era
  , ShelleyEraTxBody era
  , EraSegWits era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace (ShelleyPOOL era))
poolTraceFromBlock chainSt block =
  ( tickedChainSt
  , runShelleyBase $
      Trace.closure @(ShelleyPOOL era) poolEnv poolSt0 poolCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (toList . view certsTxBodyL . view bodyTxL)
    poolCerts = mapMaybe getPoolCertTxCert (certs txs)
    poolEnv =
      let (LedgerEnv s _ pp _) = ledgerEnv
       in PoolEnv s pp
    poolSt0 =
      certPState (lsCertState ledgerSt0)

-- | Reconstruct a DELEG trace from all the transaction certificates in a Block
delegTraceFromBlock ::
  forall era.
  ( ChainProperty era
  , ShelleyEraTxBody era
  , EraSegWits era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (DelegEnv era, Trace (ShelleyDELEG era))
delegTraceFromBlock chainSt block =
  ( delegEnv
  , runShelleyBase $
      Trace.closure @(ShelleyDELEG era) delegEnv delegSt0 blockCerts
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (reverse . toList . view certsTxBodyL . view bodyTxL)
    blockCerts = filter delegCert (certs txs)
    delegEnv =
      let (LedgerEnv s txIx pp reserves) = ledgerEnv
          dummyCertIx = minBound
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves pp
    delegSt0 =
      certDState (lsCertState ledgerSt0)
    delegCert (RegTxCert _) = True
    delegCert (UnRegTxCert _) = True
    delegCert (DelegStakeTxCert _ _) = True
    delegCert (MirTxCert _) = True
    delegCert _ = False

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
--
-- NOTE: we need to tick the slot before processing transactions
-- (in the same way that the CHAIN rule TICKs the slot before processing
-- transactions with the LEDGERS rule)
ledgerTraceBase ::
  forall era.
  ( EraSegWits era
  , GetLedgerView era
  , ApplyBlock era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, LedgerEnv era, LedgerState era, [Tx era])
ledgerTraceBase chainSt block =
  ( tickedChainSt
  , LedgerEnv slot minBound pp_ (esAccountState nes)
  , esLState nes
  , txs
  )
  where
    (UnserialisedBlock (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = nes ^. curPParamsEpochStateL
    -- Oldest to Newest first
    txs = (reverse . toList . fromTxSeq) txSeq -- HERE WE USE SOME SegWit function

-- | Transform the [(source, signal, target)] of a CHAIN Trace
-- by manually applying the Chain TICK Rule to each source and producing
-- [(source, signal, target')].
--
-- This allows for testing properties on Chain traces while excluding the effects
-- of Transactions and Certificates. For example we can check that pools that are
-- due for retirement at an epoch transition, are indeed retired.
--
-- Had we not excluded the effects of Transactions/Certificates, we might have
-- a pool that was correctly retired, but is again registered by a certificate
-- in the block following the transition.
chainSstWithTick ::
  forall era.
  ChainProperty era =>
  Trace (CHAIN era) ->
  [SourceSignalTarget (CHAIN era)]
chainSstWithTick ledgerTr =
  map applyTick (sourceSignalTargets ledgerTr)
  where
    applyTick sst@SourceSignalTarget {source = chainSt, signal = block} =
      let bh = bheader block
          slot = (bheaderSlotNo . bhbody) bh
       in sst {target = tickChainState @era slot chainSt}

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  forall era prop.
  ( Testable prop
  , EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGov era
  ) =>
  Word64 -> -- trace length
  Constants ->
  (Trace (CHAIN era) -> prop) ->
  Property
forAllChainTrace n constants prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState
      testGlobals
      n
      (Preset.genEnv p constants)
      (Just $ mkGenesisChainState (Preset.genEnv p constants))
      prop
  where
    p :: Proxy era
    p = Proxy

-- | Test a property on the first 'subtracecount' sub-Traces that end on an EpochBoundary
forEachEpochTrace ::
  forall era prop.
  ( EraGen era
  , Testable prop
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGov era
  ) =>
  Int ->
  Word64 ->
  Constants ->
  (Trace (CHAIN era) -> prop) ->
  Property
forEachEpochTrace subtracecount tracelen constants f = forAllChainTrace tracelen constants action
  where
    -- split at contiguous elements with different Epoch numbers
    p new old = (nesEL . chainNes) new /= (nesEL . chainNes) old
    -- At a minimum throw away the last trace which is probably an incomplete epoch
    action tr = conjoin $ map f (take (min subtracecount (m - 1)) (reverse traces))
      where
        traces = splitTrace p tr
        m = length traces

-- ================================
-- an example how one might debug one test, which can be replayed
-- import Test.Tasty (defaultMain)
-- main :: IO ()
-- main = defaultMain (minimal @(ShelleyEra TestCrypto))
-- Then in ghci, one can just type
-- :main --quickcheck-replay=443873
-- =================================
