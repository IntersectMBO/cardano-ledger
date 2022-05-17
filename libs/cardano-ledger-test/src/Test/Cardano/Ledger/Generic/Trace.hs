{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Trace where

-- =========================================================================

import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import qualified Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Ledger ()
import Cardano.Ledger.Babbage.Rules.Utxow ()
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty (PDoc, ppInt, ppList, ppMap, ppSafeHash, ppStrictSeq, ppString, ppWord64)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API.Wallet (totalAdaPotsES)
import Cardano.Ledger.Shelley.Constraints (UsesValue)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    StashedAVVMAddresses,
    UTxOState (..),
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (get)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Control.State.Transition.Trace (Trace (..), lastState)
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..), traceFromInitState)
import Data.Default.Class (Default (def))
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as SS
import qualified Data.Set as Set
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import GHC.Word (Word64)
import Prettyprinter (vsep)
import Test.Cardano.Ledger.Generic.ApplyTx (applyTx)
import Test.Cardano.Ledger.Generic.Functions (allInputs, getBody, getTxOutCoin, isValid')
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenSize,
    GenState (..),
    getBlocksizeMax,
    getReserves,
    getSlot,
    getTreasury,
    initialLedgerState,
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.MockChain
import Test.Cardano.Ledger.Generic.ModelState (stashedAVVMAddressesZero)
import Test.Cardano.Ledger.Generic.PrettyCore (pcCoin, pcTx, pcTxBody, pcTxIn)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen (genValidatedTx)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- ===========================================

-- | Generate a Core.Tx and an internal Model of the state after the tx
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel :: Reflect era => Proof era -> Word64 -> GenRS era (Core.Tx era)
genRsTxAndModel proof n = do
  (_, tx) <- genValidatedTx proof
  tx <$ modifyModel (\model -> applyTx proof (fromIntegral n) model tx)

-- | Generate a Vector of (StrictSeq (Core.Tx era))  representing a (Vector Block)
genRsTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  Word64 ->
  Word64 ->
  [Core.Tx era] ->
  GenRS era (Vector (StrictSeq (Core.Tx era)))
genRsTxSeq _ this lastN ans | this >= lastN = chop (reverse ans) []
genRsTxSeq proof this lastN ans = do
  tx <- genRsTxAndModel proof this
  genRsTxSeq proof (this + 1) lastN (tx : ans)

-- | Chop a list into random size blocks
chop ::
  [Core.Tx era] ->
  [StrictSeq (Core.Tx era)] ->
  GenRS era (Vector (StrictSeq (Core.Tx era)))
chop [] ans = pure $ Vector.fromList (reverse ans)
chop xs ans
  | length (Prelude.take 4 xs) <= 3 =
      pure $ Vector.fromList (reverse (SS.fromList xs : ans))
chop pairs ans = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  let block = SS.fromList $ Prelude.take n pairs
  chop (Prelude.drop n pairs) (block : ans)

-- | Generate a Vector of Blocks, and an initial LedgerState
genTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  GenSize ->
  Word64 ->
  Gen (Vector (StrictSeq (Core.Tx era)), GenState era)
genTxSeq proof gensize numTx = do
  runGenRS proof gensize (genRsTxSeq proof 0 numTx [])

run :: IO ()
run = do
  (vs, _) <- generate $ genTxSeq (Babbage Mock) def 5
  print (ppList (ppStrictSeq (pcTx (Babbage Mock))) (Vector.toList vs))

-- ==================================================================
-- Constructing the "real", initial NewEpochState, from the GenState

genMockChainState ::
  Reflect era =>
  Proof era ->
  GenState era ->
  Gen (MockChainState era)
genMockChainState proof gstate = pure $ MockChainState newepochstate (getSlot gstate) 0
  where
    ledgerstate = initialLedgerState gstate
    newepochstate =
      NewEpochState
        { nesEL = EpochNo 0,
          nesBprev = BlocksMade Map.empty,
          nesBcur = BlocksMade Map.empty,
          nesEs = makeEpochState gstate ledgerstate,
          nesRu = SNothing,
          nesPd = PoolDistr (gsInitialPoolDistr gstate),
          stashedAVVMAddresses = stashedAVVMAddressesZero proof
        }

makeEpochState :: GenState era -> LedgerState era -> EpochState era
makeEpochState gstate ledgerstate =
  EpochState
    { esAccountState = AccountState (getTreasury gstate) (getReserves gstate),
      esSnapshots = def,
      esLState = ledgerstate,
      esPrevPp = gePParams (gsGenEnv gstate),
      esPp = gePParams (gsGenEnv gstate),
      esNonMyopic = def
    }

-- ==============================================================================

-- | Turn a UTxO into a smaller UTxO, with only entries mentioned in
--   the inputs of 'txs' ,  then pretty print it.
pcSmallUTxO :: Proof era -> UTxO era -> [Core.Tx era] -> PDoc
pcSmallUTxO proof u txs = ppMap pcTxIn (pcCoin . getTxOutCoin proof) m
  where
    keys = Set.unions (map f txs)
    f tx = allInputs proof (getBody proof tx)
    m = Map.restrictKeys (unUTxO u) keys

raiseMockError ::
  (UsesValue era, Reflect era) =>
  Word64 ->
  EpochState era ->
  [MockChainFailure era] ->
  [Core.Tx era] ->
  String
raiseMockError slot epochstate pdfs txs =
  show $
    vsep
      [ pcSmallUTxO reify ((_utxo . lsUTxOState . esLState) epochstate) txs,
        ppString "===================================",
        ppString "Slot " <> ppWord64 slot,
        ppString "===================================",
        showBlock txs,
        ppString "===================================",
        ppString (show (totalAdaPotsES epochstate)),
        ppString "===================================",
        ppList (ppMockChainFailure reify) pdfs
      ]

showBlock :: Reflect era => [Core.Tx era] -> PDoc
showBlock txs = ppList pppair (zip txs [0 ..])
  where
    pppair (tx, n) =
      let body = getBody reify tx
       in vsep
            [ ppString "\n###########",
              ppInt n,
              ppSafeHash (hashAnnotated body),
              pcTxBody reify body,
              ppString (show (isValid' reify tx)),
              ppString "\n"
            ]

-- =====================================================================
-- HasTrace instance of MOCKCHAIN depends on STS(MOCKCHAIN era) instance
-- We show the type family instances here for reference.
{-
instance STS (MOCKCHAIN era)
  where
  type State (MOCKCHAIN era) = MockChainState era
  type Signal (MOCKCHAIN era) = (MockBlock era,UTxO era)
  type Environment (MOCKCHAIN era) = ()
-}
-- ==============================================================

newtype Gen1 era = Gen1 (Vector (StrictSeq (Core.Tx era)))

instance
  ( STS (MOCKCHAIN era),
    UsesValue era,
    Reflect era
  ) =>
  HasTrace (MOCKCHAIN era) (Gen1 era)
  where
  type BaseEnv (MOCKCHAIN era) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals

  envGen _gstate = pure ()

  sigGen (Gen1 txss) () mcs@(MockChainState newepoch (SlotNo lastSlot) count) = do
    let NewEpochState _epochnum _ _ epochstate _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer pooldistr
    nextSlotNo <- SlotNo . (+ lastSlot) <$> choose (15, 25)
    let txs = txss ! count
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal

    case runShelleyBase (applySTSTest (TRC @(MOCKCHAIN era) ((), mcs, mockblock))) of
      Left pdfs -> error (raiseMockError lastSlot epochstate pdfs (Fold.toList (txss ! count)))
      -- Left pdfs -> trace ("Discard\n"++show(ppList (ppMockChainFailure reify) pdfs)) discard -- TODO should we enable this?
      Right mcs2 -> seq mcs2 (pure mockblock)

  shrinkSignal _x = []

mapProportion :: (v -> Int) -> Map.Map k v -> Gen k
mapProportion toInt m = frequency [(toInt v, pure k) | (k, v) <- Map.toList m]

chooseIssuer :: PoolDistr crypto -> Gen (KeyHash 'StakePool crypto)
chooseIssuer (PoolDistr m) = mapProportion getInt m
  where
    getInt x = floor (individualPoolStake x * 1000)

-- ===================================================================================

-- Generating Traces, and making properties out of a Trace
-- =========================================================================

genTrace ::
  forall era.
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Word64 ->
  GenSize ->
  Gen (Trace (MOCKCHAIN era))
genTrace proof numTxInTrace gsize = do
  (vs, genstate) <- genTxSeq proof gsize numTxInTrace
  initState <- genMockChainState proof genstate
  traceFromInitState @(MOCKCHAIN era)
    testGlobals
    (fromIntegral (length vs))
    (Gen1 vs)
    (Just (\_ -> pure $ Right initState))

traceProp ::
  forall era prop.
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Word64 ->
  GenSize ->
  (MockChainState era -> MockChainState era -> prop) ->
  Gen prop
traceProp proof numTxInTrace gsize f = do
  trace1 <- genTrace proof numTxInTrace gsize
  pure (f (_traceInitState trace1) (lastState trace1))

-- =========================================================================
-- Test for just making a trace

chainTest ::
  forall era.
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era),
    Eq (Core.TxOut era),
    Eq (Core.PParams era),
    Eq (State (Core.EraRule "PPUP" era)),
    Eq (StashedAVVMAddresses era)
  ) =>
  Proof era ->
  Word64 ->
  GenSize ->
  TestTree
chainTest proof n gsize = testProperty message action
  where
    message = "MockChainTrace: " ++ show proof ++ " era."
    action = do
      (vs, genstate) <- genTxSeq proof gsize n
      initState <- genMockChainState proof genstate
      trace1 <-
        traceFromInitState @(MOCKCHAIN era)
          testGlobals
          (fromIntegral (length vs))
          (Gen1 vs)
          (Just (\_ -> pure $ Right initState))
      -- Here is where we can add some properties for traces:
      pure (_traceInitState trace1 === initState)

testTraces :: Word64 -> TestTree
testTraces n =
  testGroup
    "MockChainTrace"
    [ chainTest (Babbage Mock) n def,
      chainTest (Alonzo Mock) n def,
      chainTest (Mary Mock) n def,
      chainTest (Allegra Mock) n def,
      chainTest (Shelley Mock) n def
    ]
