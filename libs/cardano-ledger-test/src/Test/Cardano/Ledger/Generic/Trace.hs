{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Ledger.Pretty (ppList, ppPair, ppWord64)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    StashedAVVMAddresses,
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (get, gets)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.State.Transition.Extended (STS (..))
import Control.State.Transition.Trace (Trace (..))
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..), traceFromInitState)
import Data.Default.Class (Default (def))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence as Seq (fromList)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import GHC.Word (Word64)
import Test.Cardano.Ledger.Generic.ApplyTx (applyTx)
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
import Test.Cardano.Ledger.Generic.ModelState
  ( Model,
    mNewEpochStateZero,
    stashedAVVMAddressesZero,
  )
import Test.Cardano.Ledger.Generic.PrettyCore (pcTx)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen
  ( genValidatedTx,
  )
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- ===========================================

-- | Generate a Core.Tx and an internal Model of the state after the block
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel :: Reflect era => Proof era -> Model era -> Word64 -> GenRS era (Core.Tx era, Model era)
genRsTxAndModel proof model0 n = do
  modifyModel (const model0)
  (_, tx) <- genValidatedTx proof
  model1 <- gets gsModel
  let model2 = applyTx proof (fromIntegral n) model1 tx
  pure (tx, model2)

-- | Generate a Vector of [(Word64,Core.Tx era)] representing a (Vector Block)
genRsTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  Model era ->
  Word64 ->
  Word64 ->
  [(Word64, Core.Tx era)] ->
  GenRS era (Vector [(Word64, Core.Tx era)])
genRsTxSeq _ _ this lastN ans | this > lastN = chop (reverse ans) []
genRsTxSeq proof model0 this lastN ans = do
  (tx, model1) <- genRsTxAndModel proof model0 this
  genRsTxSeq proof model1 (this + 1) lastN ((this, tx) : ans)

-- | Chop a list into random size blocks
chop :: [(Word64, Core.Tx era)] -> [[(Word64, Core.Tx era)]] -> GenRS era (Vector [(Word64, Core.Tx era)])
chop [] ans = pure $ Vector.fromList (reverse ans)
chop xs ans | length (take 4 xs) <= 3 = pure $ Vector.fromList (reverse (xs : ans))
chop pairs ans = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  let block = Prelude.take n pairs
  chop (Prelude.drop n pairs) (block : ans)

-- | Generate a Vector of Blocks, and an initial LedgerState
genTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  GenSize ->
  Word64 ->
  Gen (Vector [(Word64, Core.Tx era)], GenState era)
genTxSeq proof gensize numTx = do
  runGenRS proof gensize (genRsTxSeq proof mNewEpochStateZero 0 numTx [])

run :: IO ()
run = do
  (vs, _) <- generate $ genTxSeq (Babbage Mock) def 5
  print (ppList (ppList (ppPair ppWord64 (pcTx (Babbage Mock)))) (Vector.toList vs))

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

newtype Gen1 era = Gen1 (Vector [Core.Tx era])

instance
  ( STS (MOCKCHAIN era),
    Reflect era
  ) =>
  HasTrace (MOCKCHAIN era) (Gen1 era)
  where
  type BaseEnv (MOCKCHAIN era) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals

  envGen _gstate = pure ()

  sigGen (Gen1 txss) () mcs@(MockChainState newepoch (SlotNo lastSlot) count) = do
    let NewEpochState _epoch _ _ _ _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer pooldistr
    nextSlotNo <- SlotNo . (+ lastSlot) <$> choose (15, 25)
    let txs = Seq.fromList (txss ! count)
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal
    goSTS
      (MOCKCHAIN reify)
      ()
      mcs
      mockblock
      ( \case
          Left pdfs -> error ("MOCKCHAIN sigGen:\n" <> show (ppList (ppMockChainFailure reify) pdfs))
          Right _mcs -> pure mockblock
      )

  shrinkSignal _x = []

mapProportion :: (v -> Int) -> Map.Map k v -> Gen k
mapProportion toInt m = frequency [(toInt v, pure k) | (k, v) <- Map.toList m]

chooseIssuer :: PoolDistr crypto -> Gen (KeyHash 'StakePool crypto)
chooseIssuer (PoolDistr m) = mapProportion getInt m
  where
    getInt x = floor (individualPoolStake x * 1000)

-- ===================================================================================

-- =========================================================================
-- Making and running traces

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
    message = "MockChainTrace in the (" ++ show proof ++ ") era."
    action = do
      (vs, genstate) <- genTxSeq proof gsize n
      initState <- genMockChainState proof genstate
      trace <-
        traceFromInitState @(MOCKCHAIN era)
          testGlobals
          (fromIntegral (length vs))
          (Gen1 (Vector.map (map snd) vs))
          (Just (\_ -> pure $ Right initState))
      -- Here we can add some properties for traces:
      pure (_traceInitState trace === initState)

-- | test that we can generate a sequence of Tx that all STS validate given a (Proof era)
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
