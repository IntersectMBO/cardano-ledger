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

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import qualified Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Ledger ()
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty
  ( PDoc,
    ppInt,
    ppKeyHash,
    ppList,
    ppMap,
    ppRecord,
    ppSafeHash,
    ppSet,
    ppString,
    ppWord64,
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.AdaPots (totalAdaPotsES)
import Cardano.Ledger.Shelley.Constraints (UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    StashedAVVMAddresses,
    UTxOState (..),
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledgers (LedgersPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow (UtxowPredicateFailure (ScriptWitnessNotValidatingUTXOW))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad (forM)
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
import Debug.Trace
import GHC.Word (Word64)
import Prettyprinter (hsep, parens, vsep)
import Test.Cardano.Ledger.Generic.ApplyTx (applyTx)
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), abstractTxBody)
import Test.Cardano.Ledger.Generic.Functions
  ( allInputs,
    getBody,
    getScriptWits,
    getTxOutCoin,
    getWitnesses,
    isValid',
    txoutFields,
  )
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenSize (..),
    GenState (..),
    genPool,
    getBlocksizeMax,
    getReserves,
    getSlot,
    getTreasury,
    initialLedgerState,
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.MockChain
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, stashedAVVMAddressesZero)
import Test.Cardano.Ledger.Generic.PrettyCore (pcCoin, pcCredential, pcScript, pcScriptHash, pcTxBodyField, pcTxIn, scriptSummary)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen (genValidatedTx)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (stakeDistr)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- ===========================================

-- | Generate a Core.Tx and an internal Model of the state after the tx
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel :: Reflect era => Proof era -> Int -> SlotNo -> GenRS era (Core.Tx era)
genRsTxAndModel proof n slot = do
  (_, tx) <- genValidatedTx proof slot
  modifyModel (\model -> applyTx proof n model tx)
  pure tx

-- | Generate a Vector of (StrictSeq (Core.Tx era))  representing a (Vector Block)
genRsTxSeq ::
  forall era.
  Reflect era =>
  Proof era ->
  Int ->
  Int ->
  [(StrictSeq (Core.Tx era), SlotNo)] ->
  SlotNo ->
  GenRS era (Vector (StrictSeq (Core.Tx era), SlotNo))
genRsTxSeq _ this lastN ans _slot | this >= lastN = pure (Vector.fromList (reverse ans))
genRsTxSeq proof this lastN ans slot = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  txs <- forM [0 .. n - 1] (\i -> genRsTxAndModel proof (this + i) slot)
  nextSlotNo <- lift $ SlotNo . (+ (unSlotNo slot)) <$> choose (5, 12)
  genRsTxSeq proof (this + n) lastN ((SS.fromList txs, slot) : ans) nextSlotNo

-- | Generate a Vector of Blocks, and an initial LedgerState
genTxSeq ::
  forall era.
  Reflect era =>
  Proof era -> -- Proof of the Era we want to generate the sequence in
  GenSize -> -- Size of things the generated code should adhere to
  Int -> -- The number of Tx in the sequence
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Vector (StrictSeq (Core.Tx era), SlotNo), GenState era)
genTxSeq proof gensize numTx initialize = do
  runGenRS proof gensize (initialize >> genRsTxSeq proof 0 numTx [] (SlotNo $ 1))

runTest :: IO ()
runTest = do
  (v, _) <- generate $ genTxSeq (Babbage Mock) def 20 (pure ())
  print (Vector.length v)

-- ==================================================================
-- Constructing the "real", initial NewEpochState, from the GenState

genMockChainState ::
  Reflect era =>
  Proof era ->
  GenState era ->
  Gen (MockChainState era)
genMockChainState proof gstate = 
  pure $ MockChainState newepochstate (getSlot gstate) 0
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

makeEpochState :: Reflect era => GenState era -> LedgerState era -> EpochState era
makeEpochState gstate ledgerstate =
  EpochState
    { esAccountState = AccountState (getTreasury gstate) (getReserves gstate),
      esSnapshots = snaps ledgerstate,
      esLState = ledgerstate,
      esPrevPp = gePParams (gsGenEnv gstate),
      esPp = gePParams (gsGenEnv gstate),
      esNonMyopic = def
    }

snaps :: Era era => LedgerState era -> SnapShots (Crypto era)
snaps (LedgerState UTxOState {_utxo = u} (DPState dstate pstate)) = SnapShots snap snap snap mempty
  where
    snap = stakeDistr u dstate pstate

-- ==============================================================================

-- | Turn a UTxO into a smaller UTxO, with only entries mentioned in
--   the inputs of 'txs' ,  then pretty print it.
pcSmallUTxO :: Proof era -> MUtxo era -> [Core.Tx era] -> PDoc
pcSmallUTxO proof u txs = ppMap pcTxIn (shortTxOut proof) m
  where
    keys = Set.unions (map f txs)
    f tx = allInputs proof (getBody proof tx)
    m = Map.restrictKeys u keys

raiseMockError ::
  forall era.
  (UsesValue era, Reflect era) =>
  Word64 ->
  SlotNo ->
  EpochState era ->
  [MockChainFailure era] ->
  [Core.Tx era] ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Set.Set (KeyHash 'StakePool (Crypto era)) ->
  String
raiseMockError slot (SlotNo next) epochstate pdfs txs scripts honestPools =
  let utxo = unUTxO $ (_utxo . lsUTxOState . esLState) epochstate
   in show $
        vsep
          [ pcSmallUTxO reify utxo txs,
            ppString "===================================",
            ppString "Honest Pools" <> ppSet ppKeyHash honestPools,
            ppString "===================================",
            showBlock utxo txs,
            ppString "===================================",
            ppString (show (totalAdaPotsES epochstate)),
            ppString "===================================",
            ppList (ppMockChainFailure reify) pdfs,
            ppString "===================================",
            ppString "Last Slot " <> ppWord64 slot,
            ppString "Current Slot " <> ppWord64 next,
            ppString "===================================",
            ppMap pcScriptHash (scriptSummary @era reify) (Map.restrictKeys scripts (badScripts reify pdfs))
          ]

badScripts :: Proof era -> [MockChainFailure era] -> Set.Set (ScriptHash (Crypto era))
badScripts proof xs = Fold.foldl' (\s mcf -> Set.union s (getw proof mcf)) Set.empty xs
  where
    getw :: Proof era -> MockChainFailure era -> Set.Set (ScriptHash (Crypto era))
    getw
      (Babbage _)
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  ( FromAlonzoUtxowFail
                      ( WrappedShelleyEraFailure
                          (ScriptWitnessNotValidatingUTXOW set)
                        )
                    )
                )
            )
        ) = set
    getw
      (Alonzo _)
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  ( WrappedShelleyEraFailure
                      (ScriptWitnessNotValidatingUTXOW set)
                    )
                )
            )
        ) = set
    getw
      (Mary _)
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  (ScriptWitnessNotValidatingUTXOW set)
                )
            )
        ) = set
    getw
      (Allegra _)
      ( MockChainFromLedgersFailure
          ( LedgerFailure
              ( UtxowFailure
                  (ScriptWitnessNotValidatingUTXOW set)
                )
            )
        ) = set
    getw _ _ = Set.empty

showBlock :: forall era. Reflect era => MUtxo era -> [Core.Tx era] -> PDoc
showBlock u txs = ppList pppair (zip txs [0 ..])
  where
    pppair (tx, n) =
      let body = getBody reify tx
       in vsep
            [ ppString "\n###########",
              ppInt n,
              ppSafeHash (hashAnnotated body),
              smartTxBody reify u body,
              ppString (show (isValid' reify tx)),
              ppMap pcScriptHash (pcScript @era reify) (getScriptWits reify (getWitnesses reify tx)),
              ppString "\n"
            ]

shortTxOut :: Proof era -> Core.TxOut era -> PDoc
shortTxOut proof out = case txoutFields proof out of
  (Addr _ pay _, _, _) -> hsep ["Out", parens $ hsep ["Addr", pcCredential pay], pcCoin (getTxOutCoin proof out)]
  _ -> error "Bootstrap Address in shortTxOut"

smartTxBody :: Reflect era => Proof era -> MUtxo era -> Core.TxBody era -> PDoc
smartTxBody proof u txbody = ppRecord "TxBody" pairs
  where
    fields = abstractTxBody proof txbody
    pairs = concat (map help fields)
    help (Inputs s) = [("spend inputs", ppSet pcIn s)]
    help (Collateral s) = [("coll inputs", ppSet pcIn s)]
    help x = pcTxBodyField proof x
    pcIn x =
      hsep
        [ pcTxIn x,
          " -> ",
          case Map.lookup x u of Just out -> shortTxOut proof out; Nothing -> "?"
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

data Gen1 era = Gen1 (Vector (StrictSeq (Core.Tx era), SlotNo)) (GenState era)

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

  sigGen (Gen1 txss gs) () mcs@(MockChainState newepoch (SlotNo lastSlot) count) = do
    let NewEpochState _epochnum _ _ epochstate _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer pooldistr
    let (txs, nextSlotNo) = txss ! count
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal

    case runShelleyBase (applySTSTest (TRC @(MOCKCHAIN era) ((), mcs, mockblock))) of
      Left pdfs ->
        let txsl = Fold.toList txs
            scs = gsScripts gs
            honestPools = gsHonestPools gs
         in trace
              (raiseMockError lastSlot nextSlotNo epochstate pdfs txsl scs honestPools)
              (error "FAILS")
      -- Left pdfs -> trace ("Discard\n"++show(ppList (ppMockChainFailure reify) pdfs)) discard -- TODO should we enable this?
      Right mcs2 -> seq mcs2 (pure mockblock)

  shrinkSignal (MockBlock _ _ xs) | SS.null xs = []
  shrinkSignal (MockBlock i s xs) = [MockBlock i s (SS.drop 1 xs), MockBlock i s (SS.take (SS.length xs - 1) xs)]

mapProportion :: (v -> Int) -> Map.Map k v -> Gen k
mapProportion toInt m =
  if null pairs
    then error "There are no stakepools to choose an issuer from"
    else
      if all (\(n, _k) -> n == 0) pairs
        then
          trace
            "All stakepools have zero stake, choose issuer arbitrarily. Probably caused by epoch boundary issue."
            (snd (head pairs))
        else frequency pairs
  where
    pairs = [(toInt v, pure k) | (k, v) <- Map.toList m]

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
  Int ->
  GenSize ->
  GenRS era () -> -- An arbitrary 'initialization action', to run before we generate the sequence
  -- use (pure ()) if you don't want or need initialization
  Gen (Trace (MOCKCHAIN era))
genTrace proof numTxInTrace gsize initialize = do
  (vs, genstate) <- genTxSeq proof gsize numTxInTrace initialize
  initState <- genMockChainState proof genstate
  traceFromInitState @(MOCKCHAIN era)
    testGlobals
    (fromIntegral (length vs))
    (Gen1 vs genstate)
    (Just (\_ -> pure $ Right initState))

makeSomePools :: Reflect era => GenRS era ()
makeSomePools = mapM_ (const genPool) [1 .. (5 :: Int)]

traceProp ::
  forall era prop.
  ( Reflect era,
    HasTrace (MOCKCHAIN era) (Gen1 era)
  ) =>
  Proof era ->
  Int ->
  GenSize ->
  (MockChainState era -> MockChainState era -> prop) ->
  Gen prop
traceProp proof numTxInTrace gsize f = do
  trace1 <- genTrace proof numTxInTrace gsize makeSomePools
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
  Int ->
  GenSize ->
  TestTree
chainTest proof n gsize = testProperty message action
  where
    message = show proof ++ " era."
    action = do
      (vs, genstate) <- genTxSeq proof gsize n makeSomePools
      initState <- genMockChainState proof genstate
      trace1 <-
        traceFromInitState @(MOCKCHAIN era)
          testGlobals
          (fromIntegral (length vs))
          (Gen1 vs genstate)
          (Just (\_ -> pure $ Right initState))
      -- Here is where we can add some properties for traces:
      pure $ (_traceInitState trace1 === initState)

testTraces :: Int -> TestTree
testTraces n =
  testGroup
    "MockChainTrace"
    [ chainTest (Babbage Mock) n def,
      chainTest (Alonzo Mock) n def,
      chainTest (Mary Mock) n def,
      chainTest (Allegra Mock) n def,
      chainTest (Shelley Mock) n def
    ]

main :: IO ()
main = defaultMain (chainTest (Mary Mock) 30 def)
