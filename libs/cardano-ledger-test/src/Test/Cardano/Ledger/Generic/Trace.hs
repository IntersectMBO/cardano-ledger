{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Trace where

-- =========================================================================

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (body))
import qualified Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Babbage.Rules.Ledger ()
-- import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (..))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (..))
import Cardano.Ledger.Babbage.TxBody (certs')
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty
  ( PDoc,
    ppInt,
    ppList,
    ppMap,
    ppRecord,
    ppSafeHash,
    ppSet,
    ppSlotNo,
    ppString,
    ppWord64,
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.AdaPots (totalAdaPotsES)
import Cardano.Ledger.Shelley.Constraints (UsesValue)
-- import Cardano.Ledger.Shelley.EpochBoundary (SnapShots (..))

-- import Debug.Trace

-- pcIndividualPoolStake,

import Cardano.Ledger.Shelley.EpochBoundary (SnapShots (..))
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
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
import Control.Monad.Trans.RWS.Strict (get, gets)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Control.State.Transition.Trace (Trace (..), lastState)
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..), traceFromInitState)
import Data.Default.Class (Default (def))
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity (runIdentity))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
import System.IO.Unsafe (unsafePerformIO)
import Test.Cardano.Ledger.Generic.ApplyTx (applyTx)
import Test.Cardano.Ledger.Generic.Fields (TxBodyField (..), abstractTxBody)
import Test.Cardano.Ledger.Generic.Functions
  ( allInputs,
    getBody,
    getScriptWits,
    getTxOutCoin,
    getWitnesses,
    isValid',
    totalAda,
    txoutFields,
  )
import Test.Cardano.Ledger.Generic.GenState
  ( GenEnv (..),
    GenRS,
    GenSize (..),
    GenState (..),
    getBlocksizeMax,
    getReserves,
    getSlot,
    getSlotDelta,
    getTreasury,
    initStableFields,
    initialLedgerState,
    modifyModel,
    runGenRS,
  )
import Test.Cardano.Ledger.Generic.MockChain
import Test.Cardano.Ledger.Generic.ModelState (MUtxo, stashedAVVMAddressesZero)
import Test.Cardano.Ledger.Generic.PrettyCore
  ( pcCoin,
    pcCredential,
    pcDCert,
    pcKeyHash,
    pcPoolParams,
    pcScript,
    pcScriptHash,
    pcTxBodyField,
    pcTxIn,
    scriptSummary,
  )
-- import Test.Cardano.Ledger.Generic.PrettyCore (pcCoin, pcTx, pcTxBody, pcTxIn)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.Cardano.Ledger.Generic.TxGen (genValidatedTx)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (stakeDistr)
-- import Test.Cardano.Ledger.Shelley.Rules.TestChain (stakeDistr)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase, testGlobals)
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- import Cardano.Ledger.Pretty(ppSnapShot)

-- ===========================================

-- | Generate a Core.Tx and an internal Model of the state after the tx
--   has been applied. That model can be used to generate the next Tx
genRsTxAndModel :: Reflect era => Proof era -> Int -> SlotNo -> GenRS era (Core.Tx era)
genRsTxAndModel proof n slot = do
  (_, tx) <- genValidatedTx proof slot
  modifyModel (\model -> applyTx proof n slot model tx)
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
genRsTxSeq proof this lastN ans _slot | this >= lastN = do
  seq
    (unsafePerformIO (writeIORef theVector (TT proof (reverse ans))))
    (pure (Vector.fromList (reverse ans)))
genRsTxSeq proof this lastN ans slot = do
  maxBlockSize <- getBlocksizeMax <$> get
  n <- lift $ choose (2 :: Int, fromIntegral maxBlockSize)
  txs <- forM [0 .. n - 1] (\i -> genRsTxAndModel proof (this + i) slot)
  newSlotRange <- gets getSlotDelta
  nextSlotNo <- lift $ SlotNo . (+ (unSlotNo slot)) <$> choose newSlotRange
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

initialMockChainState ::
  Reflect era =>
  Proof era ->
  GenState era ->
  (MockChainState era)
initialMockChainState proof gstate =
  MockChainState newepochstate (getSlot gstate) 0
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
snaps (LedgerState UTxOState {_utxo = u, _fees = f} (DPState dstate pstate)) = SnapShots snap snap snap f
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
  GenState era ->
  String
raiseMockError slot (SlotNo next) epochstate pdfs txs GenState {..} =
  let utxo = unUTxO $ (_utxo . lsUTxOState . esLState) epochstate
      _poolParams = (_pParams . dpsPState . lsDPState . esLState) epochstate
   in show $
        vsep
          [ ppString "===================================",
            ppString "UTxO\n" <> pcSmallUTxO reify utxo txs,
            ppString "===================================",
            ppString "Stable Pools\n" <> ppSet pcKeyHash gsStablePools,
            ppString "===================================",
            -- ppString "Initial Pool Distr\n" <> ppMap pcKeyHash pcIndividualPoolStake gsInitialPoolDistr,
            ppString "===================================",
            ppString "Initial Pool Params\n" <> ppMap pcKeyHash pcPoolParams gsInitialPoolParams,
            ppString "===================================",
            ppString "Stable Delegators\n" <> ppSet pcCredential gsStableDelegators,
            ppString "===================================",
            ppString "Initial Rewards\n" <> ppMap pcCredential pcCoin gsInitialRewards,
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
            ppString "Script Witnesses\n"
              <> ppMap
                pcScriptHash
                (scriptSummary @era reify)
                (Map.restrictKeys gsScripts (badScripts reify pdfs))
                -- ppString "===================================",
                -- ppString "Real Pool Params\n" <> ppMap pcKeyHash pcPoolParams poolParams
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
    let NewEpochState epochnum _ _ epochstate _ pooldistr _ = newepoch
    issuerkey <- chooseIssuer (epochnum, lastSlot, count) pooldistr
    let (txs, nextSlotNo) = txss ! count
    -- Assmble it into a MockBlock
    let mockblock = MockBlock issuerkey nextSlotNo txs
    -- Run the STS Rules for MOCKCHAIN with generated signal

    case runShelleyBase (applySTSTest (TRC @(MOCKCHAIN era) ((), mcs, mockblock))) of
      Left pdfs ->
        let txsl = Fold.toList txs
         in trace
              (raiseMockError lastSlot nextSlotNo epochstate pdfs txsl gs)
              (error "FAILS")
      Right mcs2 -> seq mcs2 (pure mockblock)

  shrinkSignal _ = []

--  shrinkSignal (MockBlock _ _ xs) | SS.null xs = []
--  shrinkSignal (MockBlock i s xs) = [MockBlock i s (SS.drop 1 xs), MockBlock i s (SS.take (SS.length xs - 1) xs)]

mapProportion :: (EpochNo, Word64, Int) -> (v -> Int) -> Map.Map k v -> Gen k
mapProportion (epochnum, lastSlot, count) toInt m =
  if null pairs
    then
      error
        ( "There are no stakepools to choose an issuer from"
            ++ "\n  epoch = "
            ++ show epochnum
            ++ "\n  last slot ="
            ++ show lastSlot
            ++ "\n  index of Tx in the trace = "
            ++ show count
        )
    else
      if all (\(n, _k) -> n == 0) pairs
        then snd (head pairs)
        else -- All stakepools have zero Stake, choose issuer arbitrarily. possible, but rare.
          frequency pairs
  where
    pairs = [(toInt v, pure k) | (k, v) <- Map.toList m]

chooseIssuer :: (EpochNo, Word64, Int) -> PoolDistr crypto -> Gen (KeyHash 'StakePool crypto)
chooseIssuer triple (PoolDistr m) = mapProportion triple getInt m
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
  let initState = initialMockChainState proof genstate
  traceFromInitState @(MOCKCHAIN era)
    testGlobals
    (fromIntegral (length vs))
    (Gen1 vs genstate)
    (Just (\_ -> pure $ Right initState))

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
  trace1 <- genTrace proof numTxInTrace gsize (initStableFields proof)
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
      (vs, genstate) <- genTxSeq proof gsize n (initStableFields proof)
      let initState = initialMockChainState proof genstate
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
main =
  let proof = Babbage Mock
      numTx = 150
      gensize = def {blocksizeMax = 4, slotDelta = (3, 7)}
   in defaultMain $
        testProperty (show proof ++ " era. Trace length = " ++ show numTx) $
          traceProp proof numTx gensize (\firstSt lastSt -> totalAda firstSt === totalAda lastSt)

data TT where
  TT :: Proof era -> [(StrictSeq (Core.Tx era), SlotNo)] -> TT

theVector :: IORef TT
theVector = unsafePerformIO (newIORef (TT (Babbage Mock) []))

showVector :: (forall era. Proof era -> [Core.Tx era] -> SlotNo -> PDoc) -> IO ()
showVector pretty = do
  xs <- readIORef theVector
  case xs of
    TT _ [] -> print ("NONE" :: String)
    TT proof ys -> mapM_ (\(ss, slot) -> print (pretty proof (Fold.toList ss) slot)) ys

main3 :: IO ()
main3 = showVector pretty
  where
    pretty :: Proof era -> [Core.Tx era] -> SlotNo -> PDoc
    pretty (Babbage Mock) xs slot =
      vsep
        [ ppSlotNo slot,
          vsep (map (\tx -> ppList pcDCert (Fold.toList (certs' (body tx)))) xs)
        ]
    pretty p _ _ = ppString ("main3 does not work in era " ++ show p)

main2 :: IO ()
main2 = defaultMain (chainTest (Babbage Mock) 100 def)

go :: IO ()
go = do
  let proof = Babbage Mock
  ((), gstate) <- generate $ runGenRS proof def (initStableFields proof)
  let mcst = initialMockChainState proof gstate
  let del = gsInitialPoolParams gstate
  print (ppMockChainState mcst)
  -- let SnapShots s1 _ _ _  = (esSnapshots . nesEs . mcsNes) mcst
  print (ppMap pcKeyHash pcPoolParams del)
