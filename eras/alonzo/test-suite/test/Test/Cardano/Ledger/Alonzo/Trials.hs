{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- HasTrace instances for AlonzoLEDGE
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export several sets of property tests for the Alonzo Era.
--     Also expert some functions that can be used to debug the Alonzo Era property test generators.
module Test.Cardano.Ledger.Alonzo.Trials
  ( alonzoPropertyTests,
    fastPropertyTests,
    genstuff,
    genAlonzoTx,
    genShelleyTx,
    genAlonzoBlock,
    genShelleyBlock,
    adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    minimalPropertyTests,
    onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    poolProperties,
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
    propertyTests,
    relevantCasesAreCovered,
    removedAfterPoolreap,
    payscript,
    stakescript,
    scripts,
    scriptspace,
    theutxo,
    alls,
    d1,
    d2,
    d3,
    d4,
    manytimes,
    search,
  )
where

import Cardano.Binary (ToCBOR)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Ledger (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Scripts (ppScript)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxBody ()
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Hashes (EraIndependentData)
import Cardano.Ledger.Pretty (PDoc, PrettyA (..))
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.BlockChain (Block)
import Cardano.Ledger.Shelley.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState,
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    PState,
    UTxOState,
  )
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Delegs (DelegsEnv)
import Cardano.Ledger.Shelley.Rules.Delpl (DelplEnv, DelplPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv)
import Cardano.Ledger.Shelley.TxBody (DCert, TxIn)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import Data.Default.Class (Default (def))
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField)
import qualified PlutusTx as P (Data (..))
import System.Timeout
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import qualified Test.Cardano.Ledger.Alonzo.PropertyTests as Alonzo
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Block (genBlock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), KeySpace (..), ScriptSpace (..), hashData)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinLEDGER_STS, allScripts, genUtxo0)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Generator.Trace.DCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (genAccountState)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.Cardano.Ledger.Shelley.PropertyTests
  ( adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    minimalPropertyTests,
    onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    poolProperties,
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
    propertyTests,
    relevantCasesAreCovered,
    removedAfterPoolreap,
  )
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Tasty
import Test.Tasty.QuickCheck

-- ======================================================================================
-- It is incredably hard to debug property test generators.  These functions mimic the
-- set up of a property test, so one can inspect some randomly generatated transactions

-- | Different property test generators depend upon a wide variety of of inputs. This function generates random
--     versions of all these inputs, and lets the user select which of these inputs he needs to make a generator.
--     See genAlonzoTx and genAlonzoBlock as examples of its use.
genstuff ::
  (EraGen era, Default (State (Core.EraRule "PPUP" era))) =>
  Proxy era ->
  ( GenEnv era ->
    ChainState era ->
    NewEpochState era ->
    EpochState era ->
    LedgerState era ->
    Core.PParams era ->
    Cardano.Ledger.Shelley.LedgerState.UTxOState era ->
    DPState (Crypto era) ->
    DState (Crypto era) ->
    PState (Crypto era) ->
    Gen b
  ) ->
  Gen b
genstuff proxy f =
  do
    let genenv = (genEnv proxy)
    either' <- mkGenesisChainState genenv (IRC ())
    case either' of
      Left _z -> error ("OOPS")
      Right chainstate ->
        let newepochstate = chainNes chainstate
            epochstate = nesEs newepochstate
            ledgerstate = esLState epochstate
            pparams = esPp epochstate
            utxostate = _utxoState ledgerstate
            dpstate = _delegationState ledgerstate
            dstate = _dstate dpstate
            pstate = _pstate dpstate
         in (f genenv chainstate newepochstate epochstate ledgerstate pparams utxostate dpstate dstate pstate)

-- ======================================================================
-- The following genXXX let one observe example generated XXX things
-- these are very usefull to visualize what the the EraGen instances are doing.

type A = AlonzoEra TestCrypto

type L = AlonzoLEDGER A

ap :: Proxy A
ap = Proxy

-- The AlonzoLEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance
  ( EraGen era,
    UsesTxBody era,
    UsesTxOut era,
    UsesValue era,
    UsesAuxiliary era,
    Mock (Crypto era),
    MinLEDGER_STS era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ DelplPredicateFailure era,
    Embed (Core.EraRule "DELEGS" era) (AlonzoLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (AlonzoLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Core.Tx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era)),
    Core.Tx era ~ ValidatedTx era
  ) =>
  TQC.HasTrace (AlonzoLEDGER era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgerEnv (SlotNo 0) 0
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen genenv env state = genTx genenv env state

  shrinkSignal _ = [] -- TODO add some kind of Shrinker?

  type BaseEnv (AlonzoLEDGER era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- An initial (mostly empty) LedgerEnv
ledgerEnv :: forall era. Default (Core.PParams era) => LedgerEnv era
ledgerEnv = LedgerEnv (SlotNo 0) 0 def (AccountState (Coin 0) (Coin 0))

genAlonzoTx :: Gen (Core.Tx A)
genAlonzoTx = genstuff ap (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genAlonzoBlock :: Gen (Block BHeader A)
genAlonzoBlock = genstuff ap (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

genShelleyTx :: Gen (Core.Tx (ShelleyEra TestCrypto))
genShelleyTx =
  genstuff
    (Proxy @(ShelleyEra TestCrypto))
    (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genShelleyBlock :: Gen (Block BHeader (ShelleyEra TestCrypto))
genShelleyBlock = genstuff (Proxy @(ShelleyEra TestCrypto)) (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

-- ==================================================================================================
-- Scripts are generated when we call genEnv. They are stored in fields inside the GenEnv structure.
-- scripts, payscript, and stakescript let one observe the 'nth' generated script. Very usefull
-- when debugging a Scriptic instance.

keys :: KeySpace A
_constants :: Constants
scriptspace :: ScriptSpace A
genenv0 :: GenEnv A
genenv0@(GenEnv keys scriptspace _constants) = genEnv (Proxy @A)

-- In scripts, n ranges over [0..149]
scripts :: Int -> (PDoc, PDoc)
scripts n = (\(x, y) -> (ppScript x, ppScript y)) ((ksMSigScripts keys) !! n)

-- In payscript and stakescript, n ranges over [0..29]
payscript :: Int -> (String, PDoc)
payscript n = (\(x, (y, _z)) -> (show x, ppScript y)) ((Map.toList (ksIndexedPayScripts keys)) !! n)

stakescript :: Int -> (String, PDoc)
stakescript n = (\(x, (y, _z)) -> (show x, ppScript y)) ((Map.toList (ksIndexedStakeScripts keys)) !! n)

theutxo :: IO ()
theutxo = do
  utx <- generate (genUtxo0 genenv0)
  putStrLn (show (prettyA utx))

alls :: [(PDoc, PDoc)]
alls = (\(x, y) -> (ppScript x, ppScript y)) <$> (allScripts @A _constants)

d1 :: P.Data
d1 = P.I 4

d2 :: Data A
d2 = (Data (P.I 4))

d3 :: SafeHash TestCrypto EraIndependentData
d3 = hashAnnotated d2

d4 :: SafeHash TestCrypto EraIndependentData
d4 = hashData @A d1

-- ====================================================================================
-- A few sets of property tests we can use to run in different Scenarios.

-- | The same property tests run in all the other Eras, specialized to the Alonzo Era.
alonzoPropertyTests :: TestTree
alonzoPropertyTests =
  testGroup
    "Alonzo property tests"
    [ propertyTests @A @L,
      Alonzo.propertyTests
    ]

-- | A select subset of all the property tests
fastPropertyTests :: TestTree
fastPropertyTests =
  testGroup
    "Fast Alonzo Property Tests"
    [ testProperty
        "total amount of Ada is preserved (Chain)"
        (withMaxSuccess 50 (adaPreservationChain @A @L))
    ]

-- ==========================================================================================
-- When debugging property tests failures, it is usefull to run a test with a given replay
-- value, or a certain number of times. manytimes is a template for how to do this.

-- | Run a a single testmany times. Uncomment out  QuickCheckReplay or QuickCheckVerbose to control things.
manytimes :: String -> Int -> Int -> IO ()
manytimes prop count _seed =
  defaultMain $
    ( localOption (QuickCheckReplay (Just _seed)) $
        localOption (QuickCheckShowReplay True) $
          -- localOption (QuickCheckVerbose True) $
          case prop of
            "valid" ->
              ( testProperty
                  "Only valid CHAIN STS signals are generated"
                  (withMaxSuccess count (onlyValidLedgerSignalsAreGenerated @A @L))
              )
            "ada" ->
              ( testProperty
                  "collection of Ada preservation properties:"
                  (withMaxSuccess count (adaPreservationChain @A @L))
              )
            "deleg" ->
              ( testProperty
                  "STS Rules - Delegation Properties"
                  (withMaxSuccess count (delegProperties @A))
              )
            other -> error ("unknown test: " ++ other)
    )

-- ==============================================================================
-- try to find a quick failure. A quick failure helps debugging turnaround time.

-- | Run the test, using replay 'seed', timeing out after 'seconds', return 'Nothing' if it timesout.
--   This means the test did not complete in the time alloted. If all tests succeed, it does not return at all,
--   it raises the Exception: ExitSuccess, if it fails in the time allotted it returns (Just ()).
--   Which is what we are looking for.
searchForQuickFailure :: Int -> Int -> IO (Maybe ())
searchForQuickFailure seconds seed = do
  timeout (seconds * 1000000) $
    defaultMain
      ( localOption
          (QuickCheckReplay (Just seed))
          -- some properties we might use
          (testProperty "preserves ADA" $ adaPreservationChain @A @L)
          -- (testProperty "Delegation Properties" (delegProperties @A))
          -- fastPropertyTests
          -- (propertyTests @A)
          -- (testProperty "Only valid CHAIN STS signals are generated" (onlyValidLedgerSignalsAreGenerated @A))
      )

-- | search for a quick failure using replay seeds 'low' .. 'high'
search :: Int -> Int -> IO ()
search low high = mapM_ zzz [low .. high]
  where
    zzz n = do
      ans <- searchForQuickFailure 20 n
      case ans of
        Nothing -> putStrLn ("OK " ++ show n) >> pure (Right ())
        Just () -> putStrLn ("Fails " ++ show n) >> pure (Left n)
