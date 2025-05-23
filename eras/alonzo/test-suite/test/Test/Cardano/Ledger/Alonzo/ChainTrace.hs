{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Embed instances for (AlonzoEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.ChainTrace (
  tests,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  collectPlutusScriptsWithContext,
  evalPlutusScripts,
 )
import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY, AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..), mkPlutusScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..), totExUnits)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..), ScriptResult (..))
import Cardano.Ledger.Plutus.Language (plutusFromRunnable)
import Cardano.Ledger.Shelley.LedgerState hiding (circulation)
import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Lens.Micro
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen (sumCollateral)
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import Test.Cardano.Ledger.Alonzo.Trace ()
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Rules.Chain (
  CHAIN,
  ChainEvent (..),
  ChainState (..),
  TestChainPredicateFailure (..),
 )
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  forAllChainTrace,
  ledgerTraceFromBlock,
 )
import Test.Control.State.Transition.Trace (SourceSignalTarget (..), sourceSignalTargets)
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  (.&&.),
  (===),
 )
import Test.Tasty
import qualified Test.Tasty.QuickCheck as TQC

instance Embed (AlonzoBBODY AlonzoEra) (CHAIN AlonzoEra) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

traceLen :: Word64
traceLen = 100

data HasPlutus = HasPlutus | NoPlutus
  deriving (Show)

tests :: TestTree
tests =
  TQC.testProperty "alonzo specific" $
    forAllChainTrace @AlonzoEra traceLen defaultConstants $ \tr ->
      conjoin $ map alonzoSpecificProps (sourceSignalTargets tr)

alonzoSpecificProps ::
  SourceSignalTarget (CHAIN AlonzoEra) ->
  Property
alonzoSpecificProps SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map alonzoSpecificPropsLEDGER $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock chainSt block
    pp = (view curPParamsEpochStateL . nesEs . chainNes) tickedChainSt
    alonzoSpecificPropsLEDGER :: SourceSignalTarget (AlonzoLEDGER AlonzoEra) -> Property
    alonzoSpecificPropsLEDGER
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = UTxO u, utxosDeposited = dp, utxosFees = f} ds
        , signal = tx
        , target = LedgerState UTxOState {utxosUtxo = UTxO u', utxosDeposited = dp', utxosFees = f'} ds'
        } =
        let isValid' = tx ^. isValidTxL
            noNewUTxO = u' `Map.isSubmapOf` u
            collateralInFees = f <> sumCollateral tx (UTxO u) == f'
            utxoConsumed = not $ u `Map.isSubmapOf` u'
            allScripts = tx ^. witsTxL . scriptTxWitsL
            hasPlutus = if all (isNativeScript @AlonzoEra) allScripts then NoPlutus else HasPlutus
            totEU = totExUnits tx
            nonTrivialExU = exUnitsMem totEU > 0 && exUnitsSteps totEU > 0
            collected =
              -- Note that none of our plutus scripts use validity intervals,
              -- so it is safe to use anything for the epech info and the system start.
              case collectPlutusScriptsWithContext
                (fixedEpochInfo (EpochSize 100) (mkSlotLength 1)) -- arbitrary
                (SystemStart $ posixSecondsToUTCTime 0) -- arbitrary
                pp
                tx
                (UTxO u) of
                Left e -> error $ "Plutus script collection error: " <> show e
                Right c -> c
            collectedScripts =
              Set.fromList
                [ plutus
                | PlutusWithContext {pwcScript} <- collected
                , Just plutus <- [mkPlutusScript $ either id plutusFromRunnable pwcScript]
                ]
            suppliedPScrpts = Set.fromList [plutus | PlutusScript plutus <- Map.elems allScripts]
            expectedPScripts = collectedScripts == suppliedPScrpts
            allPlutusTrue = case evalPlutusScripts collected of
              Fails _ _ -> False
              Passes _ -> True
         in counterexample
              ( mconcat
                  [ "\nHas plutus scripts: "
                  , show hasPlutus
                  , "\nIs valid: "
                  , show isValid'
                  , "\nAt least one UTxO is consumed: "
                  , show utxoConsumed
                  , "\nNon trivial execution units: "
                  , show nonTrivialExU
                  , "\nReceived the expected plutus scripts: "
                  , show expectedPScripts
                  , "\nPlutus scripts all evaluate to true: "
                  , show allPlutusTrue
                  , "\nNo new UTxO: "
                  , show noNewUTxO
                  , "\nThe collateral amount was added to the fees: "
                  , show collateralInFees
                  , "\nThe deposit pot is unchanged: "
                  , show (dp == dp')
                  , "\nThe delegation state is unchanged: "
                  , show (ds == ds')
                  ]
              )
              ( counterexample "At least one UTxO is consumed" utxoConsumed
                  .&&. ( case (hasPlutus, isValid') of
                           (NoPlutus, IsValid True) -> totEU === ExUnits 0 0
                           (NoPlutus, IsValid False) -> counterexample "No Plutus scripts, but isValid == False" False
                           (HasPlutus, IsValid True) ->
                             conjoin
                               [ counterexample "Non trivial execution units" nonTrivialExU
                               , counterexample "Received the expected plutus scripts" expectedPScripts
                               , counterexample "Plutus scripts all evaluate to true" allPlutusTrue
                               ]
                           (HasPlutus, IsValid False) ->
                             conjoin
                               [ counterexample "No new UTxO" noNewUTxO
                               , counterexample "The collateral amount was added to the fees" collateralInFees
                               , dp === dp'
                               , ds === ds'
                               , counterexample "No failing Plutus scripts" $ not allPlutusTrue
                               ]
                       )
              )
