{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Embed instances for (AlonzoEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.PropertyTests where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (collectTwoPhaseScriptInputs, evalScripts)
import Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBBODY)
import Cardano.Ledger.Alonzo.Rules.Ledger (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..), totExUnits)
import Cardano.Ledger.Alonzo.TxInfo (ScriptResult (..))
import Cardano.Ledger.Era (ValidateScript (..))
import Cardano.Ledger.Shelley.LedgerState hiding (circulation)
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot (EpochSize (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition
import Control.State.Transition.Trace (SourceSignalTarget (..), sourceSignalTargets)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Records (HasField (..))
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen (sumCollateral)
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Shelley.Rules.Chain
  ( CHAIN,
    ChainEvent (..),
    ChainState (..),
    TestChainPredicateFailure (..),
  )
import Test.Cardano.Ledger.Shelley.Rules.TestChain (forAllChainTrace, ledgerTraceFromBlock)
import Test.QuickCheck (Property, conjoin, counterexample, (.&&.))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as TQC

type A = AlonzoEra TestCrypto

instance Embed (AlonzoBBODY A) (CHAIN A) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

traceLen :: Word64
traceLen = 100

data HasPlutus = HasPlutus | NoPlutus
  deriving (Show)

alonzoSpecificProps ::
  SourceSignalTarget (CHAIN A) ->
  Property
alonzoSpecificProps SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map alonzoSpecificPropsLEDGER $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock chainSt block
    pp = (esPp . nesEs . chainNes) tickedChainSt
    alonzoSpecificPropsLEDGER :: SourceSignalTarget (AlonzoLEDGER A) -> Property
    alonzoSpecificPropsLEDGER
      SourceSignalTarget
        { source = (UTxOState {_utxo = UTxO u, _deposited = dp, _fees = f}, ds),
          signal = tx,
          target = (UTxOState {_utxo = UTxO u', _deposited = dp', _fees = f'}, ds')
        } =
        let isValid' = getField @"isValid" tx
            noNewUTxO = u' `Map.isSubmapOf` u
            collateralInFees = f <> sumCollateral tx (UTxO u) == f'
            utxoConsumed = not $ u `Map.isSubmapOf` u'
            allScripts = getField @"txscripts" $ getField @"wits" tx
            hasPlutus = if all (isNativeScript @A) allScripts then NoPlutus else HasPlutus
            totEU = totExUnits tx
            nonTrivialExU = exUnitsMem totEU > 0 && exUnitsSteps totEU > 0
            collected =
              -- Note that none of our plutus scripts use validity intervals,
              -- so it is safe to use anything for the epech info and the system start.
              case collectTwoPhaseScriptInputs
                (fixedEpochInfo (EpochSize 100) (mkSlotLength 1)) -- arbitrary
                (SystemStart $ posixSecondsToUTCTime 0) -- arbitrary
                pp
                tx
                (UTxO u) of
                Left e -> error $ "Plutus script collection error: " <> show e
                Right c -> c
            collectedScripts = Set.fromList $ map (\(s, _, _, _) -> s) collected
            suppliedPScrpts = Set.fromList [PlutusScript v s | PlutusScript v s <- Map.elems allScripts]
            expectedPScripts = collectedScripts == suppliedPScrpts
            allPlutusTrue = case evalScripts tx collected of
              Fails _ -> False
              Passes -> True
         in counterexample
              ( mconcat
                  [ "\nHas plutus scripts: ",
                    show hasPlutus,
                    "\nIs valid: ",
                    show isValid',
                    "\nAt least one UTxO is consumed: ",
                    show utxoConsumed,
                    "\nNon trivial execution units: ",
                    show nonTrivialExU,
                    "\nReceived the expected plutus scripts: ",
                    show expectedPScripts,
                    "\nPlutus scripts all evaluate to true: ",
                    show allPlutusTrue,
                    "\nNo new UTxO: ",
                    show noNewUTxO,
                    "\nThe collateral amount was added to the fees: ",
                    show collateralInFees,
                    "\nThe deposit pot is unchanged: ",
                    show (dp == dp'),
                    "\nThe delegation state is unchanged: ",
                    show (ds == ds')
                  ]
              )
              ( counterexample "At least one UTxO is consumed" utxoConsumed
                  .&&. ( case (hasPlutus, isValid') of
                           (NoPlutus, IsValid True) -> totEU === ExUnits 0 0
                           (NoPlutus, IsValid False) -> counterexample "No Plutus scripts, but isValid == False" False
                           (HasPlutus, IsValid True) ->
                             conjoin
                               [ counterexample "Non trivial execution units" nonTrivialExU,
                                 counterexample "Received the expected plutus scripts" expectedPScripts,
                                 counterexample "Plutus scripts all evaluate to true" allPlutusTrue
                               ]
                           (HasPlutus, IsValid False) ->
                             conjoin
                               [ counterexample "No new UTxO" noNewUTxO,
                                 counterexample "The collateral amount was added to the fees" collateralInFees,
                                 dp === dp',
                                 ds === ds',
                                 counterexample "No failing Plutus scripts" $ not allPlutusTrue
                               ]
                       )
              )

alonzoTraceTests :: Property
alonzoTraceTests =
  forAllChainTrace @A traceLen $ \tr ->
    conjoin $ map alonzoSpecificProps (sourceSignalTargets tr)

propertyTests :: TestTree
propertyTests =
  TQC.testProperty "alonzo specific" alonzoTraceTests
