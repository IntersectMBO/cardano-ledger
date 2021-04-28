{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Cardano.Ledger.Alonzo.Trials where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..), PParamsUpdate)
import Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBBODY)
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts (Script (..), ppScript)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Pretty (PDoc, PrettyA (prettyA))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Constraints (UsesTxBody, UsesTxOut)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (Embed (..), IRC (..), STS (..))
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace, forAllTraceFromInitState)
import Data.Default.Class (Default (def))
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Shelley.Spec.Ledger.API (ApplyBlock)
import Shelley.Spec.Ledger.API.Protocol (GetLedgerView)
import Shelley.Spec.Ledger.API.Validation (ApplyBlock)
import Shelley.Spec.Ledger.LedgerState (AccountState (..), DPState (..), DState, EpochState (..), LedgerState (..), NewEpochState (..), PState, UTxOState)
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainPredicateFailure (..), ChainState (..), initialShelleyState)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..), LedgerPredicateFailure (UtxowFailure))
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Alonzo.Examples.Utxow (plutusScriptExamples, utxowExamples)
import Test.Cardano.Ledger.Alonzo.Golden as Golden
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), KeySpace (..), mkBlock)
import Test.Shelley.Spec.Ledger.Generator.EraGen
  ( EraGen (..),
    genEraAuxiliaryData,
    genEraPParamsDelta,
    genEraTxBody,
    genGenesisValue,
    genUtxo0,
    updateEraTxBody,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv, genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (ScriptClass (..), baseScripts, keyPairs, mkScriptsFromKeyPair, someScripts)
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState, registerGenesisStaking)
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger (genAccountState, mkGenesisLedgerState)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.PropertyTests
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
import Test.Shelley.Spec.Ledger.Rules.TestChain
  ( adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    forAllChainTrace,
    poolProperties,
    removedAfterPoolreap,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( ChainProperty,
    maxLLSupply,
    mkHash,
    testGlobals,
  )
import Test.Tasty

kps = take 10 $ keyPairs @TestCrypto (geConstants ag)

pretty :: PrettyA x => x -> PDoc
pretty = prettyA

ppS = ppScript

ledgerEnv :: forall era. (Default (Core.PParams era)) => LedgerEnv era
ledgerEnv = LedgerEnv (SlotNo 0) 0 def (AccountState (Coin 0) (Coin 0))

baz = genTx ag ledgerEnv

ap :: Proxy (AlonzoEra TestCrypto)
ap = Proxy @(AlonzoEra TestCrypto)

ag :: GenEnv (AlonzoEra TestCrypto)
ag = genEnv ap

genstuff ::
  (EraGen era, Default (State (Core.EraRule "PPUP" era))) =>
  proxy era ->
  ( GenEnv era ->
    ChainState era ->
    NewEpochState era ->
    EpochState era ->
    LedgerState era ->
    Core.PParams era ->
    Shelley.Spec.Ledger.LedgerState.UTxOState era ->
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

genAlonzoTx = genstuff ap (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genShelleyTx =
  genstuff
    (Proxy @(ShelleyEra TestCrypto))
    (\genv _cs _nep _ep _ls _pp utxo dp _d _p -> genTx genv ledgerEnv (utxo, dp))

genAlonzoBlock = genstuff ap (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

genShelleyBlock = genstuff (Proxy @(ShelleyEra TestCrypto)) (\genv cs _nep _ep _ls _pp _utxo _dp _d _p -> genBlock genv cs)

foo = do
  either' <- mkGenesisChainState (genEnv ap) (IRC ())
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
       in pure chainstate

chain = generate foo

env@(GenEnv keys constants) = genEnv (Proxy @(AlonzoEra TestCrypto))

-- in scripts n ranges over [0..149]
scripts n = (\(x, y) -> (ppS x, ppS y)) ((ksMSigScripts keys) !! n)

-- in payscript and stakescript n ranges over [0..29]
payscript n = (\(x, (y, _z)) -> (show x, ppS y)) ((Map.toList (ksIndexedPayScripts keys)) !! n)

stakescript n = (\(x, (y, _z)) -> (show x, ppS y)) ((Map.toList (ksIndexedStakeScripts keys)) !! n)

test = defaultMain (minimalPropertyTests @AT)

bar = do cs <- foo; genBlock ag cs

acs = mkGenesisChainState ag

als = mkGenesisLedgerState ag

instance Embed (AlonzoBBODY (AlonzoEra TestCrypto)) (CHAIN (AlonzoEra TestCrypto)) where
  wrapFailed = BbodyFailure

instance Embed (AlonzoUTXOW (AlonzoEra TestCrypto)) (LEDGER (AlonzoEra TestCrypto)) where
  wrapFailed = UtxowFailure

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      Translation.tests,
      CDDL.tests 5,
      Golden.goldenUTxOEntryMinAda,
      plutusScriptExamples,
      utxowExamples
    ]

{-
alonzoProperty = testGroup
    "Alonzo minimal property tests"
    [ minimalPropertyTests @(AlonzoEra TestCrypto)
    ]
-}

type AT = AlonzoEra TestCrypto

type T = TestCrypto

main :: IO ()
main = defaultMain tests
