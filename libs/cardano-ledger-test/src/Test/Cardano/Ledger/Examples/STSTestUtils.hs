{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.STSTestUtils (
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  mkSingleRedeemer,
  someAddr,
  someKeys,
  someScriptAddr,
  testBBODY,
  runLEDGER,
  testUTXOW,
  testUTXOWsubset,
  testUTXOspecialCase,
  alwaysFailsHash,
  alwaysSucceedsHash,
  timelockScript,
  timelockHash,
  timelockStakeCred,
  genericCont,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript, pattern RequireTimeStart)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIx, ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraTxOut (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.API (Block, LedgerEnv (..), UtxoEnv (..))
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (LedgerState, UTxOState, smartUTxOState)
import Cardano.Ledger.Shelley.Rules (BbodyEnv (..), ShelleyBbodyState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import GHC.Stack
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (..), runSTS, runSTS')
import Test.Cardano.Ledger.Shelley.Era (ShelleyEraTest)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp)

-- =================================================================
-- =========================  Shared data  =========================
--   Data with specific semantics ("constants")
-- =================================================================

alwaysFailsHash :: forall era. ShelleyEraScript era => ScriptHash
alwaysFailsHash = hashScript @era . fromNativeScript $ RequireAnyOf mempty

alwaysSucceedsHash :: forall era. ShelleyEraScript era => ScriptHash
alwaysSucceedsHash = hashScript @era . fromNativeScript $ RequireAllOf mempty

someKeys :: KeyPair 'Payment
someKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

someAddr :: Addr
someAddr = mkAddr someKeys $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)

-- Create an address with a given payment script.
someScriptAddr :: forall era. EraScript era => Script era -> Addr
someScriptAddr s = mkAddr (hashScript s) $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 0)

timelockScript :: AllegraEraScript era => SlotNo -> Script era
timelockScript s =
  fromNativeScript $
    RequireAllOf
      [ RequireSignature $ theKeyHash 1
      , RequireTimeStart (100 + s)
      ]

timelockHash ::
  forall era.
  AllegraEraScript era =>
  SlotNo ->
  ScriptHash
timelockHash n = hashScript @era $ timelockScript n

timelockStakeCred :: forall era. AllegraEraScript era => StakeCredential
timelockStakeCred = ScriptHashObj (timelockHash @era 2)

-- ======================================================================
-- ========================= Initial Utxo ===============================
-- ======================================================================

initUTxO ::
  forall era.
  ( AllegraEraScript era
  , AlonzoEraTxOut era
  ) =>
  UTxO era
initUTxO =
  UTxO $
    Map.fromList $
      [ (mkGenesisTxIn 1, alwaysSucceedsOutput)
      , (mkGenesisTxIn 2, alwaysFailsOutput)
      ]
        ++ map (\i -> (mkGenesisTxIn i, someOutput)) [3 .. 8]
        ++ map (\i -> (mkGenesisTxIn i, collateralOutput)) [11 .. 18]
        ++ [ (mkGenesisTxIn 100, timelockOut)
           , (mkGenesisTxIn 101, unspendableOut)
           , (mkGenesisTxIn 102, alwaysSucceedsOutputV1)
           , (mkGenesisTxIn 103, nonScriptOutWithDatum)
           ]
  where
    alwaysSucceedsOutput =
      mkBasicTxOut (someScriptAddr @era $ fromNativeScript (RequireAllOf mempty)) (inject $ Coin 5000)
        & dataHashTxOutL .~ SJust (hashData $ datumExample1 @era)
    alwaysFailsOutput =
      mkBasicTxOut (someScriptAddr @era $ fromNativeScript (RequireAnyOf mempty)) (inject $ Coin 3000)
        & dataHashTxOutL .~ SJust (hashData $ datumExample2 @era)
    someOutput = mkBasicTxOut someAddr (inject $ Coin 1000)
    collateralOutput = mkBasicTxOut someAddr (inject $ Coin 5)
    timelockOut = mkBasicTxOut timelockAddr (inject $ Coin 1)
    timelockAddr = mkAddr tlh $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)
      where
        tlh = hashScript @era $ tls 0
        tls s =
          fromNativeScript @era $
            RequireAllOf
              [ RequireSignature $ theKeyHash 1
              , RequireTimeStart (100 + s)
              ]
    -- This output is unspendable since it is locked by a plutus script, but has no datum hash.
    unspendableOut =
      mkBasicTxOut (someScriptAddr @era $ fromNativeScript (RequireAllOf mempty)) (inject $ Coin 5000)
    alwaysSucceedsOutputV1 =
      unspendableOut & dataHashTxOutL .~ SJust (hashData (datumExample1 @era))
    nonScriptOutWithDatum =
      mkBasicTxOut someAddr (inject $ Coin 1221)
        & dataHashTxOutL .~ SJust (hashData (datumExample1 @era))

datumExample1 :: Era era => Data era
datumExample1 = Data (PV1.I 123)

datumExample2 :: Era era => Data era
datumExample2 = Data (PV1.I 0)

-- ======================================================================
-- ========================= Shared helper functions  ===================
-- ======================================================================

mkGenesisTxIn :: HasCallStack => Integer -> TxIn
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

mkTxDats :: Era era => Data era -> TxDats era
mkTxDats d = TxDats $ Map.singleton (hashData d) d

mkSingleRedeemer ::
  forall era. AlonzoEraScript era => PlutusPurpose AsIx era -> Data era -> Redeemers era
mkSingleRedeemer tag datum =
  Redeemers @era $ Map.singleton tag (datum, ExUnits 5000 5000)

-- This implements a special rule to test that for ValidationTagMismatch. Rather than comparing the insides of
-- ValidationTagMismatch (which are complicated and depend on Plutus) we just note that both the computed
-- and expected are ValidationTagMismatch. Of course the 'path' to ValidationTagMismatch differs by Era.
-- so we need to case over the Era proof, to get the path correctly.
testBBODY ::
  forall era.
  ( HasCallStack
  , Eq (State (EraRule "LEDGERS" era))
  , ToExpr (PredicateFailure (EraRule "BBODY" era))
  , ToExpr (State (EraRule "LEDGERS" era))
  , ShelleyEraImp era
  ) =>
  ShelleyBbodyState era ->
  Block BHeaderView era ->
  Either (NonEmpty (PredicateFailure (EraRule "BBODY" era))) (ShelleyBbodyState era) ->
  PParams era ->
  Assertion
testBBODY initialSt block expected pparams =
  let env = BbodyEnv pparams def
   in runSTS @"BBODY" @era (TRC (env, initialSt, block)) (genericCont "" expected)

-- | Use an equality test on the expected and computed [PredicateFailure]
testUTXOW ::
  forall era.
  ( Reflect era
  , HasCallStack
  , ShelleyEraImp era
  ) =>
  UTxO era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion
testUTXOW utxo p tx = testUTXOWwith (genericCont (show (utxo, tx))) utxo p tx

-- | Use a subset test on the expected and computed [PredicateFailure]
testUTXOWsubset ::
  forall era.
  ( Reflect era
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , STS (EraRule "UTXOW" era)
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , ToExpr (TxOut era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  ) =>
  UTxO era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion
testUTXOWsubset = testUTXOWwith subsetCont

-- | Use a test where any two (ValidationTagMismatch x y) failures match regardless of 'x' and 'y'
testUTXOspecialCase ::
  forall era.
  ( Reflect era
  , HasCallStack
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , STS (EraRule "UTXOW" era)
  ) =>
  UTxO era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion
testUTXOspecialCase utxo pparam tx expected =
  let env = UtxoEnv (SlotNo 0) pparam def
      state = smartUTxOState pparam utxo (Coin 0) (Coin 0) def mempty
   in runSTS @"UTXOW" @era (TRC (env, state, tx)) (specialCont @era expected)

-- | This type is what you get when you use runSTS in the UTXOW rule. It is also
--   the type one uses for expected answers, to compare the 'computed' against 'expected'
type Result era =
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era))

testUTXOWwith ::
  forall era.
  ( AlonzoEraImp era
  ) =>
  (Result era -> Result era -> Assertion) ->
  UTxO era ->
  PParams era ->
  Tx era ->
  Result era ->
  Assertion
testUTXOWwith cont utxo pparams tx expected =
  let env = UtxoEnv (SlotNo 0) pparams def
      state = smartUTxOState pparams utxo (Coin 0) (Coin 0) def mempty
   in runSTS @"UTXOW" @era (TRC (env, state, tx)) (cont expected)

runLEDGER ::
  forall era.
  ( BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , STS (EraRule "LEDGER" era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Tx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  LedgerState era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (State (EraRule "LEDGER" era))
runLEDGER state pparams tx =
  let env = LedgerEnv (SlotNo 0) Nothing minBound pparams def
   in runSTS' @"LEDGER" @era (TRC (env, state, tx))

-- | A small example of what a continuation for 'runSTS' might look like
genericCont ::
  ( Foldable t
  , Eq (t x)
  , Eq y
  , ToExpr x
  , ToExpr y
  , HasCallStack
  ) =>
  String ->
  Either (t x) y ->
  Either (t x) y ->
  Assertion
genericCont cause expected computed =
  case (computed, expected) of
    (Left c, Left e)
      | c /= e -> assertFailure $ causedBy ++ expectedToFail e ++ failedWith c
    (Right c, Right e)
      | c /= e -> assertFailure $ causedBy ++ expectedToPass e ++ passedWith c
    (Left x, Right y) ->
      assertFailure $ causedBy ++ expectedToPass y ++ failedWith x
    (Right x, Left y) ->
      assertFailure $ causedBy ++ expectedToFail y ++ passedWith x
    _ -> pure ()
  where
    causedBy
      | null cause = ""
      | otherwise = "Caused by:\n" ++ cause ++ "\n"
    expectedToPass y = "Expected to pass with:\n" ++ show (toExpr y) ++ "\n"
    expectedToFail x = "Expected to fail with:\n" ++ show (toExpr $ toList x) ++ "\n"
    failedWith x = "But failed with:\n" ++ show (toExpr $ toList x)
    passedWith y = "But passed with:\n" ++ show (toExpr y)

subsetCont ::
  ( Foldable t
  , Eq (t x)
  , Eq x
  , Eq y
  , ToExpr x
  , ToExpr y
  , Show (t x)
  , Show y
  ) =>
  Either (t x) y ->
  Either (t x) y ->
  Assertion
subsetCont expected computed =
  let
    isSubset small big = all (`elem` big) small
   in
    case (computed, expected) of
      (Left c, Left e) ->
        -- It is OK if the expected is a subset of what's computed
        if isSubset e c then e @?= e else c @?= e
      (Right c, Right e) -> c @?= e
      (Left x, Right y) ->
        error $
          "expected to pass with "
            ++ show (toExpr y)
            ++ "\n\nBut failed with\n\n"
            ++ show (toExpr $ toList x)
      (Right y, Left x) ->
        error $
          "expected to fail with "
            ++ show (toExpr $ toList x)
            ++ "\n\nBut passed with\n\n"
            ++ show (toExpr y)

findMismatch ::
  Proof era ->
  PredicateFailure (EraRule "UTXOW" era) ->
  Maybe (PredicateFailure (EraRule "UTXOS" era))
findMismatch Alonzo (ShelleyInAlonzoUtxowPredFailure (Shelley.UtxoFailure (UtxosFailure x@(ValidationTagMismatch _ _)))) = Just $ injectFailure x
findMismatch Babbage (Babbage.UtxoFailure (AlonzoInBabbageUtxoPredFailure (UtxosFailure x@(ValidationTagMismatch _ _)))) = Just $ injectFailure x
findMismatch
  Conway
  ( Conway.UtxoFailure
      (Conway.UtxosFailure x@(Conway.ValidationTagMismatch _ _))
    ) = Just $ injectFailure x
findMismatch _ _ = Nothing

specialCont ::
  forall era a.
  ( Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq a
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show a
  , Reflect era
  , HasCallStack
  ) =>
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) a ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) a ->
  Assertion
specialCont expected computed =
  case (computed, expected) of
    (Left (x :| []), Left (y :| [])) ->
      case (findMismatch (reify @era) x, findMismatch (reify @era) y) of
        (Just _, Just _) -> y @?= y
        (_, _) -> error "Not both ValidationTagMismatch case 1"
    (Left _, Left _) -> error "Not both ValidationTagMismatch case 2"
    (Right x, Right y) -> x @?= y
    (Left _, Right _) -> error "expected to pass, but failed."
    (Right _, Left _) -> error "expected to fail, but passed."
