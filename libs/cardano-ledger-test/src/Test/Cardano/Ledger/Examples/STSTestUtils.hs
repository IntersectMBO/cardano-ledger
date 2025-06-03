{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript, pattern RequireTimeStart)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.Rules as Babbage (BabbageUtxowPredFailure (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraTxOut (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.API (
  Block (..),
  LedgerEnv (..),
  LedgerState (..),
 )
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyState,
  UtxoEnv (..),
 )
import Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxowPredFailure (..))
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
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default (Default (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC.Stack
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common (ToExpr, toExpr)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag, mkRedeemersFromTags)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

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

mkSingleRedeemer :: Proof era -> PlutusPurposeTag -> Data era -> Redeemers era
mkSingleRedeemer proof tag datum =
  mkRedeemersFromTags proof [((tag, 0), (datum, ExUnits 5000 5000))]

-- This implements a special rule to test that for ValidationTagMismatch. Rather than comparing the insides of
-- ValidationTagMismatch (which are complicated and depend on Plutus) we just note that both the computed
-- and expected are ValidationTagMismatch. Of course the 'path' to ValidationTagMismatch differs by Era.
-- so we need to case over the Era proof, to get the path correctly.
testBBODY ::
  (HasCallStack, EraTest era, Reflect era) =>
  WitRule "BBODY" era ->
  ShelleyBbodyState era ->
  Block BHeaderView era ->
  Either (NonEmpty (PredicateFailure (EraRule "BBODY" era))) (ShelleyBbodyState era) ->
  PParams era ->
  Assertion
testBBODY wit@(BBODY proof) initialSt block expected pparams =
  let env = BbodyEnv pparams def
   in case proof of
        Alonzo -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        Babbage -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        Conway -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        other -> error ("We cannot testBBODY in era " ++ show other)

testUTXOW ::
  forall era.
  ( Reflect era
  , HasCallStack
  ) =>
  WitRule "UTXOW" era ->
  UTxO era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion

-- | Use an equality test on the expected and computed [PredicateFailure]
testUTXOW wit@(UTXOW Alonzo) utxo p tx =
  testUTXOWwith wit (genericCont (show (utxo, tx))) utxo p tx
testUTXOW wit@(UTXOW Babbage) utxo p tx = testUTXOWwith wit (genericCont (show tx)) utxo p tx
testUTXOW wit@(UTXOW Conway) utxo p tx = testUTXOWwith wit (genericCont (show tx)) utxo p tx
testUTXOW (UTXOW other) _ _ _ = error ("Cannot use testUTXOW in era " ++ show other)

testUTXOWsubset
  , testUTXOspecialCase ::
    forall era.
    ( Reflect era
    , HasCallStack
    ) =>
    WitRule "UTXOW" era ->
    UTxO era ->
    PParams era ->
    Tx era ->
    Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
    Assertion

-- | Use a subset test on the expected and computed [PredicateFailure]
testUTXOWsubset wit@(UTXOW Alonzo) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset wit@(UTXOW Babbage) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset wit@(UTXOW Conway) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset (UTXOW other) _ = error ("Cannot use testUTXOW in era " ++ show other)

-- | Use a test where any two (ValidationTagMismatch x y) failures match regardless of 'x' and 'y'
testUTXOspecialCase wit@(UTXOW proof) utxo pparam tx expected =
  let env = UtxoEnv (SlotNo 0) pparam def
      state = smartUTxOState pparam utxo (Coin 0) (Coin 0) def mempty
   in case proof of
        Alonzo -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        Babbage -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        Conway -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        other -> error ("Cannot use specialCase in era " ++ show other)

-- | This type is what you get when you use runSTS in the UTXOW rule. It is also
--   the type one uses for expected answers, to compare the 'computed' against 'expected'
type Result era =
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era))

testUTXOWwith ::
  forall era.
  ( EraTx era
  , EraGov era
  , EraStake era
  , EraCertState era
  ) =>
  WitRule "UTXOW" era ->
  (Result era -> Result era -> Assertion) ->
  UTxO era ->
  PParams era ->
  Tx era ->
  Result era ->
  Assertion
testUTXOWwith wit@(UTXOW proof) cont utxo pparams tx expected =
  let env = UtxoEnv (SlotNo 0) pparams def
      state = smartUTxOState pparams utxo (Coin 0) (Coin 0) def mempty
   in case proof of
        Conway -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Babbage -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Alonzo -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Mary -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Allegra -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Shelley -> runSTS wit (TRC (env, state, tx)) (cont expected)

runLEDGER ::
  forall era.
  ( EraTx era
  , EraGov era
  ) =>
  WitRule "LEDGER" era ->
  LedgerState era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (State (EraRule "LEDGER" era))
runLEDGER wit@(LEDGER proof) state pparams tx =
  let env = LedgerEnv (SlotNo 0) Nothing minBound pparams def
   in case proof of
        Conway -> runSTS' wit (TRC (env, state, tx))
        Babbage -> runSTS' wit (TRC (env, state, tx))
        Alonzo -> runSTS' wit (TRC (env, state, tx))
        Mary -> runSTS' wit (TRC (env, state, tx))
        Allegra -> runSTS' wit (TRC (env, state, tx))
        Shelley -> runSTS' wit (TRC (env, state, tx))

-- ======================================================================
-- =========================  Internal helper functions  ================
-- ======================================================================

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

specialCont ::
  ( Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq a
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show a
  , HasCallStack
  ) =>
  Proof era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) a ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) a ->
  Assertion
specialCont proof expected computed =
  case (computed, expected) of
    (Left (x :| []), Left (y :| [])) ->
      case (findMismatch proof x, findMismatch proof y) of
        (Just _, Just _) -> y @?= y
        (_, _) -> error "Not both ValidationTagMismatch case 1"
    (Left _, Left _) -> error "Not both ValidationTagMismatch case 2"
    (Right x, Right y) -> x @?= y
    (Left _, Right _) -> error "expected to pass, but failed."
    (Right _, Left _) -> error "expected to fail, but passed."

-- ========================================
-- This implements a special rule to test that for ValidationTagMismatch. Rather than comparing the insides of
-- ValidationTagMismatch (which are complicated and depend on Plutus) we just note that both the computed
-- and expected are ValidationTagMismatch. Of course the 'path' to ValidationTagMismatch differs by Era.
-- so we need to case over the Era proof, to get the path correctly.
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

isSubset :: (Foldable t, Eq a) => t a -> t a -> Bool
isSubset small big = all (`elem` big) small
