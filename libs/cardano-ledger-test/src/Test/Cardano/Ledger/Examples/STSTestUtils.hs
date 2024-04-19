{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.STSTestUtils (
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
  testBBODY,
  runLEDGER,
  testUTXOW,
  testUTXOWsubset,
  testUTXOspecialCase,
  trustMeP,
  alwaysFailsHash,
  alwaysSucceedsHash,
  timelockScript,
  timelockHash,
  timelockStakeCred,
)
where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBBODY,
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoTx (..),
  IsValid (..),
 )
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.Rules as Babbage (BabbageUtxowPredFailure (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  mkTxIxPartial,
 )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
  StakeReference (..),
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyRole (..), hashKey)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.API (
  Block (..),
  LedgerEnv (..),
  LedgerState (..),
  UTxO (..),
 )
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyState,
  UtxoEnv (..),
 )
import Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (Default (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import GHC.Natural (Natural)
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Fields (
  TxOutField (..),
 )
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..), after, matchkey)
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
 )
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

-- import Test.Cardano.Ledger.Generic.PrettyCore(pcTx)
import Test.Cardano.Ledger.Constrained.Preds.Tx (pcTxWithUTxO)

-- =================================================================
-- =========================  Shared data  =========================
--   Data with specific semantics ("constants")
-- =================================================================

alwaysFailsHash :: forall era. Scriptic era => Natural -> Proof era -> ScriptHash (EraCrypto era)
alwaysFailsHash n pf = hashScript @era $ never n pf

alwaysSucceedsHash ::
  forall era.
  Scriptic era =>
  Natural ->
  Proof era ->
  ScriptHash (EraCrypto era)
alwaysSucceedsHash n pf = hashScript @era $ always n pf

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(EraCrypto era) (RawSeed 1 1 1 1 1)

someAddr :: forall era. Era era => Proof era -> Addr (EraCrypto era)
someAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

-- Create an address with a given payment script.
someScriptAddr :: forall era. Scriptic era => Script era -> Addr (EraCrypto era)
someScriptAddr s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

timelockScript :: PostShelley era => Int -> Proof era -> Script era
timelockScript s = fromNativeScript . allOf [matchkey 1, after (100 + s)]

timelockHash ::
  forall era.
  PostShelley era =>
  Int ->
  Proof era ->
  ScriptHash (EraCrypto era)
timelockHash n pf = hashScript @era $ timelockScript n pf

timelockStakeCred :: PostShelley era => Proof era -> StakeCredential (EraCrypto era)
timelockStakeCred pf = ScriptHashObj (timelockHash 2 pf)

-- ======================================================================
-- ========================= Initial Utxo ===============================
-- ======================================================================

initUTxO :: forall era. (EraTxOut era, PostShelley era) => Proof era -> UTxO era
initUTxO pf =
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
      newTxOut
        pf
        [ Address (someScriptAddr (always 3 pf))
        , Amount (inject $ Coin 5000)
        , DHash' [hashData $ datumExample1 @era]
        ]
    alwaysFailsOutput =
      newTxOut
        pf
        [ Address (someScriptAddr (never 0 pf))
        , Amount (inject $ Coin 3000)
        , DHash' [hashData $ datumExample2 @era]
        ]
    someOutput = newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 1000)]
    collateralOutput = newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)]
    timelockOut = newTxOut pf [Address $ timelockAddr, Amount (inject $ Coin 1)]
    timelockAddr = Addr Testnet pCred sCred
      where
        (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 2)
        pCred = ScriptHashObj tlh
        sCred = StakeRefBase . KeyHashObj . hashKey $ svk
        tlh = hashScript @era $ tls 0
        tls s = fromNativeScript $ allOf [matchkey 1, after (100 + s)] pf
    -- This output is unspendable since it is locked by a plutus script, but has no datum hash.
    unspendableOut =
      newTxOut
        pf
        [ Address (someScriptAddr (always 3 pf))
        , Amount (inject $ Coin 5000)
        ]
    alwaysSucceedsOutputV1 =
      newTxOut
        pf
        [ Address (someScriptAddr (always 3 pf))
        , Amount (inject $ Coin 5000)
        , DHash' [hashData $ datumExample1 @era]
        ]
    nonScriptOutWithDatum =
      newTxOut
        pf
        [ Address (someAddr pf)
        , Amount (inject $ Coin 1221)
        , DHash' [hashData $ datumExample1 @era]
        ]

datumExample1 :: Era era => Data era
datumExample1 = Data (PV1.I 123)

datumExample2 :: Era era => Data era
datumExample2 = Data (PV1.I 0)

-- ======================================================================
-- ========================= Shared helper functions  ===================
-- ======================================================================

mkGenesisTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => Integer -> TxIn c
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

mkTxDats :: Era era => Data era -> TxDats era
mkTxDats d = TxDats $ Map.singleton (hashData d) d

trustMeP :: Proof era -> Bool -> Tx era -> Tx era
trustMeP Alonzo iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP Babbage iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP Conway iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP _ _ tx = tx

-- This implements a special rule to test that for ValidationTagMismatch. Rather than comparing the insides of
-- ValidationTagMismatch (which are complicated and depend on Plutus) we just note that both the computed
-- and expected are ValidationTagMismatch. Of course the 'path' to ValidationTagMismatch differs by Era.
-- so we need to case over the Era proof, to get the path correctly.
testBBODY ::
  (Reflect era, HasCallStack) =>
  WitRule "BBODY" era ->
  ShelleyBbodyState era ->
  Block (BHeaderView (EraCrypto era)) era ->
  Either (NonEmpty (PredicateFailure (AlonzoBBODY era))) (ShelleyBbodyState era) ->
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
  testUTXOWwith wit (genericCont (show (pcTxWithUTxO Alonzo utxo tx))) utxo p tx
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
  ( GoodCrypto (EraCrypto era)
  , EraTx era
  , EraGov era
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
  ( GoodCrypto (EraCrypto era)
  , EraTx era
  , EraGov era
  ) =>
  WitRule "LEDGER" era ->
  LedgerState era ->
  PParams era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (State (EraRule "LEDGER" era))
runLEDGER wit@(LEDGER proof) state pparams tx =
  let env = LedgerEnv (SlotNo 0) minBound pparams def
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
  , PrettyA x
  , PrettyA y
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
    expectedToPass y = "Expected to pass with:\n" ++ show (prettyA y) ++ "\n"
    expectedToFail x = "Expected to fail with:\n" ++ show (prettyA $ toList x) ++ "\n"
    failedWith x = "But failed with:\n" ++ show (prettyA $ toList x)
    passedWith y = "But passed with:\n" ++ show (prettyA y)

subsetCont ::
  ( Foldable t
  , Eq (t x)
  , Eq x
  , Eq y
  , PrettyA x
  , PrettyA y
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
          ++ show (prettyA y)
          ++ "\n\nBut failed with\n\n"
          ++ show (prettyA $ toList x)
    (Right y, Left x) ->
      error $
        "expected to fail with "
          ++ show (prettyA $ toList x)
          ++ "\n\nBut passed with\n\n"
          ++ show (prettyA y)

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
