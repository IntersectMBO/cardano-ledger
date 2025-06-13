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
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.STSTestUtils (
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  mkSingleRedeemer,
  someAddr,
  someKeys,
  someScriptAddr,
  alwaysFailsHash,
  alwaysSucceedsHash,
  timelockScript,
  timelockHash,
  timelockStakeCred,
  genericCont,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript, pattern RequireTimeStart)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), AsIx, ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), mkTxIxPartial)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraTxOut (..))
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
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
import qualified Data.Map.Strict as Map
import GHC.Stack
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')
import Test.Tasty.HUnit (Assertion, assertFailure)
import Data.Foldable (Foldable(..))

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
