{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW (tests, mkSingleRedeemer) where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  ProtVer (..),
  TxIx,
  mkTxIxPartial,
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  smartUTxOState,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  alwaysSucceedsHash,
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
  testUTXOW,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxField (..),
  TxOutField (..),
  WitnessesField (..),
 )
import Test.Cardano.Ledger.Generic.GenState (
  PlutusPurposeTag (..),
  mkRedeemers,
  mkRedeemersFromTags,
 )
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

tests :: TestTree
tests =
  testGroup
    "Generic Tests for valid transactions, testing Alonzo UTXOW PredicateFailures, in postAlonzo eras."
    [ alonzoUTXOWTests Alonzo
    , alonzoUTXOWTests Babbage
    -- alonzoUTXOWTests Conway TODO
    ]

alonzoUTXOWTests ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , Reflect era
  , PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto,
  ) =>
  Proof era ->
  TestTree
alonzoUTXOWTests pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "acceptable supplimentary datum" $
            testU
              pf
              (trustMeP pf True $ validatingSupplimentaryDatumTx pf)
              (Right . validatingSupplimentaryDatumState $ pf)
        , testCase "multiple identical certificates" $
            testU
              pf
              (trustMeP pf True $ validatingMultipleEqualCertsTx pf)
              (Right . validatingMultipleEqualCertsState $ pf)
        , testCase "non-script output with datum" $
            testU
              pf
              (trustMeP pf True $ validatingNonScriptOutWithDatumTx pf)
              (Right . validatingNonScriptOutWithDatumState $ pf)
        ]
    ]

-- =========================================================================
-- ============================== DATA ========================================

-- ====================================================================================
--  Example 10: A transaction with an acceptable supplimentary datum
-- ====================================================================================

validatingSupplimentaryDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  ) =>
  Proof era ->
  Tx era
validatingSupplimentaryDatumTx pf =
  newTx
    pf
    [ Body (validatingSupplimentaryDatumBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingSupplimentaryDatumBody pf)) (someKeys pf)]
        , DataWits' [Data (PV1.I 123)]
        ]
    ]

validatingSupplimentaryDatumBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingSupplimentaryDatumBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Outputs' [validatingSupplimentaryDatumTxOut pf]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [] (mkRedeemers pf []) (mkTxDats (Data (PV1.I 123))))
    ]

validatingSupplimentaryDatum :: Era era => Data era
validatingSupplimentaryDatum = Data (PV1.I 123)

validatingSupplimentaryDatumTxOut ::
  forall era. (EraTxBody era, Scriptic era) => Proof era -> TxOut era
validatingSupplimentaryDatumTxOut pf =
  newTxOut
    pf
    [ Address (someScriptAddr (always 3 pf))
    , Amount (inject $ Coin 995)
    , DHash' [hashData $ validatingSupplimentaryDatum @era]
    ]

validatingSupplimentaryDatumState ::
  (EraTxBody era, EraStake era, PostShelley era, EraGov era) =>
  Proof era ->
  UTxOState era
validatingSupplimentaryDatumState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo =
      expectedUTxO'
        pf
        (ExpectSuccess (validatingSupplimentaryDatumBody pf) (validatingSupplimentaryDatumTxOut pf))
        3

-- ====================================================================================
--  Example 11: A transaction with multiple identical certificates
-- ====================================================================================

validatingMultipleEqualCertsTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
validatingMultipleEqualCertsTx pf =
  newTx
    pf
    [ Body (validatingMultipleEqualCertsBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingMultipleEqualCertsBody pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingMultipleEqualCertsRedeemers pf
        ]
    ]

validatingMultipleEqualCertsBody ::
  (EraTxBody era, Scriptic era, ShelleyEraTxCert era) => Proof era -> TxBody era
validatingMultipleEqualCertsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingMultipleEqualCertsOut pf]
    , Certs'
        [ UnRegTxCert (scriptStakeCredSuceed pf)
        , UnRegTxCert (scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
        ]
    , Txfee (Coin 5)
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (validatingMultipleEqualCertsRedeemers pf)
            mempty
        )
    ]

validatingMultipleEqualCertsRedeemers :: Era era => Proof era -> Redeemers era
validatingMultipleEqualCertsRedeemers pf = mkSingleRedeemer pf Certifying (Data (PV1.I 42))

validatingMultipleEqualCertsOut :: EraTxOut era => Proof era -> TxOut era
validatingMultipleEqualCertsOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

validatingMultipleEqualCertsState ::
  (EraTxBody era, EraStake era, PostShelley era, EraGov era, ShelleyEraTxCert era) =>
  Proof era ->
  UTxOState era
validatingMultipleEqualCertsState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo =
      expectedUTxO'
        pf
        (ExpectSuccess (validatingMultipleEqualCertsBody pf) (validatingMultipleEqualCertsOut pf))
        3

-- ====================================================================================
--  Example 12: Attaching a datum (hash) to a non-script output.
--
--  Note that a when spending a non-script output with a datum hash, the datum cannot
--  be supplied, because it is considered extraneous,
--  as in the 'notOkSupplimentaryDatumTx' example.
-- ====================================================================================

validatingNonScriptOutWithDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  ) =>
  Proof era ->
  Tx era
validatingNonScriptOutWithDatumTx pf =
  newTx
    pf
    [ Body (validatingNonScriptOutWithDatumTxBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingNonScriptOutWithDatumTxBody pf)) (someKeys pf)]
        ]
    ]

validatingNonScriptOutWithDatumTxBody :: EraTxBody era => Proof era -> TxBody era
validatingNonScriptOutWithDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 103]
    , Outputs' [validatingNonScriptOutWithDatumTxOut pf]
    , Txfee (Coin 5)
    ]

validatingNonScriptOutWithDatumTxOut :: EraTxOut era => Proof era -> TxOut era
validatingNonScriptOutWithDatumTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1216)]

validatingNonScriptOutWithDatumState ::
  ( PostShelley era
  , EraTxBody era
  , EraGov era
  , EraStake era
  ) =>
  Proof era ->
  UTxOState era
validatingNonScriptOutWithDatumState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo =
      expectedUTxO'
        pf
        (ExpectSuccess (validatingNonScriptOutWithDatumTxBody pf) (validatingNonScriptOutWithDatumTxOut pf))
        103

-- ============================== HELPER FUNCTIONS ===============================

--  This is a helper type for the expectedUTxO function.
--  ExpectSuccess indicates that we created a valid transaction
--  where the IsValid flag is true.
data Expect era
  = ExpectSuccess (TxBody era) (TxOut era)
  | ExpectSuccessInvalid
  | ExpectFailure

-- | In each of our main eight examples, the UTxO map obtained
-- by applying the transaction is straightforward. This function
-- captures the logic.
--
-- Each example transaction (given a number i) will use
-- (TxIn genesisId (10+i), someOutput) for its' single input,
-- and (TxIn genesisId i, collateralOutput) for its' single collateral output.
--
-- If we expect the transaction script to validate, then
-- the UTxO for (TxIn genesisId i) will be consumed and a UTxO will be created.
-- Otherwise, the UTxO for (TxIn genesisId (10+i)) will be consumed.
expectedUTxO ::
  forall era.
  (HasCallStack, EraTxBody era) =>
  UTxO era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO initUtxo ex idx = UTxO utxo
  where
    utxo = case ex of
      ExpectSuccess txb newOut ->
        Map.insert (TxIn (txIdTxBody txb) minBound) newOut (filteredUTxO (mkTxIxPartial idx))
      ExpectSuccessInvalid -> filteredUTxO (mkTxIxPartial idx)
      ExpectFailure -> filteredUTxO (mkTxIxPartial (10 + idx))
    filteredUTxO :: TxIx -> Map.Map TxIn (TxOut era)
    filteredUTxO x = Map.filterWithKey (\(TxIn _ i) _ -> i /= x) $ unUTxO initUtxo

expectedUTxO' ::
  (HasCallStack, EraTxBody era, PostShelley era) =>
  Proof era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO' pf = expectedUTxO (initUTxO pf)

testU ::
  forall era.
  ( PostShelley era
  , Reflect era
  , HasCallStack
  ) =>
  Proof era ->
  Tx era ->
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion
testU pf = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf)

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

-- ============================== PPARAMS ===============================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ zeroTestingCostModels [PlutusV1]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , CollateralPercentage 100
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs

mkSingleRedeemer :: Proof era -> PlutusPurposeTag -> Data era -> Redeemers era
mkSingleRedeemer proof tag datum =
  mkRedeemersFromTags proof [((tag, 0), (datum, ExUnits 5000 5000))]
