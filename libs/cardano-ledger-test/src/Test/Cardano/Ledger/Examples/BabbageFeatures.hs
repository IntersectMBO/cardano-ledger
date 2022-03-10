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
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.BabbageFeatures where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (Data (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    mkTxIxPartial,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraRule)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
  ( Credential (..),
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (..), ValidateScript (hashScript))
import Cardano.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (ProtVer (..), UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default.Class (Default (..))
import qualified Data.Map as Map
import GHC.Stack
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( Expect (..),
    expectedUTxO,
    freeCostModel,
    testUTXOW,
    trustMeP,
  )
import Test.Cardano.Ledger.Generic.Fields
  ( PParamsField (..),
    TxBodyField (..),
    TxField (..),
    TxOutField (..),
    WitnessesField (..),
  )
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- =======================
-- Setup the initial state
-- =======================

scriptAddr :: forall era. (Scriptic era) => Proof era -> Core.Script era -> Addr (Crypto era)
scriptAddr _pf s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (Crypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 1 1 1 1 1)

plainAddr :: forall era. Era era => Proof era -> Addr (Crypto era)
plainAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

somePlainOutput :: Scriptic era => Proof era -> Core.TxOut era
somePlainOutput pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1000)]

mkGenesisTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => Integer -> TxIn crypto
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

collateralOutput :: Scriptic era => Proof era -> Core.TxOut era
collateralOutput pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 5)]

datumExample1 :: Data era
datumExample1 = Data (Plutus.I 123)

inlineDatumOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
inlineDatumOutput pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (alwaysAlt 3 pf)),
      Amount (inject $ Coin 5000),
      Datum (Babbage.Datum . dataToBinaryData $ datumExample1 @era)
    ]

referenceScriptOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceScriptOutput pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 10),
      Datum (Babbage.Datum . dataToBinaryData $ datumExample1 @era),
      RefScript (SJust $ alwaysAlt 3 pf)
    ]

initUTxO :: PostShelley era => Proof era -> UTxO era
initUTxO pf =
  UTxO $
    SplitMap.fromList $
      [ (mkGenesisTxIn 1, inlineDatumOutput pf),
        (mkGenesisTxIn 2, referenceScriptOutput pf)
      ]
        ++ map (\i -> (mkGenesisTxIn i, somePlainOutput pf)) [3 .. 8]
        ++ map (\i -> (mkGenesisTxIn i, collateralOutput pf)) [11 .. 18]

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ Map.fromList [(PlutusV1, freeCostModel), (PlutusV2, freeCostModel)],
    MaxValSize 1000000000,
    MaxTxExUnits $ ExUnits 1000000 1000000,
    MaxBlockExUnits $ ExUnits 1000000 1000000,
    ProtocolVersion $ ProtVer 7 0,
    CollateralPercentage 100
  ]

pp :: Proof era -> Core.PParams era
pp pf = newPParams pf defaultPPs

-- =========================================================================
--  Example 1: Spend a EUTxO with an inline datum
-- =========================================================================

redeemerExample1 :: Data era
redeemerExample1 = Data (Plutus.I 42)

validatingRedeemersEx1 :: Era era => Redeemers era
validatingRedeemersEx1 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample1, ExUnits 5000 5000)

outEx1 :: Scriptic era => Proof era -> Core.TxOut era
outEx1 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]

inlineDatumTxBody :: Scriptic era => Proof era -> Core.TxBody era
inlineDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      RefInputs' [mkGenesisTxIn 2],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

inlineDatumTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
inlineDatumTx pf =
  newTx
    pf
    [ Body (inlineDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (inlineDatumTxBody pf)) (someKeys pf)],
          ScriptWits' [alwaysAlt 3 pf],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx1 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx1 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (inlineDatumTxBody pf) (outEx1 pf)) 1

utxoStEx1 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx1 pf = smartUTxOState (utxoEx1 pf) (Coin 0) (Coin 5) def

-- =========================================================================
--  Example 2: Use a reference script
-- =========================================================================

outEx2 :: Scriptic era => Proof era -> Core.TxOut era
outEx2 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]

referenceScriptTxBody :: Scriptic era => Proof era -> Core.TxBody era
referenceScriptTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      RefInputs' [mkGenesisTxIn 2],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

referenceScriptTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
referenceScriptTx pf =
  newTx
    pf
    [ Body (referenceScriptTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (referenceScriptTxBody pf)) (someKeys pf)],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx2 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx2 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (referenceScriptTxBody pf) (outEx2 pf)) 1

utxoStEx2 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx2 pf = smartUTxOState (utxoEx2 pf) (Coin 0) (Coin 5) def

testU ::
  forall era.
  ( GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  Core.Tx era ->
  Either [(PredicateFailure (Core.EraRule "UTXOW" era))] (State (Core.EraRule "UTXOW" era)) ->
  Assertion
testU pf tx expect = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf) tx expect

genericBabbageFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  TestTree
genericBabbageFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $
            testU
              pf
              (trustMeP pf True $ inlineDatumTx pf)
              (Right . utxoStEx1 $ pf),
          testCase "reference script" $
            testU
              pf
              (trustMeP pf True $ referenceScriptTx pf)
              (Right . utxoStEx2 $ pf)
        ]
    ]

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures (Babbage Mock)
    ]
