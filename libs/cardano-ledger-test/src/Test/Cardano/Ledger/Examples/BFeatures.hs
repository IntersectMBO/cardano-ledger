{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.BFeatures where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..))
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (..))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraRule)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
  )
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (ProtVer (..), UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( freeCostModelV1,
    freeCostModelV2,
    testUTXOW,
    trustMeP,
  )
import Test.Cardano.Ledger.Generic.Fields
  ( PParamsField (..),
    TxBodyField (..),
    TxField (..),
    WitnessesField (..),
  )
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls . CostModels $ Map.fromList [(PlutusV1, freeCostModelV1), (PlutusV2, freeCostModelV2)],
    MaxValSize 1000000000,
    MaxTxExUnits $ ExUnits 1000000 1000000,
    MaxBlockExUnits $ ExUnits 1000000 1000000,
    ProtocolVersion $ ProtVer 7 0,
    CollateralPercentage 1,
    AdaPerUTxOByte (Coin 5)
  ]

pp :: Proof era -> Core.PParams era
pp pf = newPParams pf defaultPPs

class BabbageBased era failure where
  fromUtxoB :: BabbageUtxoPred era -> failure
  fromUtxowB :: BabbageUtxowPred era -> failure

instance BabbageBased (BabbageEra c) (BabbageUtxowPred (BabbageEra c)) where
  fromUtxoB = UtxoFailure
  fromUtxowB = id

type InOut era = (TxIn (Crypto era), Core.TxOut era)

data TestCaseData era = TestCaseData
  { input :: InOut era,
    collateral :: [InOut era],
    refInputs :: [InOut era],
    ttxOut :: Core.TxOut era,
    txBodyFields :: [TxBodyField era],
    keysForAddrWits :: [KeyPair 'Payment (Crypto era)],
    otherWitsFields :: [WitnessesField era]
  }

testExpectSuccessValid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    Scriptic era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessValid
  pf
  (TestCaseData input' collateral' refInputs' txOut' txBodyFields' keysForAddrWits' otherWitsFields') =
    let txBody' =
          newTxBody
            pf
            ( [ Inputs' [fst input'],
                Collateral' $ fst <$> collateral',
                RefInputs' $ fst <$> refInputs',
                Outputs' [txOut']
              ]
                ++ txBodyFields'
            )
        addrWits = makeWitnessVKey (hashAnnotated txBody') <$> keysForAddrWits'
        tx' =
          newTx
            pf
            ( Body txBody' :
              [ WitnessesI
                  (AddrWits' addrWits : otherWitsFields')
              ]
            )
        newTxIn = TxIn (txid txBody') minBound
        utxo = (UTxO . Map.fromList) $ [input'] ++ collateral' ++ refInputs'
        expectedUtxo = UTxO $ Map.insert newTxIn txOut' (Map.fromList (collateral' ++ refInputs'))
        expectedState = smartUTxOState expectedUtxo (Coin 0) (Coin 5) def
     in testUTXOW (UTXOW pf) utxo (pp pf) (trustMeP pf True tx') (Right expectedState)

genericBFeatures ::
  forall era.
  Proof era ->
  TestTree
genericBFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup "valid transactions" [],
      testGroup "invalid transactions" []
    ]

bFeatures :: TestTree
bFeatures =
  testGroup
    "B Features"
    [genericBFeatures (Babbage Mock)]
