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

module Test.Cardano.Ledger.Examples.AlonzoAPI (tests) where

import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModels (..),
    ExUnits (..),
  )
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (getMinFeeTx)
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (evaluateTransactionFee)
import Cardano.Ledger.UTxO (makeWitnessVKey)
import Cardano.Ledger.Val (Val (inject))
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Examples.STSTestUtils
  ( freeCostModelV1,
    mkGenesisTxIn,
    mkTxDats,
    someAddr,
    someKeys,
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
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Alonzo API" [testCase "evaluateTransactionFee" testEvaluateTransactionFee]

type A = AlonzoEra C_Crypto

testEvaluateTransactionFee :: Assertion
testEvaluateTransactionFee =
  evaluateTransactionFee @A
    pparams
    validatingTxNoWits
    1
    @?= getMinFeeTx pparams validatingTx
  where
    pf = Alonzo Mock
    pparams = newPParams pf $ defaultPPs ++ [MinfeeA 1]
    validatingTxNoWits =
      newTx
        pf
        [ Body validatingBody,
          WitnessesI
            [ ScriptWits' [always 3 pf],
              DataWits' [Data (PV1.I 123)],
              RdmrWits redeemers
            ]
        ]
    validatingTx =
      newTx
        pf
        [ Body validatingBody,
          WitnessesI
            [ AddrWits' [makeWitnessVKey (hashAnnotated validatingBody) (someKeys pf)],
              ScriptWits' [always 3 pf],
              DataWits' [Data (PV1.I 123)],
              RdmrWits redeemers
            ]
        ]
    validatingBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1],
          Collateral' [mkGenesisTxIn 11],
          Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]],
          Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (newPParams pf defaultPPs) [PlutusV1] redeemers (mkTxDats (Data (PV1.I 123))))
        ]
    redeemers =
      Redeemers $
        Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls . CostModels $ Map.singleton PlutusV1 freeCostModelV1,
    MaxValSize 1000000000,
    MaxTxExUnits $ ExUnits 1000000 1000000,
    MaxBlockExUnits $ ExUnits 1000000 1000000,
    ProtocolVersion $ ProtVer (natVersion @5) 0,
    CollateralPercentage 100
  ]
