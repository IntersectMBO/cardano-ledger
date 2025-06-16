{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Ledger.Examples.AlonzoAPI (tests) where

import Cardano.Ledger.Alonzo.Tx (alonzoMinFeeTx, hashData)
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), inject, natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraTxWits (..))
import Cardano.Ledger.Core (EraTx (..), EraTxWits (..), hashScript)
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Tools (estimateMinFeeTx)
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  mkGenesisTxIn,
  mkSingleRedeemer,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxOutField (..),
 )
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag (..))
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Alonzo API" [testCase "estimateMinFee" testEstimateMinFee]

testEstimateMinFee :: Assertion
testEstimateMinFee =
  estimateMinFeeTx @AlonzoEra
    pparams
    validatingTxNoWits
    1
    0
    0
    @?= alonzoMinFeeTx pparams validatingTx
  where
    pf = Alonzo
    pparams = newPParams pf $ defaultPPs ++ [MinfeeA (Coin 1)]
    script = always 3 pf
    dat = Data (PV1.I 123)
    validatingTxNoWits =
      mkBasicTx validatingBody
        & witsTxL . scriptTxWitsL .~ [(hashScript script, script)]
        & witsTxL . datsTxWitsL .~ TxDats [(hashData dat, dat)]
        & witsTxL . rdmrsTxWitsL .~ redeemers
    validatingTx =
      validatingTxNoWits
        & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated validatingBody) (someKeys pf)]
        & witsTxL . scriptTxWitsL .~ [(hashScript script, script)]
        & witsTxL . datsTxWitsL .~ [dat]
        & witsTxL . rdmrsTxWitsL .~ redeemers
    validatingBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 316)
        , WppHash
            ( newScriptIntegrityHash
                pf
                (newPParams pf defaultPPs)
                [PlutusV1]
                redeemers
                (mkTxDats (Data (PV1.I 123)))
            )
        ]
    redeemers = mkSingleRedeemer pf Spending (Data (PV1.I 42))

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ zeroTestingCostModels [PlutusV1]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , CollateralPercentage 100
  ]
