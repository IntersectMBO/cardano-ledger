{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Ledger.Examples.AlonzoAPI (tests, defaultPParams) where

import Cardano.Ledger.Alonzo.Tx (alonzoMinFeeTx, hashData)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), TxDats (..), unTxDatsL)
import Cardano.Ledger.BaseTypes (ProtVer (..), inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  AlonzoEraPParams,
  AlonzoEraTxBody (..),
  AsIx (..),
  EraPParams (..),
  EraTxOut (..),
  PParams,
  emptyPParams,
  eraProtVerLow,
  ppCollateralPercentageL,
  ppCostModelsL,
  ppMaxBlockExUnitsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
  ppMinFeeAL,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Core (EraScript (..), EraTx (..), EraTxBody (..), EraTxWits (..), hashScript)
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf)
import Cardano.Ledger.Tools (estimateMinFeeTx)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  mkGenesisTxIn,
  mkSingleRedeemer,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.ApplyTx (defaultPPs)
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.Proof (AlonzoEra, Reflect (..))
import Test.Cardano.Ledger.Generic.TxGen ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

tests :: Spec
tests =
  describe "Alonzo API" $ do
    it "estimateMinFee" $ do
      testEstimateMinFee @AlonzoEra

testEstimateMinFee ::
  forall era.
  ( Reflect era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Expectation
testEstimateMinFee =
  estimateMinFeeTx @era
    pparams
    validatingTxNoWits
    1
    0
    0
    `shouldBe` alonzoMinFeeTx pparams validatingTx
  where
    pparams =
      defaultPPs emptyPParams
        & ppMinFeeAL .~ Coin 1
    dat = Data (PV1.I 123)
    dataMap = Map.singleton (hashData dat) dat
    script = fromNativeScript $ RequireAllOf mempty
    scriptMap = Map.singleton (hashScript script) script
    validatingTxNoWits =
      mkBasicTx validatingBody
        & witsTxL . scriptTxWitsL .~ [(hashScript script, script)]
        & witsTxL . datsTxWitsL .~ TxDats [(hashData dat, dat)]
        & witsTxL . rdmrsTxWitsL .~ redeemers
    validatingTx =
      mkBasicTx validatingBody
        & witsTxL . addrTxWitsL
          .~ Set.singleton (mkWitnessVKey (hashAnnotated validatingBody) someKeys)
        & witsTxL . scriptTxWitsL .~ scriptMap
        & witsTxL . datsTxWitsL . unTxDatsL .~ dataMap
        & witsTxL . rdmrsTxWitsL .~ redeemers
    validatingBody =
      mkBasicTxBody
        & inputsTxBodyL .~ [mkGenesisTxIn 1]
        & collateralInputsTxBodyL .~ [mkGenesisTxIn 11]
        & outputsTxBodyL .~ [mkBasicTxOut @era someAddr (inject $ Coin 4995)]
        & feeTxBodyL .~ Coin 316
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash @era
            defaultPParams
            [PlutusV1]
            redeemers
            (mkTxDats (Data (PV1.I 123)))
    redeemers = mkSingleRedeemer (SpendingPurpose $ AsIx 0) (Data (PV1.I 42))

defaultPParams :: forall era. AlonzoEraPParams era => PParams era
defaultPParams =
  emptyPParams @era
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1]
    & ppMaxValSizeL .~ 1_000_000_000
    & ppMaxTxExUnitsL .~ ExUnits 1_000_000 1_000_000
    & ppMaxBlockExUnitsL .~ ExUnits 1_000_000 1_000_000
    & ppProtocolVersionL .~ ProtVer (eraProtVerLow @era) 0
    & ppCollateralPercentageL .~ 100
