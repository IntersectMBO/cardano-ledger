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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoCollectInputs (tests) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext,
  EraPlutusTxInfo,
  LedgerTxInfo (..),
  toPlutusArgs,
  toPlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..), collectPlutusScriptsWithContext)
import Cardano.Ledger.Alonzo.Scripts (
  AsIx (..),
  AsIxItem (..),
  PlutusPurpose,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  Redeemers (..),
  TxDats (..),
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (AlonzoEraScript, AlonzoEraTxBody (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  PlutusWithContext (..),
  hashData,
  hashPlutusScript,
 )
import Cardano.Ledger.State (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysSucceeds)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.AlonzoAPI (defaultPParams)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Plutus (
  alwaysSucceedsPlutus,
  zeroTestingCostModel,
 )

tests :: Spec
tests =
  it
    "collectTwoPhaseScriptInputs output order"
    collectTwoPhaseScriptInputsOutputOrdering

-- Test for Plutus Data Ordering, using this strategy

-- | Never apply this to any Era but Alonzo or Babbage
collectTwoPhaseScriptInputsOutputOrdering ::
  Expectation
collectTwoPhaseScriptInputsOutputOrdering = do
  collectInputs @AlonzoEra testEpochInfo testSystemStart defaultPParams validatingTx initUTxO
    `shouldBe` Right
      [ PlutusWithContext
          { pwcProtocolVersion = pvMajor (defaultPParams @AlonzoEra ^. ppProtocolVersionL)
          , pwcScript = Left plutus
          , pwcScriptHash = hashPlutusScript plutus
          , pwcArgs = either (error . show) id $ do
              txInfo <- toPlutusTxInfo plutus lti
              toPlutusArgs
                plutus
                (defaultPParams @AlonzoEra ^. ppProtocolVersionL)
                txInfo
                spendingPurpose1
                (Just (datum @AlonzoEra))
                (redeemer @AlonzoEra)
          , pwcExUnits = ExUnits 5000 5000
          , pwcCostModel = zeroTestingCostModel PlutusV1
          }
      ]
  where
    plutus = alwaysSucceedsPlutus @'PlutusV1 3
    lti =
      LedgerTxInfo
        { ltiProtVer = defaultPParams @AlonzoEra ^. ppProtocolVersionL
        , ltiEpochInfo = testEpochInfo
        , ltiSystemStart = testSystemStart
        , ltiUTxO = initUTxO
        , ltiTx = validatingTx
        }

-- ============================== DATA ===============================

datum :: Era era => Data era
datum = Data (PV1.I 123)

redeemer :: Era era => Data era
redeemer = Data (PV1.I 42)

spendingPurpose1 :: AlonzoEraScript era => PlutusPurpose AsIxItem era
spendingPurpose1 = SpendingPurpose . AsIxItem 1 $ mkGenesisTxIn 1

validatingTx ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  Tx era
validatingTx =
  let script = alwaysSucceeds @PlutusV1 @era 3
   in mkBasicTx validatingBody
        & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated validatingBody) someKeys]
        & witsTxL . scriptTxWitsL .~ [(hashScript script, script)]
        & witsTxL . datsTxWitsL .~ TxDats [(hashData @era datum, datum)]
        & witsTxL . rdmrsTxWitsL .~ redeemers
  where
    validatingBody =
      mkBasicTxBody
        & inputsTxBodyL .~ [mkGenesisTxIn 1]
        & collateralInputsTxBodyL .~ [mkGenesisTxIn 11]
        & outputsTxBodyL .~ [mkBasicTxOut someAddr (inject $ Coin 4995)]
        & feeTxBodyL .~ Coin 5
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash defaultPParams [PlutusV1] redeemers (mkTxDats datum)
    redeemers =
      Redeemers [(SpendingPurpose $ AsIx 0, (redeemer, ExUnits 5000 5000))]

-- ============================== Helper functions ===============================

-- We have some tests that use plutus scripts, so they can only be run in
-- Babbage and Alonzo. How do we do that? We identify functions that are
-- only well typed in those Eras, and we make versions which are parameterized
-- by a proof. But which raise an error in other Eras.

collectInputs ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [PlutusWithContext]
collectInputs = collectPlutusScriptsWithContext

testEpochInfo :: EpochInfo (Either Text)
testEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

testSystemStart :: SystemStart
testSystemStart = SystemStart $ posixSecondsToUTCTime 0
