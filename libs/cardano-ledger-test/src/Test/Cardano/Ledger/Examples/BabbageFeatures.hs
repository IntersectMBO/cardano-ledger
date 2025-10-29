{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.BabbageFeatures (
  babbageFeatures,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..), hashDataTxWitsL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
  ProtVer (..),
  ShelleyBase,
  SlotNo (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (UTxO (..), UtxoEnv (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysSucceeds)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  genericCont,
  mkGenesisTxIn,
  mkTxDats,
 )
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Era (ShelleyEraTest)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')

someKeys :: KeyPair 'Payment
someKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

someKeysPaymentKeyRole :: forall era. KeyPairRole era
someKeysPaymentKeyRole = KeyPairPayment someKeys

plainAddr :: Addr
plainAddr = mkAddr someKeys $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)

scriptAddr :: forall era. Reflect era => Script era -> Addr
scriptAddr s = mkAddr (hashScript s) $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 0)

validatingRedeemers :: AlonzoEraScript era => Redeemers era
validatingRedeemers =
  Redeemers [(SpendingPurpose $ AsIx 0, (Data (PV1.I 42), ExUnits 5000 5000))]

-- We intentionally use a ByteString with length greater than 64 to serve as
-- as reminder that our protection against contiguous data over 64 Bytes on
-- the wire is done during deserialization using the Plutus library.
sixtyFiveBytes :: BS.ByteString
sixtyFiveBytes = BS.pack [1 .. 65]

datumExampleSixtyFiveBytes :: Era era => Data era
datumExampleSixtyFiveBytes = Data (PV1.B sixtyFiveBytes)

txDats :: Era era => TxDats era
txDats = mkTxDats datumExampleSixtyFiveBytes

someTxIn :: HasCallStack => TxIn
someTxIn = mkGenesisTxIn 1

anotherTxIn :: HasCallStack => TxIn
anotherTxIn = mkGenesisTxIn 2

yetAnotherTxIn :: HasCallStack => TxIn
yetAnotherTxIn = mkGenesisTxIn 3

commonTxIn :: HasCallStack => TxIn
commonTxIn = mkGenesisTxIn 4

commonReferenceScript ::
  forall era.
  ( Reflect era
  , BabbageEraTxBody era
  , EraPlutusTxInfo PlutusV2 era
  , AlonzoEraTxWits era
  , EraModel era
  , BabbageEraPParams era
  ) =>
  TestCaseData era
commonReferenceScript =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn, commonTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn, commonTxIn]
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers txDats
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut (scriptAddr @era (alwaysSucceeds @PlutusV2 3)) (inject $ Coin 2500)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              , mkBasicTxOut plainAddr (inject $ Coin 2500)
              ]
          , ofRefInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 5000)
                  & referenceScriptTxOutL .~ SJust (alwaysSucceeds @PlutusV2 3)
              ]
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . hashDataTxWitsL .~ [datumExampleSixtyFiveBytes]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================

type InOut era = (TxIn, TxOut era)

data TestCaseData era = TestCaseData
  { txBody :: TxBody TopTx era
  , initOutputs :: InitOutputs era
  , keysForAddrWits :: [KeyPairRole era]
  , otherWitsFields :: Tx TopTx era -> Tx TopTx era
  }

data InitOutputs era = InitOutputs
  { ofInputs :: [TxOut era]
  , ofRefInputs :: [TxOut era]
  , ofCollateral :: [TxOut era]
  }

data InitUtxo era = InitUtxo
  { inputs :: [InOut era]
  , refInputs :: [InOut era]
  , collateral :: [InOut era]
  }

data KeyPairRole era
  = KeyPairPayment (KeyPair 'Payment)
  | KeyPairWitness (KeyPair 'Witness)
  | KeyPairStakePool (KeyPair 'StakePool)
  | KeyPairDRep (KeyPair 'DRepRole)
  | KeyPairCommittee (KeyPair 'HotCommitteeRole)

initUtxoFromTestCaseData ::
  BabbageEraTxBody era =>
  TestCaseData era ->
  InitUtxo era
initUtxoFromTestCaseData
  (TestCaseData txBody' (InitOutputs ofInputs' ofRefInputs' ofCollateral') _ _) =
    let inputsIns = txBody' ^. inputsTxBodyL
        refInputsIns = txBody' ^. referenceInputsTxBodyL
        collateralIns = txBody' ^. collateralInputsTxBodyL

        inputs' = Set.toList inputsIns `zip` ofInputs'
        refInputs' = Set.toList refInputsIns `zip` ofRefInputs'
        collateral' = Set.toList collateralIns `zip` ofCollateral'
     in InitUtxo inputs' refInputs' collateral'

txFromTestCaseData ::
  forall era.
  EraTx era =>
  TestCaseData era ->
  Tx TopTx era
txFromTestCaseData
  testCaseData =
    let addrWits =
          fmap
            ( \case
                KeyPairPayment p -> mkWitnessVKey (hashAnnotated (txBody testCaseData)) p
                KeyPairWitness w -> mkWitnessVKey (hashAnnotated (txBody testCaseData)) w
                KeyPairStakePool s -> mkWitnessVKey (hashAnnotated (txBody testCaseData)) s
                KeyPairDRep d -> mkWitnessVKey (hashAnnotated (txBody testCaseData)) d
                KeyPairCommittee d -> mkWitnessVKey (hashAnnotated (txBody testCaseData)) d
            )
            (keysForAddrWits testCaseData)
     in otherWitsFields testCaseData $
          mkBasicTx (txBody testCaseData)
            & witsTxL . addrTxWitsL .~ Set.fromList addrWits

testExpectSuccessValid ::
  forall era.
  ( ShelleyEraTest era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Tx TopTx era ~ Signal (EraRule "UTXOW" era)
  , Reflect era
  , BabbageEraTxBody era
  , AlonzoEraTx era
  , STS (EraRule "UTXOW" era)
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , BabbageEraPParams era
  ) =>
  TestCaseData era ->
  Expectation
testExpectSuccessValid tc =
  let txBody' = txBody tc
      tx' = txFromTestCaseData tc
      fees = txBody' ^. feeTxBodyL
      InitUtxo {inputs, refInputs, collateral} = initUtxoFromTestCaseData tc

      newTxIn = TxIn (txIdTxBody txBody') minBound
      newTxInOut = [newTxIn] `zip` (maybeToList . StrictSeq.lookup 0) (txBody' ^. outputsTxBodyL)

      initUtxo = (UTxO . Map.fromList) $ inputs ++ refInputs ++ collateral
      expectedUtxo = UTxO $ Map.fromList (newTxInOut ++ refInputs ++ collateral)
      expectedState = smartUTxOState defaultPParams expectedUtxo (Coin 0) fees def mempty
      assumedValidTx = tx' & isValidTxL .~ IsValid True
      env = UtxoEnv (SlotNo 0) defaultPParams def
      state = smartUTxOState defaultPParams initUtxo (Coin 0) (Coin 0) def mempty
   in runSTS @"UTXOW" @era
        (TRC (env, state, assumedValidTx))
        (genericCont (show assumedValidTx) $ Right expectedState)

babbageFeatures :: Spec
babbageFeatures =
  describe "Babbage Features" $ do
    it "inputs and refinputs overlap in Babbage and don't Fail" $
      testExpectSuccessValid @BabbageEra commonReferenceScript
    it "inputs and refinputs overlap in Conway and Fail" $
      testExpectUTXOFailure
        @ConwayEra
        commonReferenceScript
        (Conway.BabbageNonDisjointRefInputs (pure commonTxIn))

testExpectUTXOFailure ::
  forall era.
  ( Reflect era
  , BabbageEraTxBody era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , Tx TopTx era ~ Signal (EraRule "UTXO" era)
  , STS (EraRule "UTXO" era)
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , BabbageEraPParams era
  ) =>
  TestCaseData era ->
  PredicateFailure (EraRule "UTXO" era) ->
  Expectation
testExpectUTXOFailure tc failure =
  let tx' = txFromTestCaseData tc
      InitUtxo inputs' refInputs' collateral' = initUtxoFromTestCaseData @era tc
      initUtxo = UTxO . Map.fromList $ inputs' ++ refInputs' ++ collateral'
      pparams = defaultPParams
      env = Shelley.UtxoEnv (SlotNo 0) pparams def
      state = smartUTxOState pparams initUtxo (Coin 0) (Coin 0) def mempty
   in goSTS
        @"UTXO"
        @era
        env
        state
        tx'
        ( \case
            Left (predfail :| []) -> predfail `shouldBe` failure
            Left xs -> expectationFailure $ "not exactly one failure" <> showExpr xs
            Right _ -> expectationFailure "testExpectUTXOFailure succeeds"
        )

defaultPParams :: forall era. (AlonzoEraScript era, BabbageEraPParams era) => PParams era
defaultPParams =
  emptyPParams @era
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1, PlutusV2]
    & ppMaxValSizeL .~ 1_000_000_000
    & ppMaxTxExUnitsL .~ ExUnits 1_000_000 1_000_000
    & ppMaxBlockExUnitsL .~ ExUnits 1_000_000 1_000_000
    & ppProtocolVersionL .~ ProtVer (eraProtVerLow @era) 0
    & ppCollateralPercentageL .~ 1
    & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 5)
