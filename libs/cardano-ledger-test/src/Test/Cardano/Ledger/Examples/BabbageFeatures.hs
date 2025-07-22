{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.BabbageFeatures (
  InOut,
  TestCaseData (..),
  InitOutputs (..),
  InitUtxo (..),
  KeyPairRole (..),
  txFromTestCaseData,
  utxoFromTestCaseData,
  babbageFeatures,
  toolTests,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, mkSupportedPlutusScript)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..), hashDataTxWitsL)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
  ProtVer (..),
  ShelleyBase,
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), dataToBinaryData, hashData)
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
 )
import Cardano.Ledger.Shelley.API (UTxO (..), UtxoEnv (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (pack)
import Data.Default (Default (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Tools (
  exUnitsTranslationRoundTrip,
  exampleExUnitCalc,
  exampleInvalidExUnitCalc,
 )
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
import Test.Cardano.Ledger.TreeDiff (ToExpr, showExpr)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

someKeys :: KeyPair 'Payment
someKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

someKeysPaymentKeyRole :: forall era. KeyPairRole era
someKeysPaymentKeyRole = KeyPairPayment someKeys

keysForMultisig :: KeyPair 'Witness
keysForMultisig = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 99)

keysForMultisigWitnessKeyRole :: forall era. KeyPairRole era
keysForMultisigWitnessKeyRole = KeyPairWitness keysForMultisig

keyHashForMultisig :: KeyHash 'Witness
keyHashForMultisig = hashKey $ vKey keysForMultisig

simpleScript :: forall era. ShelleyEraScript era => Script era
simpleScript = fromNativeScript $ RequireAllOf [RequireSignature keyHashForMultisig]

evenData3ArgsScript ::
  forall era.
  Reflect era =>
  Script era
evenData3ArgsScript =
  case reify @era of
    Shelley -> error unsupported
    Mary -> error unsupported
    Allegra -> error unsupported
    Alonzo -> evenData3ArgsLang @'PlutusV1
    Babbage -> evenData3ArgsLang @'PlutusV2
    Conway -> evenData3ArgsLang @'PlutusV2
  where
    unsupported = "Plutus scripts are not supported in: " ++ show (eraName @era)
    evenData3ArgsLang :: forall l era'. EraPlutusTxInfo l era' => Script era'
    evenData3ArgsLang =
      fromPlutusScript . mkSupportedPlutusScript . Plutus @l . PlutusBinary . SBS.pack $
        mconcat
          [ [88, 65, 1, 0, 0, 51, 50, 34, 51, 34, 34, 37, 51, 83, 0]
          , [99, 50, 35, 51, 87, 52, 102, 225, 192, 8, 0, 64, 40, 2, 76]
          , [200, 140, 220, 48, 1, 0, 9, 186, 208, 3, 72, 1, 18, 0, 1]
          , [0, 81, 50, 99, 83, 0, 64, 5, 73, 132, 128, 4, 128, 4, 72]
          , [128, 8, 72, 128, 4, 128, 5]
          ]

plainAddr :: Addr
plainAddr = mkAddr someKeys $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)

scriptAddr :: forall era. Reflect era => Script era -> Addr
scriptAddr s = mkAddr (hashScript s) $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 0)

simpleScriptAddr :: forall era. (Reflect era, ShelleyEraScript era) => Addr
simpleScriptAddr = scriptAddr @era simpleScript

datumExampleEven :: Era era => Data era
datumExampleEven = Data (PV1.I 2)

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

-- =========================================================================
-- Spend a EUTxO with an inline datum (without and with a failing script)
-- =========================================================================

inlineDatum ::
  forall era.
  ( Reflect era
  , AlonzoEraTxBody era
  , BabbageEraTxOut era
  , AlonzoEraTxWits era
  , EraModel era
  , BabbageEraPParams era
  ) =>
  TestCaseData era
inlineDatum =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr . inject $ Coin 4995]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers mempty
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut (scriptAddr @era evenData3ArgsScript) (inject $ Coin 5000)
                  & datumTxOutL .~ (Datum . dataToBinaryData $ datumExampleEven @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr . inject $ Coin 2115]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . hashScriptTxWitsL .~ [evenData3ArgsScript @era]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- =========================================================================
-- Valid: Use a reference script.
-- =========================================================================

referenceScript ::
  forall era.
  ( Reflect era
  , BabbageEraTxBody era
  , EraPlutusTxInfo PlutusV2 era
  , AlonzoEraTxWits era
  , EraModel era
  , BabbageEraPParams era
  ) =>
  TestCaseData era
referenceScript =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr . inject $ Coin 4995]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers txDats
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut (scriptAddr @era (alwaysSucceeds @PlutusV2 3)) (inject $ Coin 5000)
                  & dataHashTxOutL .~ SJust (hashData (datumExampleSixtyFiveBytes @era))
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
          & witsTxL . hashDataTxWitsL .~ [datumExampleSixtyFiveBytes @era]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

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

-- =========================================================================
-- Valid: Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScript ::
  forall era.
  (Reflect era, BabbageEraTxBody era, AlonzoEraTxWits era, EraModel era, BabbageEraPParams era) =>
  TestCaseData era
inlineDatumAndRefScript =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers mempty
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut (scriptAddr @era evenData3ArgsScript) (inject $ Coin 5000)
                  & datumTxOutL .~ Datum (dataToBinaryData $ datumExampleEven @era)
              ]
          , ofRefInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 5000)
                  & referenceScriptTxOutL .~ SJust evenData3ArgsScript
              ]
          , ofCollateral = [mkBasicTxOut @era plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Valid: Use a reference input with a data hash in the correspending output and
-- without supplying the correspending data witness.
-- ====================================================================================

refInputWithDataHashNoWit ::
  forall era.
  BabbageEraTxBody era =>
  TestCaseData era
refInputWithDataHashNoWit =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 1135)]
          & feeTxBodyL .~ Coin 5
    , initOutputs =
        InitOutputs
          { ofInputs = [mkBasicTxOut plainAddr (inject $ Coin 1140)]
          , ofRefInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 10)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = id
    }

-- =======================================================================================
-- Valid:  Use a reference input with a data hash in the correspending output and
-- supplying the correspending data witness.
-- =======================================================================================

refInputWithDataHashWithWit ::
  forall era.
  (Reflect era, BabbageEraTxBody era, AlonzoEraTxWits era, EraModel era, BabbageEraPParams era) =>
  TestCaseData era
refInputWithDataHashWithWit =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [] (mkRedeemers []) txDats
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 1135)]
          & feeTxBodyL .~ Coin 5
    , initOutputs =
        InitOutputs
          { ofInputs = [mkBasicTxOut plainAddr (inject $ Coin 1140)]
          , ofRefInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 10)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = witsTxL . hashDataTxWitsL .~ [datumExampleSixtyFiveBytes]
    }

-- ====================================================================================
-- Valid: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

certRedeemers ::
  (AlonzoEraScript era, EraModel era) =>
  Redeemers era
certRedeemers =
  mkRedeemers [(CertifyingPurpose $ AsIx 0, (Data (PV1.I 42), ExUnits 5000 5000))]

refscriptForDelegCert ::
  forall era.
  ( ShelleyEraTxCert era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , EraPlutusTxInfo PlutusV2 era
  , EraModel era
  , BabbageEraPParams era
  ) =>
  TestCaseData era
refscriptForDelegCert =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 1135)]
          & certsTxBodyL
            .~ [ UnRegTxCert (ScriptHashObj (hashScript @era $ alwaysSucceeds @PlutusV2 2))
               ]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] certRedeemers mempty
    , initOutputs =
        InitOutputs
          { ofInputs = [mkBasicTxOut plainAddr (inject $ Coin 1140)]
          , ofRefInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 5000)
                  & referenceScriptTxOutL .~ SJust (alwaysSucceeds @PlutusV2 2)
              ]
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = witsTxL . rdmrsTxWitsL .~ certRedeemers
    }

-- ====================================================================================
--  Valid: Don't run reference scripts in output for validation
-- ====================================================================================

refScriptInOutput :: forall era. (EraTxBody era, BabbageEraTxOut era) => TestCaseData era
refScriptInOutput =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut plainAddr (inject $ Coin 5000)
                  & referenceScriptTxOutL .~ SJust simpleScript
              ]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = id
    }

-- ====================================================================================
--  Valid: Unlock Simple Scripts with a Reference Script
-- ====================================================================================

simpleScriptOutWithRefScriptUTxOState ::
  forall era.
  (Reflect era, BabbageEraTxBody era) => TestCaseData era
simpleScriptOutWithRefScriptUTxOState =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (simpleScriptAddr @era)
                  (inject $ Coin 5000)
              ]
          , ofRefInputs =
              [ mkBasicTxOut
                  plainAddr
                  (inject $ Coin 5000)
                  & referenceScriptTxOutL .~ SJust simpleScript
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole, keysForMultisigWitnessKeyRole]
    , otherWitsFields = id
    }

-- ====================================================================================

type InOut era = (TxIn, TxOut era)

data TestCaseData era = TestCaseData
  { txBody :: TxBody era
  , initOutputs :: InitOutputs era
  , keysForAddrWits :: [KeyPairRole era]
  , otherWitsFields :: Tx era -> Tx era
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

utxoFromTestCaseData ::
  forall era.
  BabbageEraTxBody era =>
  TestCaseData era ->
  (UTxO era, UTxO era)
utxoFromTestCaseData (TestCaseData txBody' (InitOutputs ofInputs' ofRefInputs' ofCollateral') _ _) =
  let inputsIns = txBody' ^. inputsTxBodyL
      refInputsIns = txBody' ^. referenceInputsTxBodyL
      collateralIns = txBody' ^. collateralInputsTxBodyL

      inputs' = Set.toList inputsIns `zip` ofInputs'
      refInputs' = Set.toList refInputsIns `zip` ofRefInputs'
      collateral' = Set.toList collateralIns `zip` ofCollateral'

      newTxIns = fmap (TxIn (txIdTxBody txBody') . TxIx) [0 ..] :: [TxIn]
      newTxInOuts = newTxIns `zip` toList (txBody' ^. outputsTxBodyL)

      initUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ collateral')
      expectedUtxo = UTxO $ Map.fromList (newTxInOuts ++ refInputs' ++ collateral')
   in (initUtxo, expectedUtxo)

txFromTestCaseData ::
  forall era.
  EraTx era =>
  TestCaseData era ->
  Tx era
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
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , Reflect era
  , BabbageEraTxBody era
  , AlonzoEraTx era
  , STS (EraRule "UTXOW" era)
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , BabbageEraPParams era
  ) =>
  TestCaseData era ->
  Assertion
testExpectSuccessValid tc =
  let txBody' = txBody tc
      tx' = txFromTestCaseData tc
      fees = txBody' ^. feeTxBodyL
      (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData tc

      newTxIn = TxIn (txIdTxBody txBody') minBound
      newTxInOut = [newTxIn] `zip` (maybeToList . StrictSeq.lookup 0) (txBody' ^. outputsTxBodyL)

      initUtxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
      expectedUtxo = UTxO $ Map.fromList (newTxInOut ++ refInputs' ++ collateral')
      expectedState = smartUTxOState defaultPParams expectedUtxo (Coin 0) fees def mempty
      assumedValidTx = tx' & isValidTxL .~ IsValid True
      env = UtxoEnv (SlotNo 0) defaultPParams def
      state = smartUTxOState defaultPParams initUtxo (Coin 0) (Coin 0) def mempty
   in runSTS @"UTXOW" @era
        (TRC (env, state, assumedValidTx))
        (genericCont (show assumedValidTx) $ Right expectedState)

genericBabbageFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , BabbageEraTxBody era
  , Reflect era
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , ShelleyEraTest era
  , AlonzoEraTx era
  , STS (EraRule "UTXOW" era)
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , EraPlutusTxInfo PlutusV2 era
  , EraModel era
  , BabbageEraPParams era
  ) =>
  TestTree
genericBabbageFeatures =
  testGroup
    (eraName @era ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $ testExpectSuccessValid @era inlineDatum
        , testCase "reference script" $ testExpectSuccessValid @era referenceScript
        , testCase "inline datum and ref script" $ testExpectSuccessValid @era inlineDatumAndRefScript
        , testCase "reference input with data hash, no data witness" $
            testExpectSuccessValid @era refInputWithDataHashNoWit
        , testCase "reference input with data hash, with data witness" $
            testExpectSuccessValid @era refInputWithDataHashWithWit
        , testCase "reference script to authorize delegation certificate" $
            testExpectSuccessValid @era refscriptForDelegCert
        , testCase "reference script in output" $ testExpectSuccessValid @era refScriptInOutput
        , testCase "spend simple script output with reference script" $
            testExpectSuccessValid @era simpleScriptOutWithRefScriptUTxOState
        ]
    ]

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures @BabbageEra
    , genericBabbageFeatures @ConwayEra
    , testCase "inputs and refinputs overlap in Babbage and don't Fail" $
        testExpectSuccessValid @BabbageEra commonReferenceScript
    , testCase "inputs and refinputs overlap in Conway and Fail" $
        testExpectUTXOFailure
          @ConwayEra
          commonReferenceScript
          (Conway.BabbageNonDisjointRefInputs (pure commonTxIn))
    ]

testExpectUTXOFailure ::
  forall era.
  ( Reflect era
  , BabbageEraTxBody era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , BaseM (EraRule "UTXO" era) ~ ShelleyBase
  , Tx era ~ Signal (EraRule "UTXO" era)
  , STS (EraRule "UTXO" era)
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , BabbageEraPParams era
  ) =>
  TestCaseData era ->
  PredicateFailure (EraRule "UTXO" era) ->
  Assertion
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
            Left (predfail :| []) -> assertEqual "unexpected failure" predfail failure
            Left xs -> assertFailure $ "not exactly one failure" <> showExpr xs
            Right _ -> assertFailure "testExpectUTXOFailure succeeds"
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

toolTests :: TestTree
toolTests =
  testGroup
    "ExUnit tools"
    [ testProperty "Plutus ExUnit translation round-trip" exUnitsTranslationRoundTrip
    , testGroup
        "Alonzo"
        [ testCase "calculate ExUnits" (exampleExUnitCalc @AlonzoEra)
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc @AlonzoEra)
        ]
    , testGroup
        "Babbage"
        [ testCase "calculate ExUnits" (exampleExUnitCalc @BabbageEra)
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc @BabbageEra)
        ]
    ]
