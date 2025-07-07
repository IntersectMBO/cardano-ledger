{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
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
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, mkSupportedPlutusScript)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (
  TxOutSource (TxOutFromInput, TxOutFromOutput),
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (CollectErrors),
  AlonzoUtxowPredFailure (MissingRequiredDatums, NotAllowedSupplementalDatums),
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import qualified Cardano.Ledger.Babbage.Collateral as Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (
  BabbageContextError (
    InlineDatumsNotSupported,
    ReferenceInputsNotSupported,
    ReferenceScriptsNotSupported
  ),
 )
import Cardano.Ledger.BaseTypes (
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), dataToBinaryData, hashData)
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
 )
import Cardano.Ledger.Shelley.API (UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
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
import Lens.Micro ((^.), (&), (.~), (%~), (<>~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common (showExpr)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  mkGenesisTxIn,
  mkTxDats,
  testUTXOW,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxOutField (..),
 )
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.GenState (
  PlutusPurposeTag (..),
  mkRedeemers,
  mkRedeemersFromTags,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import qualified Test.Cardano.Ledger.Generic.Scriptic as Scriptic
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

someKeys :: forall era. Proof era -> KeyPair 'Payment
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 1 1 1 1)

someKeysPaymentKeyRole :: forall era. Proof era -> KeyPairRole era
someKeysPaymentKeyRole pf = KeyPairPayment (someKeys pf)

keysForMultisig :: forall era. Proof era -> KeyPair 'Witness
keysForMultisig _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 99)

keysForMultisigWitnessKeyRole :: forall era. Proof era -> KeyPairRole era
keysForMultisigWitnessKeyRole pf = KeyPairWitness (keysForMultisig pf)

keyHashForMultisig :: forall era. Proof era -> KeyHash 'Witness
keyHashForMultisig pf = hashKey . vKey $ keysForMultisig pf

simpleScript :: forall era. Scriptic era => Proof era -> Script era
simpleScript pf = fromNativeScript $ Scriptic.allOf [require @era (keyHashForMultisig pf)] pf

evenData3ArgsScript :: HasCallStack => Proof era -> Script era
evenData3ArgsScript proof =
  case proof of
    Shelley -> error unsupported
    Mary -> error unsupported
    Allegra -> error unsupported
    Alonzo -> evenData3ArgsLang @'PlutusV1
    Babbage -> evenData3ArgsLang @'PlutusV2
    Conway -> evenData3ArgsLang @'PlutusV2
  where
    unsupported = "Plutus scripts are not supported in:" ++ show proof
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

plainAddr :: forall era. Proof era -> Addr
plainAddr pf = mkAddr (someKeys pf) $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)

scriptAddr :: forall era. Reflect era => Proof era -> Script era -> Addr
scriptAddr _pf s = mkAddr (hashScript s) $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 0)

simpleScriptAddr :: forall era. (Reflect era, Scriptic era) => Proof era -> Addr
simpleScriptAddr pf = scriptAddr pf (simpleScript pf)

datumExampleEven :: Era era => Data era
datumExampleEven = Data (PV1.I 2)

datumExampleOdd :: Era era => Data era
datumExampleOdd = Data (PV1.I 3)

validatingRedeemers :: Era era => Proof era -> Redeemers era
validatingRedeemers proof =
  mkRedeemersFromTags
    proof
    [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

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

defaultPPs :: Proof era -> [PParamsField era]
defaultPPs p =
  [ Costmdls $ zeroTestingCostModels [PlutusV1, PlutusV2]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ protocolVersion p
  , CollateralPercentage 1
  , CoinPerUTxOByte (CoinPerByte (Coin 5))
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf (defaultPPs pf)

-- =========================================================================
-- Spend a EUTxO with an inline datum (without and with a failing script)
-- =========================================================================

inlineDatum ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
inlineDatum pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = evenData3ArgsScript pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

inlineDatumFailingScript ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
inlineDatumFailingScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleOdd @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = evenData3ArgsScript pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- =========================================================================
-- Valid: Use a reference script.
-- =========================================================================

referenceScript ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
referenceScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (alwaysAlt 3 pf))
                  , Amount (inject $ Coin 5000)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ alwaysAlt 3 pf)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        wits
          & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
          & rdmrsTxWitsL .~ validatingRedeemers pf
    }

commonReferenceScript ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
commonReferenceScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn, commonTxIn]
          , RefInputs' [anotherTxIn, commonTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (alwaysAlt 3 pf))
                  , Amount (inject $ Coin 2500)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              , newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 2500)
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ alwaysAlt 3 pf)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        wits
          & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
          & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- =========================================================================
-- Valid: Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScript ::
  forall era.
  (Scriptic era, Reflect era, AlonzoEraTxWits era) =>
  Proof era ->
  TestCaseData era
inlineDatumAndRefScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ evenData3ArgsScript pf)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- =========================================================================
-- Invalid: Spend a EUTxO with an inline datum, using a reference script,
-- and also redundantly supply the script witness.
-- =========================================================================

inlineDatumAndRefScriptWithRedundantWitScript ::
  forall era.
  (Scriptic era, Reflect era, AlonzoEraTxWits era) =>
  Proof era ->
  TestCaseData era
inlineDatumAndRefScriptWithRedundantWitScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ evenData3ArgsScript pf)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = alwaysAlt 3 pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)] -- This is redundant with the reference script
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================
-- Valid: Use a reference input with a data hash in the correspending output and
-- without supplying the correspending data witness.
-- ====================================================================================

refInputWithDataHashNoWit ::
  forall era.
  (Scriptic era, EraTxBody era) =>
  Proof era ->
  TestCaseData era
refInputWithDataHashNoWit pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]]
          , Txfee (Coin 5)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 10)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = id
    }

-- =======================================================================================
-- Valid:  Use a reference input with a data hash in the correspending output and
-- supplying the correspending data witness.
-- =======================================================================================

refInputWithDataHashWithWit ::
  forall era.
  (Scriptic era, Reflect era, AlonzoEraTxWits era) =>
  Proof era ->
  TestCaseData era
refInputWithDataHashWithWit pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , WppHash (newScriptIntegrityHash pf (pp pf) [] (mkRedeemers pf []) txDats)
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]]
          , Txfee (Coin 5)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 10)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = datsTxWitsL .~ [datumExampleSixtyFiveBytes]
    }

-- ====================================================================================
-- Valid: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

certRedeemers :: Era era => Proof era -> Redeemers era
certRedeemers proof =
  mkRedeemersFromTags
    proof
    [((Certifying, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

refscriptForDelegCert ::
  forall era.
  ( Scriptic era
  , EraTxBody era
  , ShelleyEraTxCert era
  , AlonzoEraTxWits era
  ) =>
  Proof era ->
  TestCaseData era
refscriptForDelegCert pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]]
          , Certs'
              [ UnRegTxCert (ScriptHashObj (hashScript @era $ alwaysAlt 2 pf))
              ]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (certRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ alwaysAlt 2 pf)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = rdmrsTxWitsL .~ certRedeemers pf
    }

-- ====================================================================================
--  Invalid: Use a collateral output
-- ====================================================================================

useCollateralReturn ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
useCollateralReturn pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , CollateralReturn' [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2110)]]
          , TotalCol (SJust $ Coin 5)
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (never 3 pf))
                  , Amount (inject $ Coin 5000)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = never 3 pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================
-- Invalid: Invalid collateral total
-- ====================================================================================

incorrectCollateralTotal ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
incorrectCollateralTotal pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , CollateralReturn' [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2110)]]
          , TotalCol (SJust $ Coin 6)
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = evenData3ArgsScript pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================
-- Invalid: Inline datum used with redundant datum in witness set
-- ====================================================================================

inlineDatumRedundantDatumWit ::
  forall era.
  (Scriptic era, Reflect era, AlonzoEraTxWits era) =>
  Proof era ->
  TestCaseData era
inlineDatumRedundantDatumWit pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = evenData3ArgsScript pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
              & rdmrsTxWitsL .~ (validatingRedeemers pf)
    }

-- ====================================================================================
-- Invalid:  Using inline datums with Plutus V1 script
-- ====================================================================================

inlineDatumWithPlutusV1Script ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
inlineDatumWithPlutusV1Script pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemers pf) mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf))
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = always 3 pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & rdmrsTxWitsL .~ (validatingRedeemers pf)
    }

-- ====================================================================================
-- Invalid:  Using reference script with Plutus V1 script
-- ====================================================================================

referenceScriptWithPlutusV1Script ::
  forall era.
  ( Scriptic era
  , Reflect era
  , AlonzoEraTxWits era
  ) =>
  Proof era -> TestCaseData era
referenceScriptWithPlutusV1Script pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs'
              [ newTxOut
                  pf
                  [Address (plainAddr pf), Amount (inject $ Coin 4995), RefScript (SJust $ simpleScript pf)]
              ]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf))
                  , Amount (inject $ Coin 5000)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = always 3 pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================
-- Invalid:  Using reference input with Plutus V1 script
-- ====================================================================================

referenceInputWithPlutusV1Script ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
referenceInputWithPlutusV1Script pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemers pf) txDats)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf))
                  , Amount (inject $ Coin 5000)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf))
                  , Amount (inject $ Coin 5000)
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let script = always 3 pf
         in wits
              & scriptTxWitsL .~ [(hashScript script, script)]
              & datsTxWitsL .~ [datumExampleSixtyFiveBytes]
              & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================
--  Valid: Don't run reference scripts in output for validation
-- ====================================================================================

refScriptInOutput :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
refScriptInOutput pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ simpleScript pf)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = id
    }

-- ====================================================================================
--  Valid: Unlock Simple Scripts with a Reference Script
-- ====================================================================================

simpleScriptOutWithRefScriptUTxOState ::
  (Scriptic era, Reflect era) => Proof era -> TestCaseData era
simpleScriptOutWithRefScriptUTxOState pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (simpleScriptAddr pf)
                  , Amount (inject $ Coin 5000)
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript (SJust $ simpleScript pf)
                  ]
              ]
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf, keysForMultisigWitnessKeyRole pf]
    , otherWitsFields = id
    }

-- ========================================================================================
-- Invalid: TxOut too large for the included ADA, using a large inline datum
-- ========================================================================================

largeDatum :: Era era => Data era
largeDatum = Data (PV1.B . BS.pack $ replicate 1500 0)

largeOutput' :: forall era. EraTxOut era => Proof era -> TxOut era
largeOutput' pf =
  newTxOut
    pf
    [ Address (plainAddr pf)
    , Amount (inject $ Coin 1135)
    , FDatum . Datum . dataToBinaryData $ largeDatum @era
    ]

largeOutput :: forall era. BabbageEraTxBody era => Proof era -> TestCaseData era
largeOutput pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Outputs' [largeOutput' pf]
          , Txfee (Coin 5)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = id
    }

-- =============================================================================
-- Invalid:  There is no such thing as a "reference datum".
-- In other words,  you cannot include a reference input that contains an
-- inline datum and have it count for the datum witness where ever it is needed.
-- =============================================================================

noSuchThingAsReferenceDatum ::
  forall era. (Scriptic era, Reflect era, AlonzoEraTxWits era) => Proof era -> TestCaseData era
noSuchThingAsReferenceDatum pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn] -- Note that this reference input has the required datum
          , Collateral' [yetAnotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] (validatingRedeemers pf) (TxDats mempty))
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (alwaysAlt 3 pf))
                  , Amount (inject $ Coin 5000)
                  , DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ]
          , ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  -- Note that this inline datum does not witness the datum for the plutus script
                  ]
              ]
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = \wits ->
        let
          script = alwaysAlt 3 pf
         in
          wits
            & scriptTxWitsL .~ [(hashScript script, script)]
            & rdmrsTxWitsL .~ validatingRedeemers pf
    }

-- ====================================================================================

type InOut era = (TxIn, TxOut era)

data TestCaseData era = TestCaseData
  { txBody :: TxBody era
  , initOutputs :: InitOutputs era
  , keysForAddrWits :: [KeyPairRole era]
  , otherWitsFields :: TxWits era -> TxWits era
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
  Proof era ->
  TestCaseData era ->
  InitUtxo era
initUtxoFromTestCaseData
  pf
  (TestCaseData txBody' (InitOutputs ofInputs' ofRefInputs' ofCollateral') _ _) =
    let inputsIns = getInputs pf txBody'
        refInputsIns = txBody' ^. referenceInputsTxBodyL
        collateralIns = txBody' ^. collateralInputsTxBodyL

        inputs' = Set.toList inputsIns `zip` ofInputs'
        refInputs' = Set.toList refInputsIns `zip` ofRefInputs'
        collateral' = Set.toList collateralIns `zip` ofCollateral'
     in InitUtxo inputs' refInputs' collateral'

utxoFromTestCaseData ::
  forall era.
  BabbageEraTxBody era =>
  Proof era ->
  TestCaseData era ->
  (UTxO era, UTxO era)
utxoFromTestCaseData pf (TestCaseData txBody' (InitOutputs ofInputs' ofRefInputs' ofCollateral') _ _) =
  let inputsIns = getInputs pf txBody'
      refInputsIns = txBody' ^. referenceInputsTxBodyL
      collateralIns = txBody' ^. collateralInputsTxBodyL

      inputs' = Set.toList inputsIns `zip` ofInputs'
      refInputs' = Set.toList refInputsIns `zip` ofRefInputs'
      collateral' = Set.toList collateralIns `zip` ofCollateral'

      newTxIns = fmap (TxIn (txIdTxBody txBody') . TxIx) [0 ..] :: [TxIn]
      newTxInOuts = newTxIns `zip` toList (getOutputs pf txBody')

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
     in mkBasicTx (txBody testCaseData)
          & witsTxL %~ otherWitsFields testCaseData
          & witsTxL . addrTxWitsL <>~ Set.fromList addrWits

testExpectSuccessValid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , Reflect era
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessValid
  pf
  tc =
    let txBody' = txBody tc
        tx' = txFromTestCaseData tc
        fees = txBody' ^. feeTxBodyL
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc

        newTxIn = TxIn (txIdTxBody txBody') minBound
        newTxInOut = [newTxIn] `zip` (maybeToList . StrictSeq.lookup 0) (getOutputs pf txBody')

        initUtxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
        expectedUtxo = UTxO $ Map.fromList (newTxInOut ++ refInputs' ++ collateral')
        expectedState = smartUTxOState (pp pf) expectedUtxo (Coin 0) fees def mempty
        assumedValidTx = trustMeP pf True tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedValidTx (Right expectedState)

newColReturn ::
  forall era.
  BabbageEraTxBody era =>
  TxBody era ->
  [InOut era]
newColReturn
  txBody' =
    let newColReturnTxIn = mkTxInPartial (txIdTxBody txBody') 1
        colReturnOut = case txBody' ^. collateralReturnTxBodyL of
          SNothing -> []
          SJust rOut -> [rOut]
     in [newColReturnTxIn] `zip` colReturnOut

testExpectSuccessInvalid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , Reflect era
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessInvalid
  pf
  tc =
    let txBody' = txBody tc
        tx' = txFromTestCaseData tc
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc
        initUtxo = UTxO . Map.fromList $ inputs' ++ refInputs' ++ collateral'
        DeltaCoin colBallance = Collateral.collAdaBalance txBody' (Map.fromList collateral')
        expectedUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ newColReturn txBody')
        expectedState = smartUTxOState (pp pf) expectedUtxo (Coin 0) (Coin colBallance) def mempty
        assumedInvalidTx = trustMeP pf False tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedInvalidTx (Right expectedState)

testExpectFailure ::
  forall era.
  ( BabbageEraTxBody era
  , Reflect era
  ) =>
  Proof era ->
  TestCaseData era ->
  PredicateFailure (EraRule "UTXOW" era) ->
  Assertion
testExpectFailure
  pf
  tc
  predicateFailure =
    let tx' = txFromTestCaseData tc
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc
        utxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
     in testUTXOW (UTXOW pf) utxo (pp pf) (trustMeP pf True tx') (Left $ pure predicateFailure)

genericBabbageFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , BabbageEraTxBody era
  , PostShelley era
  , Reflect era
  , AlonzoEraTxWits era
  ) =>
  Proof era ->
  TestTree
genericBabbageFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $ testExpectSuccessValid pf (inlineDatum pf)
        , testCase "reference script" $ testExpectSuccessValid pf (referenceScript pf)
        , testCase "inline datum and ref script" $ testExpectSuccessValid pf (inlineDatumAndRefScript pf)
        , testCase "reference input with data hash, no data witness" $
            testExpectSuccessValid pf (refInputWithDataHashNoWit pf)
        , testCase "reference input with data hash, with data witness" $
            testExpectSuccessValid pf (refInputWithDataHashWithWit pf)
        , testCase "reference script to authorize delegation certificate" $
            testExpectSuccessValid pf (refscriptForDelegCert pf)
        , testCase "reference script in output" $ testExpectSuccessValid pf (refScriptInOutput pf)
        , testCase "spend simple script output with reference script" $
            testExpectSuccessValid pf (simpleScriptOutWithRefScriptUTxOState pf)
        ]
    ]

badTranslation :: Proof era -> BabbageContextError era -> CollectError era
badTranslation proof x =
  case proof of
    Babbage -> BadTranslation x
    Conway -> BadTranslation (BabbageContextError x)
    _ -> error "No reference inputs before BabbageEra"

plutusV1RefScriptFailures ::
  forall era.
  ( PostShelley era
  , BabbageEraTxBody era
  , Reflect era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure era
  , AlonzoEraTxWits era
  ) =>
  Proof era ->
  TestTree
plutusV1RefScriptFailures pf =
  testGroup
    (show pf ++ " PlutusV1 reference script failure examples")
    [ testCase "reference script with Plutus V1" $
        testExpectFailure
          pf
          (referenceScriptWithPlutusV1Script pf)
          ( injectFailure
              ( CollectErrors
                  [badTranslation pf $ ReferenceScriptsNotSupported (TxOutFromOutput (TxIx 0))]
              )
          )
    , testCase "reference input with Plutus V1" $
        testExpectFailure
          pf
          (referenceInputWithPlutusV1Script pf)
          ( injectFailure
              ( CollectErrors
                  [badTranslation pf $ ReferenceInputsNotSupported @era $ Set.singleton anotherTxIn]
              )
          )
    ]

genericBabbageFailures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure era
  , InjectRuleFailure "UTXOW" Shelley.ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxoPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , BabbageEraTxBody era
  , PostShelley era
  , Reflect era
  , AlonzoEraTxWits era
  ) =>
  Proof era ->
  TestTree
genericBabbageFailures pf =
  testGroup
    (show pf ++ " UTXOW failure examples")
    [ testGroup
        "invalid transactions"
        [ testCase "inline datum failing script" $ testExpectSuccessInvalid pf (inlineDatumFailingScript pf)
        , testCase "use a collateral output" $ testExpectSuccessInvalid pf (useCollateralReturn pf)
        , testCase "incorrect collateral total" $
            testExpectFailure
              pf
              (incorrectCollateralTotal pf)
              (injectFailure (IncorrectTotalCollateralField (DeltaCoin 5) (Coin 6)))
        , testCase "inline datum and ref script and redundant script witness" $
            testExpectFailure
              pf
              (inlineDatumAndRefScriptWithRedundantWitScript pf)
              ( injectFailure
                  (Shelley.ExtraneousScriptWitnessesUTXOW (Set.singleton $ hashScript @era (alwaysAlt 3 pf)))
              )
        , testCase "inline datum with redundant datum witness" $
            testExpectFailure
              pf
              (inlineDatumRedundantDatumWit pf)
              ( injectFailure
                  ( NotAllowedSupplementalDatums
                      (Set.singleton $ hashData @era datumExampleSixtyFiveBytes)
                      mempty
                  )
              )
        , testCase "inline datum with Plutus V1" $
            testExpectFailure
              pf
              (inlineDatumWithPlutusV1Script pf)
              ( injectFailure
                  ( CollectErrors
                      [badTranslation pf $ InlineDatumsNotSupported (TxOutFromInput someTxIn)]
                  )
              )
        , testCase "min-utxo value with output too large" $
            testExpectFailure
              pf
              (largeOutput pf)
              (injectFailure $ BabbageOutputTooSmallUTxO [(largeOutput' pf, Coin 8915)])
        , testCase "no such thing as a reference datum" $
            testExpectFailure
              pf
              (noSuchThingAsReferenceDatum pf)
              ( injectFailure
                  ( MissingRequiredDatums
                      (Set.singleton (hashData $ datumExampleSixtyFiveBytes @era))
                      mempty
                  )
              )
        ]
    ]

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures Babbage
    , genericBabbageFailures Babbage
    , plutusV1RefScriptFailures Babbage
    , genericBabbageFeatures Conway
    , genericBabbageFailures Conway
    , testCase "inputs and refinputs overlap in Babbage and don't Fail" $
        testExpectSuccessValid Babbage (commonReferenceScript Babbage)
    , testCase "inputs and refinputs overlap in Conway and Fail" $
        testExpectUTXOFailure
          Conway
          (commonReferenceScript Conway)
          (Conway.BabbageNonDisjointRefInputs (pure commonTxIn))
    ]

testExpectUTXOFailure ::
  forall era.
  ( PostShelley era
  , Reflect era
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  TestCaseData era ->
  PredicateFailure (EraRule "UTXO" era) ->
  Assertion
testExpectUTXOFailure pf@Conway tc failure =
  let tx' = txFromTestCaseData tc
      InitUtxo inputs' refInputs' collateral' = initUtxoFromTestCaseData pf tc
      initUtxo = UTxO . Map.fromList $ inputs' ++ refInputs' ++ collateral'
      pparams = newPParams pf (defaultPPs pf)
      env = Shelley.UtxoEnv (SlotNo 0) pparams def
      state = smartUTxOState pparams initUtxo (Coin 0) (Coin 0) def mempty
   in goSTS
        (UTXO pf)
        env
        state
        tx'
        ( \case
            Left (predfail :| []) -> assertEqual "unexpected failure" predfail failure
            Left xs -> assertFailure $ "not exactly one failure" <> showExpr xs
            Right _ -> assertFailure "testExpectUTXOFailure succeeds"
        )
testExpectUTXOFailure _ _ _ = error "testExpectUTXOFailure is only good in Conway Era"
