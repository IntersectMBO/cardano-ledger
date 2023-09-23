{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
) where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (CollectErrors),
  AlonzoUtxowPredFailure (MissingRequiredDatums, NonOutputSupplimentaryDatums),
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (PlutusScript), ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.TxInfo (
  TranslationError (InlineDatumsNotSupported, ReferenceInputsNotSupported, ReferenceScriptsNotSupported),
  TxOutSource (TxOutFromInput, TxOutFromOutput),
 )
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..))
import qualified Cardano.Ledger.Babbage.Collateral as Collateral (collAdaBalance)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxBody (..),
  Datum (..),
 )
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  mkTxIx,
  mkTxIxPartial,
  natVersion,
 )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  hashKey,
 )
import Cardano.Ledger.Language (BinaryPlutus (..), Language (..), Plutus (..))
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (ProtVer (..), UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.TxCert (pattern UnRegTxCert)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (ShortByteString, pack)
import Data.Default.Class (Default (..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.CostModel (freeV1V2CostModels)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  AlonzoBased (..),
  mkGenesisTxIn,
  mkTxDats,
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
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase)

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(EraCrypto era) (RawSeed 1 1 1 1 1)

someKeysPaymentKeyRole :: forall era. Era era => Proof era -> KeyPairRole era
someKeysPaymentKeyRole pf = KeyPairPayment (someKeys pf)

keysForMultisig :: forall era. Era era => Proof era -> KeyPair 'Witness (EraCrypto era)
keysForMultisig _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 99)

keysForMultisigWitnessKeyRole :: forall era. Era era => Proof era -> KeyPairRole era
keysForMultisigWitnessKeyRole pf = KeyPairWitness (keysForMultisig pf)

keyHashForMultisig :: forall era. Era era => Proof era -> KeyHash 'Witness (EraCrypto era)
keyHashForMultisig pf = hashKey . vKey $ keysForMultisig pf

simpleScript :: forall era. Scriptic era => Proof era -> Script era
simpleScript pf = allOf [require @era (keyHashForMultisig pf)] pf

evenData3ArgsScript :: HasCallStack => Proof era -> Script era
evenData3ArgsScript proof =
  case proof of
    Shelley _ -> error unsupported
    Mary _ -> error unsupported
    Allegra _ -> error unsupported
    Alonzo _ -> evenData3ArgsLang PlutusV1
    Babbage _ -> evenData3ArgsLang PlutusV2
    Conway _ -> evenData3ArgsLang PlutusV2
  where
    unsupported = "Plutus scripts are not supported in:" ++ show proof
    evenData3ArgsLang lang =
      PlutusScript . Plutus lang . BinaryPlutus . SBS.pack $
        concat
          [ [88, 65, 1, 0, 0, 51, 50, 34, 51, 34, 34, 37, 51, 83, 0]
          , [99, 50, 35, 51, 87, 52, 102, 225, 192, 8, 0, 64, 40, 2, 76]
          , [200, 140, 220, 48, 1, 0, 9, 186, 208, 3, 72, 1, 18, 0, 1]
          , [0, 81, 50, 99, 83, 0, 64, 5, 73, 132, 128, 4, 128, 4, 72]
          , [128, 8, 72, 128, 4, 128, 5]
          ]

plainAddr :: forall era. Era era => Proof era -> Addr (EraCrypto era)
plainAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

scriptAddr :: forall era. Scriptic era => Proof era -> Script era -> Addr (EraCrypto era)
scriptAddr _pf s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

malformedScriptAddr :: forall era. EraScript era => Proof era -> Addr (EraCrypto era)
malformedScriptAddr pf = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ malformedScript pf "malfoy"
    (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

simpleScriptAddr :: forall era. Scriptic era => Proof era -> Addr (EraCrypto era)
simpleScriptAddr pf = scriptAddr pf (simpleScript pf)

datumExampleEven :: Era era => Data era
datumExampleEven = Data (PV1.I 2)

datumExampleOdd :: Era era => Data era
datumExampleOdd = Data (PV1.I 3)

validatingRedeemers :: Era era => Redeemers era
validatingRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

-- We intentionally use a ByteString with length greater than 64 to serve as
-- as reminder that our protection against contiguous data over 64 Bytes on
-- the wire is done during deserialization using the Plutus library.
sixtyFiveBytes :: BS.ByteString
sixtyFiveBytes = BS.pack [1 .. 65]

datumExampleSixtyFiveBytes :: Era era => Data era
datumExampleSixtyFiveBytes = Data (PV1.B sixtyFiveBytes)

txDats :: Era era => TxDats era
txDats = mkTxDats datumExampleSixtyFiveBytes

someTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => TxIn c
someTxIn = mkGenesisTxIn 1

anotherTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => TxIn c
anotherTxIn = mkGenesisTxIn 2

yetAnotherTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => TxIn c
yetAnotherTxIn = mkGenesisTxIn 3

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls freeV1V2CostModels
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @7) 0
  , CollateralPercentage 1
  , AdaPerUTxOByte (CoinPerByte (Coin 5))
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs

-- =========================================================================
-- Spend a EUTxO with an inline datum (without and with a failing script)
-- =========================================================================

inlineDatum :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
inlineDatum pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf]
        , RdmrWits validatingRedeemers
        ]
    }

inlineDatumFailingScript :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
inlineDatumFailingScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf]
        , RdmrWits validatingRedeemers
        ]
    }

-- =========================================================================
-- Valid: Use a reference script.
-- =========================================================================

referenceScript :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers txDats)
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
    , otherWitsFields =
        [ DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits validatingRedeemers
        ]
    }

-- =========================================================================
-- Valid: Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScript ::
  forall era.
  (Scriptic era, EraTxBody era) =>
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ RdmrWits validatingRedeemers
        ]
    }

-- =========================================================================
-- Invalid: Spend a EUTxO with an inline datum, using a reference script,
-- and also redundantly supply the script witness.
-- =========================================================================

inlineDatumAndRefScriptWithRedundantWitScript ::
  forall era.
  (Scriptic era, EraTxBody era) =>
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ ScriptWits' [alwaysAlt 3 pf] -- This is redundant with the reference script
        , RdmrWits validatingRedeemers
        ]
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
    , otherWitsFields = []
    }

-- =======================================================================================
-- Valid:  Use a reference input with a data hash in the correspending output and
-- supplying the correspending data witness.
-- =======================================================================================

refInputWithDataHashWithWit ::
  forall era.
  (Scriptic era, EraTxBody era) =>
  Proof era ->
  TestCaseData era
refInputWithDataHashWithWit pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , RefInputs' [anotherTxIn]
          , WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) txDats)
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
    , otherWitsFields = [DataWits' [datumExampleSixtyFiveBytes]]
    }

-- ====================================================================================
-- Valid: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

certRedeemers :: Era era => Redeemers era
certRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (Data (PV1.I 42), ExUnits 5000 5000)

refscriptForDelegCert ::
  forall era.
  ( Scriptic era
  , EraTxBody era
  , ShelleyEraTxCert era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] certRedeemers mempty)
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
    , otherWitsFields = [RdmrWits certRedeemers]
    }

-- ====================================================================================
--  Invalid: Use a collateral output
-- ====================================================================================

useCollateralReturn :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
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
    , otherWitsFields =
        [ ScriptWits' [never 3 pf]
        , DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid: Invalid collateral total
-- ====================================================================================

incorrectCollateralTotal :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid: Inline datum used with redundant datum in witness set
-- ====================================================================================

inlineDatumRedundantDatumWit ::
  forall era.
  (Scriptic era, EraTxBody era) =>
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers txDats)
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
    , otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf]
        , DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid:  Using inline datums with Plutus V1 script
-- ====================================================================================

inlineDatumWithPlutusV1Script :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
inlineDatumWithPlutusV1Script pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers mempty)
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
    , otherWitsFields =
        [ ScriptWits' [always 3 pf]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid:  Using reference script with Plutus V1 script
-- ====================================================================================

referenceScriptWithPlutusV1Script :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
referenceScriptWithPlutusV1Script pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995), RefScript (SJust $ simpleScript pf)]]
          , Txfee (Coin 5)
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
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
    , otherWitsFields =
        [ ScriptWits' [always 3 pf]
        , DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid:  Using reference input with Plutus V1 script
-- ====================================================================================

referenceInputWithPlutusV1Script :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
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
    , otherWitsFields =
        [ ScriptWits' [always 3 pf]
        , DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================
-- Invalid: Malformed plutus reference script creation
-- ====================================================================================

malformedScript :: forall era. Proof era -> ShortByteString -> Script era
malformedScript pf s = case pf of
  Conway {} -> ms
  Babbage {} -> ms
  Alonzo {} -> ms
  x@Shelley {} -> er x
  x@Mary {} -> er x
  x@Allegra {} -> er x
  where
    ms :: AlonzoScript era
    ms = PlutusScript (Plutus PlutusV2 (BinaryPlutus ("nonsense " <> s)))
    er x = error $ "no malformedScript for " <> show x

malformedPlutusRefScript :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
malformedPlutusRefScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Outputs'
              [ newTxOut
                  pf
                  [ Address (plainAddr pf)
                  , Amount (inject $ Coin 5000)
                  , RefScript' [malformedScript pf "rs"]
                  ]
              ]
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 5000)]]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields = []
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
    , otherWitsFields = []
    }

-- ====================================================================================
--  Valid: Unlock Simple Scripts with a Reference Script
-- ====================================================================================

simpleScriptOutWithRefScriptUTxOState :: (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
    , otherWitsFields = []
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
    , otherWitsFields = []
    }

-- ====================================================================================
-- Invalid:  Malformed plutus script witness
-- ====================================================================================

malformedScriptWit :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
malformedScriptWit pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' [anotherTxIn]
          , Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 5000)]]
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (malformedScriptAddr pf)
                  , Amount (inject $ Coin 5000)
                  , FDatum (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole pf]
    , otherWitsFields =
        [ ScriptWits' [malformedScript pf "malfoy"]
        , RdmrWits validatingRedeemers
        ]
    }

-- =============================================================================
-- Invalid:  There is no such thing as a "reference datum".
-- In other words,  you cannot include a reference input that contains an
-- inline datum and have it count for the datum witness where ever it is needed.
-- =============================================================================

noSuchThingAsReferenceDatum :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
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
          , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers (TxDats mempty))
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
    , otherWitsFields =
        [ ScriptWits' [alwaysAlt 3 pf]
        , RdmrWits validatingRedeemers
        ]
    }

-- ====================================================================================

class BabbageBased era failure where
  fromUtxoB :: BabbageUtxoPredFailure era -> failure
  fromUtxowB :: BabbageUtxowPredFailure era -> failure

instance BabbageBased (BabbageEra c) (BabbageUtxowPredFailure (BabbageEra c)) where
  fromUtxoB = UtxoFailure
  fromUtxowB = id

instance BabbageBased (ConwayEra c) (BabbageUtxowPredFailure (ConwayEra c)) where
  fromUtxoB = UtxoFailure
  fromUtxowB = id

type InOut era = (TxIn (EraCrypto era), TxOut era)

data TestCaseData era = TestCaseData
  { txBody :: TxBody era
  , initOutputs :: InitOutputs era
  , keysForAddrWits :: [KeyPairRole era]
  , otherWitsFields :: [WitnessesField era]
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
  = KeyPairPayment (KeyPair 'Payment (EraCrypto era))
  | KeyPairWitness (KeyPair 'Witness (EraCrypto era))
  | KeyPairStakePool (KeyPair 'StakePool (EraCrypto era))
  | KeyPairDRep (KeyPair 'DRepRole (EraCrypto era))
  | KeyPairCommittee (KeyPair 'HotCommitteeRole (EraCrypto era))

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

      newTxIns = fmap (TxIn (txid txBody') . mkTxIx) [0 ..] :: [TxIn (EraCrypto era)]
      newTxInOuts = newTxIns `zip` toList (getOutputs pf txBody')

      initUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ collateral')
      expectedUtxo = UTxO $ Map.fromList (newTxInOuts ++ refInputs' ++ collateral')
   in (initUtxo, expectedUtxo)

txFromTestCaseData ::
  forall era.
  ( Scriptic era
  , GoodCrypto (EraCrypto era)
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  TestCaseData era ->
  Tx era
txFromTestCaseData
  pf
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
        tx =
          newTx
            pf
            ( Body (txBody testCaseData)
                : [ WitnessesI
                      (AddrWits' addrWits : otherWitsFields testCaseData)
                  ]
            )
     in tx

testExpectSuccessValid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , GoodCrypto (EraCrypto era)
  , PostShelley era
  , EraTx era
  , BabbageEraTxBody era
  , EraGov era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessValid
  pf
  tc =
    let txBody' = txBody tc
        tx' = txFromTestCaseData pf tc
        fees = txBody' ^. feeTxBodyL
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc

        newTxIn = TxIn (txid txBody') minBound
        newTxInOut = [newTxIn] `zip` (maybeToList . StrictSeq.lookup 0) (getOutputs pf txBody')

        initUtxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
        expectedUtxo = UTxO $ Map.fromList (newTxInOut ++ refInputs' ++ collateral')
        expectedState = smartUTxOState (pp pf) expectedUtxo (Coin 0) fees def mempty
        assumedValidTx = trustMeP pf True tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedValidTx (Right expectedState)

newColReturn ::
  forall era.
  ( TxBody era ~ BabbageTxBody era
  , BabbageEraTxBody era
  ) =>
  TxBody era ->
  [InOut era]
newColReturn
  txBody' =
    let newColReturnTxIn = TxIn (txid txBody') (mkTxIxPartial 1)
        colReturnOut = case txBody' ^. collateralReturnTxBodyL of
          SNothing -> []
          SJust rOut -> [rOut]
     in [newColReturnTxIn] `zip` colReturnOut

testExpectSuccessInvalid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , TxBody era ~ BabbageTxBody era
  , GoodCrypto (EraCrypto era)
  , PostShelley era
  , EraTx era
  , BabbageEraTxBody era
  , EraGov era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessInvalid
  pf
  tc =
    let txBody' = txBody tc
        tx' = txFromTestCaseData pf tc
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc
        initUtxo = UTxO . Map.fromList $ inputs' ++ refInputs' ++ collateral'
        colBallance = Collateral.collAdaBalance txBody' (Map.fromList collateral')
        expectedUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ newColReturn txBody')
        expectedState = smartUTxOState (pp pf) expectedUtxo (Coin 0) colBallance def mempty
        assumedInvalidTx = trustMeP pf False tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedInvalidTx (Right expectedState)

testExpectFailure ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , GoodCrypto (EraCrypto era)
  , PostShelley era
  , BabbageEraTxBody era
  , EraTx era
  ) =>
  Proof era ->
  TestCaseData era ->
  PredicateFailure (EraRule "UTXOW" era) ->
  Assertion
testExpectFailure
  pf
  tc
  predicateFailure =
    let tx' = txFromTestCaseData pf tc
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc
        utxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
     in testUTXOW (UTXOW pf) utxo (pp pf) (trustMeP pf True tx') (Left [predicateFailure])

genericBabbageFeatures ::
  forall era.
  ( AlonzoBased era (PredicateFailure (EraRule "UTXOW" era))
  , BabbageBased era (PredicateFailure (EraRule "UTXOW" era))
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , TxBody era ~ BabbageTxBody era
  , GoodCrypto (EraCrypto era)
  , BabbageEraTxBody era
  , PostShelley era
  , EraTx era
  , EraGov era
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
        , testCase "reference input with data hash, no data witness" $ testExpectSuccessValid pf (refInputWithDataHashNoWit pf)
        , testCase "reference input with data hash, with data witness" $ testExpectSuccessValid pf (refInputWithDataHashWithWit pf)
        , testCase "reference script to authorize delegation certificate" $ testExpectSuccessValid pf (refscriptForDelegCert pf)
        , testCase "reference script in output" $ testExpectSuccessValid pf (refScriptInOutput pf)
        , testCase "spend simple script output with reference script" $ testExpectSuccessValid pf (simpleScriptOutWithRefScriptUTxOState pf)
        ]
    , testGroup
        "invalid transactions"
        [ testCase "inline datum failing script" $ testExpectSuccessInvalid pf (inlineDatumFailingScript pf)
        , testCase "use a collateral output" $ testExpectSuccessInvalid pf (useCollateralReturn pf)
        , testCase "incorrect collateral total" $
            testExpectFailure
              pf
              (incorrectCollateralTotal pf)
              (fromUtxoB @era (IncorrectTotalCollateralField (Coin 5) (Coin 6)))
        , testCase "inline datum and ref script and redundant script witness" $
            testExpectFailure
              pf
              (inlineDatumAndRefScriptWithRedundantWitScript pf)
              ( fromUtxow @era
                  ( Shelley.ExtraneousScriptWitnessesUTXOW
                      (Set.singleton $ hashScript @era (alwaysAlt 3 pf))
                  )
              )
        , testCase "inline datum with redundant datum witness" $
            testExpectFailure
              pf
              (inlineDatumRedundantDatumWit pf)
              ( fromPredFail @era
                  ( NonOutputSupplimentaryDatums
                      (Set.singleton $ hashData @era datumExampleSixtyFiveBytes)
                      mempty
                  )
              )
        , testCase "inline datum with Plutus V1" $
            testExpectFailure
              pf
              (inlineDatumWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ InlineDatumsNotSupported (TxOutFromInput someTxIn)]
                  )
              )
        , testCase "reference script with Plutus V1" $
            testExpectFailure
              pf
              (referenceScriptWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ ReferenceScriptsNotSupported (TxOutFromOutput (mkTxIxPartial 0))]
                  )
              )
        , testCase "reference input with Plutus V1" $
            testExpectFailure
              pf
              (referenceInputWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ ReferenceInputsNotSupported $ Set.singleton anotherTxIn]
                  )
              )
        , testCase "malformed reference script" $
            testExpectFailure
              pf
              (malformedPlutusRefScript pf)
              ( fromUtxowB @era $
                  MalformedReferenceScripts $
                    Set.singleton
                      (hashScript @era $ malformedScript pf "rs")
              )
        , testCase "malformed script witness" $
            testExpectFailure
              pf
              (malformedScriptWit pf)
              ( fromUtxowB @era $
                  MalformedScriptWitnesses $
                    Set.singleton
                      (hashScript @era $ malformedScript pf "malfoy")
              )
        , testCase "min-utxo value with output too large" $
            testExpectFailure
              pf
              (largeOutput pf)
              (fromUtxoB @era $ BabbageOutputTooSmallUTxO [(largeOutput' pf, Coin 8915)])
        , testCase "no such thing as a reference datum" $
            testExpectFailure
              pf
              (noSuchThingAsReferenceDatum pf)
              ( fromPredFail @era
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
    [ genericBabbageFeatures (Babbage Mock)
    -- genericBabbageFeatures (Conway Mock) TODO
    ]
