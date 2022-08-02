{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.BabbageFeatures where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (Data (..), dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (BadTranslation))
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxosPredFailure (CollectErrors),
    AlonzoUtxowPredFailure (NonOutputSupplimentaryDatums),
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (PlutusScript), CostModels (..), ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( TranslationError (InlineDatumsNotSupported, ReferenceInputsNotSupported, ReferenceScriptsNotSupported),
    TxOutSource (TxOutFromInput, TxOutFromOutput),
  )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..))
import qualified Cardano.Ledger.Babbage.Collateral as Collateral (collBalance)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxBody
  ( AlonzoEraTxBody (..),
    BabbageEraTxBody (..),
    BabbageTxBody (..),
    BabbageTxOut (..),
    Datum (..),
  )
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    mkTxIxPartial,
  )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
  ( KeyHash,
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (DCert (DCertDeleg), DelegCert (DeRegKey), ProtVer (..), UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject)
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (ShortByteString, pack)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( AlonzoBased (..),
    freeCostModelV1,
    freeCostModelV2,
    keyBy,
    mkGenesisTxIn,
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
import Test.Cardano.Ledger.Generic.Functions
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase)

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (Crypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 1 1 1 1 1)

someKeysPaymentKeyRole :: forall era. Era era => Proof era -> KeyPairRole era
someKeysPaymentKeyRole pf = KeyPairPayment (someKeys pf)

keysForMultisig :: forall era. Era era => Proof era -> KeyPair 'Witness (Crypto era)
keysForMultisig _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 99)

keysForMultisigWitnessKeyRole :: forall era. Era era => Proof era -> KeyPairRole era
keysForMultisigWitnessKeyRole pf = KeyPairWitness (keysForMultisig pf)

keyHashForMultisig :: forall era. Era era => Proof era -> KeyHash 'Witness (Crypto era)
keyHashForMultisig pf = hashKey . vKey $ keysForMultisig pf

simpleScript :: forall era. (Scriptic era) => Proof era -> Script era
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
      PlutusScript lang . SBS.pack $
        concat
          [ [88, 65, 1, 0, 0, 51, 50, 34, 51, 34, 34, 37, 51, 83, 0],
            [99, 50, 35, 51, 87, 52, 102, 225, 192, 8, 0, 64, 40, 2, 76],
            [200, 140, 220, 48, 1, 0, 9, 186, 208, 3, 72, 1, 18, 0, 1],
            [0, 81, 50, 99, 83, 0, 64, 5, 73, 132, 128, 4, 128, 4, 72],
            [128, 8, 72, 128, 4, 128, 5]
          ]

plainAddr :: forall era. Era era => Proof era -> Addr (Crypto era)
plainAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

scriptAddr :: forall era. (Scriptic era) => Proof era -> Script era -> Addr (Crypto era)
scriptAddr _pf s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

malformedScriptAddr :: forall era. EraScript era => Proof era -> Addr (Crypto era)
malformedScriptAddr pf = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ malformedScript pf "malfoy"
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

simpleScriptAddr :: forall era. (Scriptic era) => Proof era -> Addr (Crypto era)
simpleScriptAddr pf = scriptAddr pf (simpleScript pf)

datumExampleEven :: Era era => Data era
datumExampleEven = Data (Plutus.I 2)

datumExampleOdd :: Era era => Data era
datumExampleOdd = Data (Plutus.I 3)

validatingRedeemers :: Era era => Redeemers era
validatingRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (Data (Plutus.I 42), ExUnits 5000 5000)

-- We intentionally use a ByteString with length greater than 64 to serve as
-- as reminder that our protection against contiguous data over 64 Bytes on
-- the wire is done during deserialization using the Plutus library.
sixtyFiveBytes :: BS.ByteString
sixtyFiveBytes = BS.pack [1 .. 65]

datumExampleSixtyFiveBytes :: Era era => Data era
datumExampleSixtyFiveBytes = Data (Plutus.B sixtyFiveBytes)

txDats :: Era era => TxDats era
txDats = TxDats $ keyBy hashData [datumExampleSixtyFiveBytes]

someTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
someTxIn = mkGenesisTxIn 1

anotherTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
anotherTxIn = mkGenesisTxIn 2

yetAnotherTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
yetAnotherTxIn = mkGenesisTxIn 3

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

pp :: Proof era -> PParams era
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf],
          RdmrWits validatingRedeemers
        ]
    }

inlineDatumFailingScript :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
inlineDatumFailingScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleOdd @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Collateral' [yetAnotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers txDats)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (alwaysAlt 3 pf)),
                    Amount (inject $ Coin 5000),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ alwaysAlt 3 pf)
                  ]
              ],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Collateral' [yetAnotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ evenData3ArgsScript pf)
                  ]
              ],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Collateral' [yetAnotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ evenData3ArgsScript pf)
                  ]
              ],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [alwaysAlt 3 pf], -- This is redundant with the reference script
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]],
            Txfee (Coin 5)
          ],
      initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 10),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = []
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) txDats),
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]],
            Txfee (Coin 5)
          ],
      initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 10),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = [DataWits' [datumExampleSixtyFiveBytes]]
    }

-- ====================================================================================
-- Valid: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

certRedeemers :: Era era => Redeemers era
certRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (Data (Plutus.I 42), ExUnits 5000 5000)

refscriptForDelegCert :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
refscriptForDelegCert pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Collateral' [yetAnotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]],
            Certs' [DCertDeleg (DeRegKey (ScriptHashObj (hashScript @era $ alwaysAlt 2 pf)))],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] certRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ alwaysAlt 2 pf)
                  ]
              ],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = [RdmrWits certRedeemers]
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            CollateralReturn' [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2110)]],
            TotalCol (SJust $ Coin 5),
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (never 3 pf)),
                    Amount (inject $ Coin 5000),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [never 3 pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            CollateralReturn' [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2110)]],
            TotalCol (SJust $ Coin 6),
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers txDats)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (evenData3ArgsScript pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleEven @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf)),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [always 3 pf],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995), RefScript (SJust $ simpleScript pf)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf)),
                    Amount (inject $ Coin 5000),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [always 3 pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemers
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Collateral' [yetAnotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5),
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers txDats)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf)),
                    Amount (inject $ Coin 5000),
                    DHash' [hashData $ datumExampleSixtyFiveBytes @era]
                  ]
              ],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (scriptAddr pf (always 3 pf)),
                    Amount (inject $ Coin 5000)
                  ]
              ],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [always 3 pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemers
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
    ms = PlutusScript PlutusV2 $ "nonsense " <> s
    er x = error $ "no malformedScript for " <> show x

malformedPlutusRefScript :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
malformedPlutusRefScript pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn],
            Outputs'
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript' [malformedScript pf "rs"]
                  ]
              ]
          ],
      initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 5000)]],
            ofRefInputs = [],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = []
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
          [ Inputs' [someTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ simpleScript pf)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = []
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
          [ Inputs' [someTxIn],
            RefInputs' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]],
            Txfee (Coin 5)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (simpleScriptAddr pf),
                    Amount (inject $ Coin 5000)
                  ]
              ],
            ofRefInputs =
              [ newTxOut
                  pf
                  [ Address (plainAddr pf),
                    Amount (inject $ Coin 5000),
                    RefScript (SJust $ simpleScript pf)
                  ]
              ],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf, keysForMultisigWitnessKeyRole pf],
      otherWitsFields = []
    }

-- ========================================================================================
-- Invalid: TxOut too large for the included ADA, using a large inline datum
-- ========================================================================================

largeDatum :: Era era => Data era
largeDatum = Data (Plutus.B . BS.pack $ replicate 1500 0)

largeOutput' :: forall era. (Scriptic era, EraTxOut era) => Proof era -> TxOut era
largeOutput' pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 1135),
      FDatum . Datum . dataToBinaryData $ largeDatum @era
    ]

largeOutput :: forall era. (BabbageEraTxBody era, Scriptic era) => Proof era -> TestCaseData era
largeOutput pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn],
            Outputs' [largeOutput' pf],
            Txfee (Coin 5)
          ],
      initOutputs =
        InitOutputs
          { ofInputs = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]],
            ofRefInputs = [],
            ofCollateral = []
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields = []
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
          [ Inputs' [someTxIn],
            Collateral' [anotherTxIn],
            Outputs' [newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 5000)]],
            WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemers mempty)
          ],
      initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (malformedScriptAddr pf),
                    Amount (inject $ Coin 5000),
                    FDatum (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  ]
              ],
            ofRefInputs = [],
            ofCollateral = [newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]]
          },
      keysForAddrWits = [someKeysPaymentKeyRole pf],
      otherWitsFields =
        [ ScriptWits' [malformedScript pf "malfoy"],
          RdmrWits validatingRedeemers
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

type InOut era = (TxIn (Crypto era), TxOut era)

data TestCaseData era = TestCaseData
  { txBody :: TxBody era,
    initOutputs :: InitOutputs era,
    keysForAddrWits :: [KeyPairRole era],
    otherWitsFields :: [WitnessesField era]
  }

data InitOutputs era = InitOutputs
  { ofInputs :: [TxOut era],
    ofRefInputs :: [TxOut era],
    ofCollateral :: [TxOut era]
  }

data InitUtxo era = InitUtxo
  { inputs :: [InOut era],
    refInputs :: [InOut era],
    collateral :: [InOut era]
  }

data KeyPairRole era
  = KeyPairPayment (KeyPair 'Payment (Crypto era))
  | KeyPairWitness (KeyPair 'Witness (Crypto era))

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

txFromTestCaseData ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era),
    BabbageEraTxBody era
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
                KeyPairPayment p -> makeWitnessVKey (hashAnnotated (txBody testCaseData)) p
                KeyPairWitness w -> makeWitnessVKey (hashAnnotated (txBody testCaseData)) w
            )
            (keysForAddrWits testCaseData)
        tx =
          newTx
            pf
            ( Body (txBody testCaseData) :
              [ WitnessesI
                  (AddrWits' addrWits : otherWitsFields testCaseData)
              ]
            )
     in tx

testExpectSuccessValid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraTx era,
    BabbageEraTxBody era
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
        expectedState = smartUTxOState expectedUtxo (Coin 0) fees def
        assumedValidTx = trustMeP pf True tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedValidTx (Right expectedState)

newColReturn ::
  forall era.
  ( TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era,
    BabbageEraTxBody era
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
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraTx era,
    BabbageEraTxBody era
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

        initUtxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
        colBallance = Val.coin $ Collateral.collBalance txBody' initUtxo
        expectedUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ newColReturn txBody')
        expectedState = smartUTxOState expectedUtxo (Coin 0) colBallance def
        assumedInvalidTx = trustMeP pf False tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedInvalidTx (Right expectedState)

testExpectFailure ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    BabbageEraTxBody era,
    EraTx era
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
  ( AlonzoBased era (PredicateFailure (EraRule "UTXOW" era)),
    BabbageBased era (PredicateFailure (EraRule "UTXOW" era)),
    State (EraRule "UTXOW" era) ~ UTxOState era,
    TxBody era ~ BabbageTxBody era,
    TxOut era ~ BabbageTxOut era,
    GoodCrypto (Crypto era),
    BabbageEraTxBody era,
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraTx era
  ) =>
  Proof era ->
  TestTree
genericBabbageFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $ testExpectSuccessValid pf (inlineDatum pf),
          testCase "reference script" $ testExpectSuccessValid pf (referenceScript pf),
          testCase "inline datum and ref script" $ testExpectSuccessValid pf (inlineDatumAndRefScript pf),
          testCase "reference input with data hash, no data witness" $ testExpectSuccessValid pf (refInputWithDataHashNoWit pf),
          testCase "reference input with data hash, with data witness" $ testExpectSuccessValid pf (refInputWithDataHashWithWit pf),
          testCase "reference script to authorize delegation certificate" $ testExpectSuccessValid pf (refscriptForDelegCert pf),
          testCase "reference script in output" $ testExpectSuccessValid pf (refScriptInOutput pf),
          testCase "spend simple script output with reference script" $ testExpectSuccessValid pf (simpleScriptOutWithRefScriptUTxOState pf)
        ],
      testGroup
        "invalid transactions"
        [ testCase "inline datum failing script" $ testExpectSuccessInvalid pf (inlineDatumFailingScript pf),
          testCase "use a collateral output" $ testExpectSuccessInvalid pf (useCollateralReturn pf),
          testCase "incorrect collateral total" $
            testExpectFailure
              pf
              (incorrectCollateralTotal pf)
              (fromUtxoB @era (IncorrectTotalCollateralField (Coin 5) (Coin 6))),
          testCase "inline datum and ref script and redundant script witness" $
            testExpectFailure
              pf
              (inlineDatumAndRefScriptWithRedundantWitScript pf)
              ( fromUtxow @era
                  ( Shelley.ExtraneousScriptWitnessesUTXOW
                      (Set.singleton $ hashScript @era (alwaysAlt 3 pf))
                  )
              ),
          testCase "inline datum with redundant datum witness" $
            testExpectFailure
              pf
              (inlineDatumRedundantDatumWit pf)
              ( fromPredFail @era
                  ( NonOutputSupplimentaryDatums
                      (Set.singleton $ hashData @era datumExampleSixtyFiveBytes)
                      mempty
                  )
              ),
          testCase "inline datum with Plutus V1" $
            testExpectFailure
              pf
              (inlineDatumWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ InlineDatumsNotSupported (TxOutFromInput someTxIn)]
                  )
              ),
          testCase "reference script with Plutus V1" $
            testExpectFailure
              pf
              (referenceScriptWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ ReferenceScriptsNotSupported (TxOutFromOutput (mkTxIxPartial 0))]
                  )
              ),
          testCase "reference input with Plutus V1" $
            testExpectFailure
              pf
              (referenceInputWithPlutusV1Script pf)
              ( fromUtxos @era
                  ( CollectErrors
                      [BadTranslation $ ReferenceInputsNotSupported $ Set.singleton anotherTxIn]
                  )
              ),
          testCase "malformed reference script" $
            testExpectFailure
              pf
              (malformedPlutusRefScript pf)
              ( fromUtxowB @era $
                  MalformedReferenceScripts $
                    Set.singleton
                      (hashScript @era $ malformedScript pf "rs")
              ),
          testCase "malformed script witness" $
            testExpectFailure
              pf
              (malformedScriptWit pf)
              ( fromUtxowB @era $
                  MalformedScriptWitnesses $
                    Set.singleton
                      (hashScript @era $ malformedScript pf "malfoy")
              ),
          testCase "min-utxo value with output too large" $
            testExpectFailure
              pf
              (largeOutput pf)
              (fromUtxoB @era $ BabbageOutputTooSmallUTxO [(largeOutput' pf, Coin 8915)])
        ]
    ]

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures (Babbage Mock),
      genericBabbageFeatures (Conway Mock)
    ]
