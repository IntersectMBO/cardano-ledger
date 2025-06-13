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
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
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
  ShelleyBase,
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
 )
import Cardano.Ledger.Binary (DecShareCBOR (..), Interns)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import qualified Cardano.Ledger.Conway.Rules as Conway (ConwayUtxoPredFailure (..))
import Cardano.Ledger.Conway.State (EraCertState (..), EraStake (..))
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..))
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
import Cardano.Ledger.Shelley.Scripts (pattern RequireAllOf, pattern RequireSignature)
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
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessVKey)
import Test.Cardano.Ledger.Examples.AlonzoAPI (defaultPParams)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  genericCont,
  mkGenesisTxIn,
  mkTxDats,
 )
import Test.Cardano.Ledger.Generic.GenState (
  mkRedeemers,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair, mkKeyPair')
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

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

simpleScript :: forall era. Era era => Script era
simpleScript = fromNativeScript $ RequireAllOf [RequireSignature keyHashForMultisig]

evenData3ArgsScript ::
  forall era.
  ( EraGov era
  , Share (TxOut era) ~ Interns (Credential Staking)
  , Share (InstantStake era) ~ Interns (Credential Staking)
  , Share (CertState era)
      ~ ( Interns (Credential Staking)
        , Interns (KeyHash StakePool)
        , Interns (Credential DRepRole)
        , Interns (Credential HotCommitteeRole)
        )
  ) =>
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
        concat
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

simpleScriptAddr :: forall era. Reflect era => Addr
simpleScriptAddr = scriptAddr @era simpleScript

datumExampleEven :: Era era => Data era
datumExampleEven = Data (PV1.I 2)

datumExampleOdd :: Era era => Data era
datumExampleOdd = Data (PV1.I 3)

validatingRedeemers :: Era era => Redeemers era
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

inlineDatum :: forall era. Reflect era => TestCaseData era
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
          & witsTxL . scriptTxWitsL .~ [(undefined, evenData3ArgsScript @era)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

inlineDatumFailingScript :: forall era. Reflect era => TestCaseData era
inlineDatumFailingScript =
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
                  & datumTxOutL .~ (Datum . dataToBinaryData $ datumExampleOdd @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, evenData3ArgsScript)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- =========================================================================
-- Valid: Use a reference script.
-- =========================================================================

referenceScript ::
  forall era.
  Reflect era =>
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
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes @era)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

commonReferenceScript :: forall era. Reflect era => TestCaseData era
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
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- =========================================================================
-- Valid: Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScript ::
  forall era.
  Reflect era =>
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

-- =========================================================================
-- Invalid: Spend a EUTxO with an inline datum, using a reference script,
-- and also redundantly supply the script witness.
-- =========================================================================

inlineDatumAndRefScriptWithRedundantWitScript ::
  forall era.
  Reflect era =>
  TestCaseData era
inlineDatumAndRefScriptWithRedundantWitScript =
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
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysSucceeds @PlutusV2 3)] -- This is redundant with the reference script
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Valid: Use a reference input with a data hash in the correspending output and
-- without supplying the correspending data witness.
-- ====================================================================================

refInputWithDataHashNoWit ::
  forall era.
  EraTxBody era =>
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
    , otherWitsFields = []
    }

-- =======================================================================================
-- Valid:  Use a reference input with a data hash in the correspending output and
-- supplying the correspending data witness.
-- =======================================================================================

refInputWithDataHashWithWit ::
  forall era.
  Reflect era =>
  TestCaseData era
refInputWithDataHashWithWit =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & scriptIntegrityHashTxBodyL
            .~ (newScriptIntegrityHash @era defaultPParams [] (mkRedeemers []) txDats)
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 1135)]
          & feeTxBodyL .~ (Coin 5)
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
    , otherWitsFields = witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
    }

-- ====================================================================================
-- Valid: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

certRedeemers :: Era era => Redeemers era
certRedeemers =
  mkRedeemers [(CertifyingPurpose @_ @AsIx 0, (Data (PV1.I 42), ExUnits 5000 5000))]

refscriptForDelegCert ::
  forall era.
  ( EraTxBody era
  , ShelleyEraTxCert era
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
          & feeTxBodyL .~ (Coin 5)
          & scriptIntegrityHashTxBodyL
            .~ (newScriptIntegrityHash @era defaultPParams [PlutusV2] certRedeemers mempty)
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
--  Invalid: Use a collateral output
-- ====================================================================================

useCollateralReturn :: forall era. Reflect era => TestCaseData era
useCollateralReturn =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & collateralReturnTxBodyL .~ SJust (mkBasicTxOut plainAddr (inject $ Coin 2110))
          & totalCollateralTxBodyL .~ (SJust $ Coin 5)
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ (Coin 5)
          & scriptIntegrityHashTxBodyL
            .~ (newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemers txDats)
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut (scriptAddr @era (alwaysFails @PlutusV1 3)) (inject $ Coin 5000)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysFails @PlutusV1 3)]
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Invalid: Invalid collateral total
-- ====================================================================================

incorrectCollateralTotal :: forall era. Reflect era => TestCaseData era
incorrectCollateralTotal =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & collateralReturnTxBodyL .~ SJust (mkBasicTxOut plainAddr (inject $ Coin 2110))
          & totalCollateralTxBodyL .~ SJust (Coin 6)
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
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, evenData3ArgsScript)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Invalid: Inline datum used with redundant datum in witness set
-- ====================================================================================

inlineDatumRedundantDatumWit ::
  forall era.
  Reflect era =>
  TestCaseData era
inlineDatumRedundantDatumWit =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ (Coin 5)
          & scriptIntegrityHashTxBodyL
            .~ (newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers txDats)
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (scriptAddr @era evenData3ArgsScript)
                  (inject $ Coin 5000)
                  & datumTxOutL .~ (Datum . dataToBinaryData $ datumExampleEven @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, evenData3ArgsScript)]
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Invalid:  Using inline datums with Plutus V1 script
-- ====================================================================================

inlineDatumWithPlutusV1Script ::
  forall era.
  Reflect era =>
  TestCaseData era
inlineDatumWithPlutusV1Script =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemers mempty
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (scriptAddr @era $ alwaysSucceeds @PlutusV1 3)
                  (inject $ Coin 5000)
                  & datumTxOutL .~ (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysSucceeds @PlutusV1 3)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Invalid:  Using reference script with Plutus V1 script
-- ====================================================================================

referenceScriptWithPlutusV1Script ::
  forall era. Reflect era => TestCaseData era
referenceScriptWithPlutusV1Script =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & collateralInputsTxBodyL .~ [anotherTxIn]
          & outputsTxBodyL
            .~ [ mkBasicTxOut plainAddr (inject $ Coin 4995)
                   & referenceScriptTxOutL .~ SJust simpleScript
               ]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemers txDats
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (scriptAddr @era $ alwaysSucceeds @PlutusV1 3)
                  (inject $ Coin 5000)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofRefInputs = []
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysSucceeds @PlutusV1 3)]
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
-- Invalid:  Using reference input with Plutus V1 script
-- ====================================================================================

referenceInputWithPlutusV1Script ::
  forall era. Reflect era => TestCaseData era
referenceInputWithPlutusV1Script =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn]
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ (Coin 5)
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemers txDats
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (scriptAddr @era $ alwaysSucceeds @PlutusV1 3)
                  (inject $ Coin 5000)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofRefInputs =
              [ mkBasicTxOut
                  (scriptAddr @era $ alwaysSucceeds @PlutusV1 3)
                  (inject $ Coin 5000)
              ]
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysSucceeds @PlutusV1 3)]
          & witsTxL . datsTxWitsL .~ TxDats [(undefined, datumExampleSixtyFiveBytes)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
    }

-- ====================================================================================
--  Valid: Don't run reference scripts in output for validation
-- ====================================================================================

refScriptInOutput :: forall era. EraTxBody era => TestCaseData era
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
    , otherWitsFields = []
    }

-- ====================================================================================
--  Valid: Unlock Simple Scripts with a Reference Script
-- ====================================================================================

simpleScriptOutWithRefScriptUTxOState ::
  forall era.
  Reflect era => TestCaseData era
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
    , otherWitsFields = []
    }

-- ========================================================================================
-- Invalid: TxOut too large for the included ADA, using a large inline datum
-- ========================================================================================

largeDatum :: Era era => Data era
largeDatum = Data (PV1.B . BS.pack $ replicate 1500 0)

largeOutput' :: forall era. EraTxOut era => TxOut era
largeOutput' =
  mkBasicTxOut
    plainAddr
    (inject $ Coin 1135)
    & datumTxOutL .~ Datum (dataToBinaryData $ largeDatum @era)

largeOutput :: forall era. BabbageEraTxBody era => TestCaseData era
largeOutput =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & outputsTxBodyL .~ [largeOutput']
          & feeTxBodyL .~ Coin 5
    , initOutputs =
        InitOutputs
          { ofInputs = [mkBasicTxOut plainAddr (inject $ Coin 1140)]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = []
    }

-- =============================================================================
-- Invalid:  There is no such thing as a "reference datum".
-- In other words,  you cannot include a reference input that contains an
-- inline datum and have it count for the datum witness where ever it is needed.
-- =============================================================================

noSuchThingAsReferenceDatum ::
  forall era. Reflect era => TestCaseData era
noSuchThingAsReferenceDatum =
  TestCaseData
    { txBody =
        mkBasicTxBody
          & inputsTxBodyL .~ [someTxIn]
          & referenceInputsTxBodyL .~ [anotherTxIn] -- Note that this reference input has the required datum
          & collateralInputsTxBodyL .~ [yetAnotherTxIn]
          & outputsTxBodyL .~ [mkBasicTxOut plainAddr (inject $ Coin 4995)]
          & feeTxBodyL .~ Coin 5
          & scriptIntegrityHashTxBodyL
            .~ newScriptIntegrityHash @era defaultPParams [PlutusV2] validatingRedeemers (TxDats mempty)
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ mkBasicTxOut
                  (scriptAddr @era $ alwaysSucceeds @PlutusV2 3)
                  (inject $ Coin 5000)
                  & dataHashTxOutL .~ SJust (hashData $ datumExampleSixtyFiveBytes @era)
              ]
          , ofRefInputs =
              [ mkBasicTxOut
                  plainAddr
                  (inject $ Coin 5000)
                  & datumTxOutL .~ (Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
                  -- Note that this inline datum does not witness the datum for the plutus script
              ]
          , ofCollateral = [mkBasicTxOut plainAddr (inject $ Coin 2115)]
          }
    , keysForAddrWits = [someKeysPaymentKeyRole]
    , otherWitsFields = \x ->
        x
          & witsTxL . scriptTxWitsL .~ [(undefined, alwaysSucceeds @PlutusV2 1)]
          & witsTxL . rdmrsTxWitsL .~ validatingRedeemers
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
  Proof era ->
  TestCaseData era ->
  (UTxO era, UTxO era)
utxoFromTestCaseData pf (TestCaseData txBody' (InitOutputs ofInputs' ofRefInputs' ofCollateral') _ _) =
  let inputsIns = txBody' ^. inputsTxBodyL
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
  BabbageEraTxBody era =>
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
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Tx era ~ Signal (EraRule "UTXOW" era)
  , Reflect era
  , BabbageEraTxBody era
  , Era era
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
        tx' = txFromTestCaseData pf tc
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc
        initUtxo = UTxO . Map.fromList $ inputs' ++ refInputs' ++ collateral'
        DeltaCoin colBallance = Collateral.collAdaBalance txBody' (Map.fromList collateral')
        expectedUtxo = UTxO $ Map.fromList (inputs' ++ refInputs' ++ newColReturn txBody')
        expectedState = smartUTxOState defaultPParams expectedUtxo (Coin 0) (Coin colBallance) def mempty
        assumedInvalidTx = tx' & isValidTxL .~ IsValid False
     in testUTXOW initUtxo defaultPParams assumedInvalidTx (Right expectedState)

testExpectFailure ::
  forall era.
  ( BabbageEraTxBody era
  , Reflect era
  ) =>
  TestCaseData era ->
  PredicateFailure (EraRule "UTXOW" era) ->
  Assertion
testExpectFailure tc predicateFailure =
  let tx' = txFromTestCaseData tc
      (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData tc
      utxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
   in testUTXOW
        UTXOW
        utxo
        defaultPParams
        (tx' & isValidTxL .~ IsValid True)
        (Left $ pure predicateFailure)

genericBabbageFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , BabbageEraTxBody era
  , Reflect era
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
  ( BabbageEraTxBody era
  , Reflect era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure era
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
  , Reflect era
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
                  (Shelley.ExtraneousScriptWitnessesUTXOW (Set.singleton $ hashScript @era (alwaysSucceeds 1)))
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
  ( Reflect era
  , BabbageEraTxBody era
  ) =>
  Proof era ->
  TestCaseData era ->
  PredicateFailure (EraRule "UTXO" era) ->
  Assertion
testExpectUTXOFailure pf@Conway tc failure =
  let tx' = txFromTestCaseData pf tc
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
            Left _ -> assertFailure "not exactly one failure"
            Right _ -> assertFailure "testExpectUTXOFailure succeeds"
        )
testExpectUTXOFailure _ _ _ = error "testExpectUTXOFailure is only good in Conway Era"
