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
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
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
import Cardano.Ledger.Shelley.API (UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
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
  TxField (..),
  TxOutField (..),
  WitnessesField (..),
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
        concat
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

inlineDatum :: forall era. (Scriptic era, Reflect era) => Proof era -> TestCaseData era
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
    , otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf]
        , RdmrWits $ validatingRedeemers pf
        ]
    }

-- =========================================================================
-- Valid: Use a reference script.
-- =========================================================================

referenceScript :: forall era. (Scriptic era, Reflect era) => Proof era -> TestCaseData era
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
    , otherWitsFields =
        [ DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits $ validatingRedeemers pf
        ]
    }

commonReferenceScript :: forall era. (Scriptic era, Reflect era) => Proof era -> TestCaseData era
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
    , otherWitsFields =
        [ DataWits' [datumExampleSixtyFiveBytes]
        , RdmrWits $ validatingRedeemers pf
        ]
    }

-- =========================================================================
-- Valid: Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScript ::
  forall era.
  (Scriptic era, Reflect era) =>
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
    , otherWitsFields =
        [ RdmrWits $ validatingRedeemers pf
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
  (Scriptic era, Reflect era) =>
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
    , otherWitsFields = [DataWits' [datumExampleSixtyFiveBytes]]
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
    , otherWitsFields = [RdmrWits $ certRedeemers pf]
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
    , otherWitsFields = []
    }

-- ====================================================================================

type InOut era = (TxIn, TxOut era)

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
  ( Scriptic era
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
  , PostShelley era
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
        tx' = txFromTestCaseData pf tc
        fees = txBody' ^. feeTxBodyL
        (InitUtxo inputs' refInputs' collateral') = initUtxoFromTestCaseData pf tc

        newTxIn = TxIn (txIdTxBody txBody') minBound
        newTxInOut = [newTxIn] `zip` (maybeToList . StrictSeq.lookup 0) (getOutputs pf txBody')

        initUtxo = (UTxO . Map.fromList) $ inputs' ++ refInputs' ++ collateral'
        expectedUtxo = UTxO $ Map.fromList (newTxInOut ++ refInputs' ++ collateral')
        expectedState = smartUTxOState (pp pf) expectedUtxo (Coin 0) fees def mempty
        assumedValidTx = trustMeP pf True tx'
     in testUTXOW (UTXOW pf) initUtxo (pp pf) assumedValidTx (Right expectedState)

genericBabbageFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , BabbageEraTxBody era
  , PostShelley era
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

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures Babbage
    , genericBabbageFeatures Conway
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
