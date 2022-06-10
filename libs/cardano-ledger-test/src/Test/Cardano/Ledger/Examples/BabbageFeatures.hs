{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..), Script (PlutusScript))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.TxInfo (TranslationError (..), TxOutSource (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (..))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    mkTxIxPartial,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraRule)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (..), ValidateScript (hashScript))
import Cardano.Ledger.Keys
  ( KeyHash,
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (ProtVer (..), UTxO (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), smartUTxOState)
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..))
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (ShortByteString, pack)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Stack
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( AlonzoBased (..),
    Expect (..),
    expectedUTxO,
    freeCostModelV1,
    freeCostModelV2,
    keyBy,
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
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- =======================
-- Setup the initial state
-- =======================

scriptAddr :: forall era. (Scriptic era) => Proof era -> Core.Script era -> Addr (Crypto era)
scriptAddr _pf s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

malformedScriptAddr :: forall era. (ValidateScript era) => Proof era -> Addr (Crypto era)
malformedScriptAddr pf = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ malformedScript pf "malfoy"
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (Crypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 1 1 1 1 1)

plainAddr :: forall era. Era era => Proof era -> Addr (Crypto era)
plainAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

somePlainOutput :: Scriptic era => Proof era -> Core.TxOut era
somePlainOutput pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]

somePlainOutput2 :: Scriptic era => Proof era -> Core.TxOut era
somePlainOutput2 pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 5000)]

mkGenesisTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => Integer -> TxIn crypto
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

collateralOutput :: Scriptic era => Proof era -> Core.TxOut era
collateralOutput pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]

-- We intentionally use a ByteString with length greater than 64 to serve as
-- as reminder that our protection against contiguous data over 64 Bytes on
-- the wire is done during deserialization using the Plutus library.
sixtyFiveBytes :: BS.ByteString
sixtyFiveBytes = BS.pack [1 .. 65]

datumExampleSixtyFiveBytes :: Data era
datumExampleSixtyFiveBytes = Data (Plutus.B sixtyFiveBytes)

datumExampleEven :: Data era
datumExampleEven = Data (Plutus.I 2)

datumExampleOdd :: Data era
datumExampleOdd = Data (Plutus.I 3)

inlineDatumOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
inlineDatumOutput pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (evenData3ArgsScript pf)),
      Amount (inject $ Coin 5000),
      Datum (Babbage.Datum . dataToBinaryData $ datumExampleEven @era)
    ]

inlineDatumOutputFailingScript :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
inlineDatumOutputFailingScript pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (evenData3ArgsScript pf)),
      Amount (inject $ Coin 5000),
      Datum (Babbage.Datum . dataToBinaryData $ datumExampleOdd @era)
    ]

inlineDatumOutputV1 :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
inlineDatumOutputV1 pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (always 3 pf)),
      Amount (inject $ Coin 5000),
      Datum (Babbage.Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
    ]

simpleV2EUTxO :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
simpleV2EUTxO pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (alwaysAlt 3 pf)),
      Amount (inject $ Coin 5000),
      DHash' [hashData $ datumExampleSixtyFiveBytes @era]
    ]

failsEUTxO :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
failsEUTxO pf =
  newTxOut
    pf
    [ Address (scriptAddr pf (never 3 pf)),
      Amount (inject $ Coin 5000),
      DHash' [hashData $ datumExampleSixtyFiveBytes @era]
    ]

referenceScriptOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceScriptOutput pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 5000),
      RefScript (SJust $ evenData3ArgsScript pf)
    ]

referenceScriptOutput2 :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceScriptOutput2 pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 5000),
      RefScript (SJust $ alwaysAlt 2 pf)
    ]

referenceScriptOutput4 :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceScriptOutput4 pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 5000),
      RefScript (SJust $ alwaysAlt 3 pf)
    ]

keysForMultisig :: forall era. Era era => Proof era -> KeyPair 'Witness (Crypto era)
keysForMultisig _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 99)

keyHashForMultisig :: forall era. Era era => Proof era -> KeyHash 'Witness (Crypto era)
keyHashForMultisig pf = hashKey . vKey $ keysForMultisig pf

simpleScript :: forall era. (Scriptic era) => Proof era -> Core.Script era
simpleScript pf = allOf [require @era (keyHashForMultisig pf)] pf

referenceSimpleScriptOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceSimpleScriptOutput pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 5000),
      RefScript (SJust $ simpleScript pf)
    ]

simpleScriptAddr :: forall era. (Scriptic era) => Proof era -> Addr (Crypto era)
simpleScriptAddr pf = scriptAddr pf (simpleScript pf)

simpleScriptLockedOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
simpleScriptLockedOutput pf =
  newTxOut
    pf
    [ Address (simpleScriptAddr pf),
      Amount (inject $ Coin 5000)
    ]

referenceDataHashOutput :: forall era. (Scriptic era) => Proof era -> Core.TxOut era
referenceDataHashOutput pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 10),
      DHash' [hashData $ datumExampleSixtyFiveBytes @era]
    ]

malformedScriptTxOut :: forall era. (ValidateScript era) => Proof era -> Core.TxOut era
malformedScriptTxOut pf =
  newTxOut
    pf
    [ Address (malformedScriptAddr pf),
      Amount (inject $ Coin 5000),
      Datum (Babbage.Datum . dataToBinaryData $ datumExampleSixtyFiveBytes @era)
    ]

--
-- Genesis Inputs
--

inlineDatumInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
inlineDatumInput = mkGenesisTxIn 1

referenceScriptInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
referenceScriptInput = mkGenesisTxIn 2

referenceDataHashInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
referenceDataHashInput = mkGenesisTxIn 3

somePlainInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
somePlainInput = mkGenesisTxIn 4

simpleV2EUTxOInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
simpleV2EUTxOInput = mkGenesisTxIn 5

referenceScriptInput2 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
referenceScriptInput2 = mkGenesisTxIn 6

failsEUTxOInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
failsEUTxOInput = mkGenesisTxIn 7

inlineDatumInputV1 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
inlineDatumInputV1 = mkGenesisTxIn 8

referenceScriptInput4 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
referenceScriptInput4 = mkGenesisTxIn 9

inlineDatumInputOdd :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
inlineDatumInputOdd = mkGenesisTxIn 10

collateralInput11 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
collateralInput11 = mkGenesisTxIn 11

referenceSimpleScriptInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
referenceSimpleScriptInput = mkGenesisTxIn 12

collateralInput17 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
collateralInput17 = mkGenesisTxIn 17

somePlainInput2 :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
somePlainInput2 = mkGenesisTxIn 18

malformedScriptTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
malformedScriptTxIn = mkGenesisTxIn 19

simpleScriptLockedInput :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => TxIn crypto
simpleScriptLockedInput = mkGenesisTxIn 20

--
-- Genesis UTxO
--

initUTxO :: PostShelley era => Proof era -> UTxO era
initUTxO pf =
  UTxO $
    Map.fromList
      [ (inlineDatumInput, inlineDatumOutput pf),
        (referenceScriptInput, referenceScriptOutput pf),
        (referenceDataHashInput, referenceDataHashOutput pf),
        (somePlainInput, somePlainOutput pf),
        (simpleV2EUTxOInput, simpleV2EUTxO pf),
        (referenceScriptInput2, referenceScriptOutput2 pf),
        (failsEUTxOInput, failsEUTxO pf),
        (inlineDatumInputV1, inlineDatumOutputV1 pf),
        (collateralInput11, collateralOutput pf),
        (collateralInput17, collateralOutput pf),
        (referenceScriptInput4, referenceScriptOutput4 pf),
        (inlineDatumInputOdd, inlineDatumOutputFailingScript pf),
        (somePlainInput2, somePlainOutput2 pf),
        (referenceSimpleScriptInput, referenceSimpleScriptOutput pf),
        (simpleScriptLockedInput, simpleScriptLockedOutput pf),
        (malformedScriptTxIn, malformedScriptTxOut pf)
      ]

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

pp :: Proof era -> Core.PParams era
pp pf = newPParams pf defaultPPs

-- =========================================================================
--  Example 1: Spend a EUTxO with an inline datum.
-- =========================================================================

redeemerExample1 :: Data era
redeemerExample1 = Data (Plutus.I 42)

validatingRedeemersEx1 :: Era era => Redeemers era
validatingRedeemersEx1 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample1, ExUnits 5000 5000)

outEx1 :: Scriptic era => Proof era -> Core.TxOut era
outEx1 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]

inlineDatumTxBodyEven, inlineDatumTxBodyOdd :: Scriptic era => Proof era -> Core.TxBody era
inlineDatumTxBodyEven pf =
  newTxBody
    pf
    [ Inputs' [inlineDatumInput],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]
inlineDatumTxBodyOdd pf =
  newTxBody
    pf
    [ Inputs' [inlineDatumInputOdd],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

inlineDatumTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  (Proof era -> Core.TxBody era) ->
  Core.Tx era
inlineDatumTx pf mkTxBody =
  let txBody = mkTxBody pf
   in newTx
        pf
        [ Body txBody,
          WitnessesI
            [ AddrWits' [makeWitnessVKey (hashAnnotated txBody) (someKeys pf)],
              ScriptWits' [evenData3ArgsScript pf],
              RdmrWits validatingRedeemersEx1
            ]
        ]

evenData3ArgsScript :: HasCallStack => Proof era -> Core.Script era
evenData3ArgsScript proof =
  case proof of
    Shelley _ -> error unsupported
    Mary _ -> error unsupported
    Allegra _ -> error unsupported
    Alonzo _ -> evenData3ArgsLang PlutusV1
    Babbage _ -> evenData3ArgsLang PlutusV2
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

utxoEx1 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx1 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (inlineDatumTxBodyEven pf) (outEx1 pf)) 1

utxoStEx1 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx1 pf = smartUTxOState (utxoEx1 pf) (Coin 0) (Coin 5) def

utxoEx1invalid :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx1invalid pf = expectedUTxO (initUTxO pf) ExpectSuccessInvalid 11

utxoStEx1invalid ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx1invalid pf = smartUTxOState (utxoEx1invalid pf) (Coin 0) (Coin 2115) def

-- =========================================================================
--  Example 2: Use a reference script.
-- =========================================================================

txDatsExample2 :: Era era => TxDats era
txDatsExample2 = TxDats $ keyBy hashData [datumExampleSixtyFiveBytes]

referenceScriptTxBody :: Scriptic era => Proof era -> Core.TxBody era
referenceScriptTxBody pf =
  newTxBody
    pf
    [ Inputs' [simpleV2EUTxOInput],
      RefInputs' [referenceScriptInput4],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 txDatsExample2)
    ]

referenceScriptTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
referenceScriptTx pf =
  newTx
    pf
    [ Body (referenceScriptTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (referenceScriptTxBody pf)) (someKeys pf)],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx2 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx2 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (referenceScriptTxBody pf) (outEx1 pf)) 5

utxoStEx2 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx2 pf = smartUTxOState (utxoEx2 pf) (Coin 0) (Coin 5) def

-- =========================================================================
--  Example 3: Spend a EUTxO with an inline datum, using a reference script.
--             Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScriptTxBody :: Scriptic era => Proof era -> Core.TxBody era
inlineDatumAndRefScriptTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      RefInputs' [referenceScriptInput],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

inlineDatumAndRefScriptTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
inlineDatumAndRefScriptTx pf =
  newTx
    pf
    [ Body (inlineDatumAndRefScriptTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (inlineDatumAndRefScriptTxBody pf)) (someKeys pf)],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx3 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx3 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (inlineDatumAndRefScriptTxBody pf) (outEx1 pf)) 1

utxoStEx3 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx3 pf = smartUTxOState (utxoEx3 pf) (Coin 0) (Coin 5) def

-- =========================================================================
--  Example 4: Spend a EUTxO with an inline datum, using a reference script,
--             and also redundantly supply the script witness.
-- =========================================================================

inlineDatumAndRefScriptAndWitScriptTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
inlineDatumAndRefScriptAndWitScriptTx pf =
  newTx
    pf
    [ Body (inlineDatumAndRefScriptTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (inlineDatumAndRefScriptTxBody pf)) (someKeys pf)],
          ScriptWits' [alwaysAlt 3 pf], -- This is redundant with the reference script
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx4 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx4 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (inlineDatumAndRefScriptTxBody pf) (outEx1 pf)) 1

utxoStEx4 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx4 pf = smartUTxOState (utxoEx4 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 5: Use a reference input with a data hash in the correspending output and
--             without supplying the correspending data witness.
-- ====================================================================================

outEx5 :: Scriptic era => Proof era -> Core.TxOut era
outEx5 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]

refInputWithDataHashNoWitTxBody :: Scriptic era => Proof era -> Core.TxBody era
refInputWithDataHashNoWitTxBody pf =
  newTxBody
    pf
    [ Inputs' [somePlainInput],
      RefInputs' [referenceDataHashInput],
      Outputs' [outEx5 pf],
      Txfee (Coin 5)
    ]

refInputWithDataHashNoWitTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
refInputWithDataHashNoWitTx pf =
  newTx
    pf
    [ Body (refInputWithDataHashNoWitTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (refInputWithDataHashNoWitTxBody pf)) (someKeys pf)]
        ]
    ]

utxoEx5 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx5 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (refInputWithDataHashNoWitTxBody pf) (outEx5 pf)) 4

utxoStEx5 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx5 pf = smartUTxOState (utxoEx5 pf) (Coin 0) (Coin 5) def

-- =======================================================================================
--  Example 6: Use a reference input with a data hash in the correspending output and
--             supplying the correspending data witness.
-- =======================================================================================

refInputWithDataHashWithWitTxBody :: Scriptic era => Proof era -> Core.TxBody era
refInputWithDataHashWithWitTxBody pf =
  newTxBody
    pf
    [ Inputs' [somePlainInput],
      RefInputs' [referenceDataHashInput],
      WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) txDatsExample2),
      Outputs' [outEx5 pf],
      Txfee (Coin 5)
    ]

refInputWithDataHashWithWitTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
refInputWithDataHashWithWitTx pf =
  newTx
    pf
    [ Body (refInputWithDataHashWithWitTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (refInputWithDataHashWithWitTxBody pf)) (someKeys pf)],
          DataWits' [datumExampleSixtyFiveBytes]
        ]
    ]

utxoEx6 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx6 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (refInputWithDataHashWithWitTxBody pf) (outEx5 pf)) 4

utxoStEx6 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx6 pf = smartUTxOState (utxoEx6 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 7: Use a reference script for authorizing a delegation certificate
-- ====================================================================================

outEx7 :: Scriptic era => Proof era -> Core.TxOut era
outEx7 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]

redeemersEx7 :: Era era => Redeemers era
redeemersEx7 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (redeemerExample1, ExUnits 5000 5000)

refScriptForDelegCertTxBody :: forall era. Scriptic era => Proof era -> Core.TxBody era
refScriptForDelegCertTxBody pf =
  newTxBody
    pf
    [ Inputs' [somePlainInput],
      RefInputs' [referenceScriptInput2],
      Collateral' [collateralInput11],
      Outputs' [outEx7 pf],
      Certs' [DCertDeleg (DeRegKey cred)],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] redeemersEx7 mempty)
    ]
  where
    cred = ScriptHashObj (hashScript @era $ alwaysAlt 2 pf)

refScriptForDelegCertTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
refScriptForDelegCertTx pf =
  newTx
    pf
    [ Body (refScriptForDelegCertTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (refScriptForDelegCertTxBody pf)) (someKeys pf)],
          RdmrWits redeemersEx7
        ]
    ]

utxoEx7 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx7 pf = expectedUTxO (initUTxO pf) (ExpectSuccess (refScriptForDelegCertTxBody pf) (outEx5 pf)) 4

utxoStEx7 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx7 pf = smartUTxOState (utxoEx7 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 8: Use a collateral output
-- ====================================================================================

collateralReturn :: Era era => Proof era -> Core.TxOut era
collateralReturn pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2110)]

collateralOutputTxBody :: Scriptic era => Proof era -> Core.TxBody era
collateralOutputTxBody pf =
  newTxBody
    pf
    [ Inputs' [failsEUTxOInput],
      Collateral' [collateralInput17],
      CollateralReturn' [collateralReturn pf],
      TotalCol (SJust $ Coin 5),
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx1 txDatsExample2)
    ]

collateralOutputTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
collateralOutputTx pf =
  newTx
    pf
    [ Body (collateralOutputTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (collateralOutputTxBody pf)) (someKeys pf)],
          ScriptWits' [never 3 pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx8 :: forall era. PostShelley era => Proof era -> UTxO era
utxoEx8 pf =
  UTxO $
    Map.insert
      (TxIn (txid (collateralOutputTxBody pf)) (mkTxIxPartial 1))
      (collateralReturn pf)
      utxoWithoutCollateral
  where
    UTxO utxoWithoutCollateral = expectedUTxO (initUTxO pf) ExpectFailure 7

utxoStEx8 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx8 pf = smartUTxOState (utxoEx8 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 9: Invalid - collateral total
-- ====================================================================================

incorrectCollateralTotalTxBody :: Scriptic era => Proof era -> Core.TxBody era
incorrectCollateralTotalTxBody pf =
  newTxBody
    pf
    [ Inputs' [inlineDatumInput],
      Collateral' [collateralInput11],
      CollateralReturn' [collateralReturn pf],
      TotalCol (SJust $ Coin 6),
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

incorrectCollateralTotalTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
incorrectCollateralTotalTx pf =
  newTx
    pf
    [ Body (incorrectCollateralTotalTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (incorrectCollateralTotalTxBody pf)) (someKeys pf)],
          ScriptWits' [evenData3ArgsScript pf],
          RdmrWits validatingRedeemersEx1
        ]
    ]

-- ====================================================================================
--  Example 10: Invalid - Inline datum used with redundant datum in witness set
-- ====================================================================================

inlineDatumRedundantDatumTxBody :: Scriptic era => Proof era -> Core.TxBody era
inlineDatumRedundantDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [inlineDatumInput],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 txDatsExample2)
    ]

inlineDatumRedundantDatumTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
inlineDatumRedundantDatumTx pf =
  newTx
    pf
    [ Body (inlineDatumRedundantDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (inlineDatumRedundantDatumTxBody pf)) (someKeys pf)],
          ScriptWits' [evenData3ArgsScript pf],
          DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemersEx1
        ]
    ]

-- ====================================================================================
--  Example 11: Invalid - Using inline datums with Plutus V1 script
-- ====================================================================================

inlineDatumV1TxBody :: Scriptic era => Proof era -> Core.TxBody era
inlineDatumV1TxBody pf =
  newTxBody
    pf
    [ Inputs' [inlineDatumInputV1],
      Collateral' [collateralInput11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx1 mempty)
    ]

inlineDatumV1Tx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
inlineDatumV1Tx pf =
  newTx
    pf
    [ Body (inlineDatumV1TxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (inlineDatumV1TxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          RdmrWits validatingRedeemersEx1
        ]
    ]

class BabbageBased era failure where
  fromUtxoB :: BabbageUtxoPred era -> failure

  fromUtxowB :: BabbageUtxowPred era -> failure

instance BabbageBased (BabbageEra c) (BabbageUtxowPred (BabbageEra c)) where
  fromUtxoB = UtxoFailure
  fromUtxowB = id

-- ====================================================================================
--  Example 12: Invalid - Malformed plutus reference script creation
-- ====================================================================================

malformedScriptRefTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
malformedScriptRefTx pf =
  newTx
    pf
    [ Body txb,
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated txb) (someKeys pf)]
        ]
    ]
  where
    txb = malformedScriptRefTxBody pf

malformedScriptRefTxBody ::
  forall era.
  (Scriptic era) =>
  Proof era ->
  Core.TxBody era
malformedScriptRefTxBody pf =
  newTxBody
    pf
    [ Outputs' [malformedScriptsTxOut pf],
      Inputs' [somePlainInput2]
    ]

malformedScriptsTxOut ::
  Era era =>
  Proof era ->
  Core.TxOut era
malformedScriptsTxOut pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 5000),
      RefScript' [malformedScript pf "rs"]
    ]

malformedScript :: forall era. Proof era -> ShortByteString -> Core.Script era
malformedScript pf s = case pf of
  Babbage {} -> ms
  Alonzo {} -> ms
  x@Shelley {} -> er x
  x@Mary {} -> er x
  x@Allegra {} -> er x
  where
    ms :: Script era
    ms = PlutusScript PlutusV2 $ "nonsense " <> s
    er x = error $ "no malformedScript for " <> show x

-- ====================================================================================
--  Example: Don't run reference scripts in output for validation
-- ====================================================================================

refScriptWasInOutputTxBody :: Scriptic era => Proof era -> Core.TxBody era
refScriptWasInOutputTxBody pf =
  newTxBody
    pf
    [ Inputs' [referenceSimpleScriptInput],
      Outputs' [outEx1 pf],
      Txfee (Coin 5)
    ]

refScriptWasInOutputTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
refScriptWasInOutputTx pf =
  newTx
    pf
    [ Body (refScriptWasInOutputTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (refScriptWasInOutputTxBody pf)) (someKeys pf)]
        ]
    ]

refScriptWasInOutputUTxO :: forall era. PostShelley era => Proof era -> UTxO era
refScriptWasInOutputUTxO pf = expectedUTxO (initUTxO pf) (ExpectSuccess (refScriptWasInOutputTxBody pf) (outEx1 pf)) 12

refScriptWasInOutputState ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
refScriptWasInOutputState pf = smartUTxOState (refScriptWasInOutputUTxO pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example: Unlock Simple Scripts with a Reference Script
-- ====================================================================================

spendSimpleScriptLockedOutputWithRefScriptsTxBody :: Scriptic era => Proof era -> Core.TxBody era
spendSimpleScriptLockedOutputWithRefScriptsTxBody pf =
  newTxBody
    pf
    [ Inputs' [simpleScriptLockedInput],
      RefInputs' [referenceSimpleScriptInput],
      Outputs' [outEx1 pf],
      Txfee (Coin 5)
    ]

spendSimpleScriptLockedOutputWithRefScriptsTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
spendSimpleScriptLockedOutputWithRefScriptsTx pf =
  newTx
    pf
    [ Body (spendSimpleScriptLockedOutputWithRefScriptsTxBody pf),
      WitnessesI
        [ AddrWits'
            [ makeWitnessVKey (hashAnnotated (spendSimpleScriptLockedOutputWithRefScriptsTxBody pf)) (someKeys pf),
              makeWitnessVKey (hashAnnotated (spendSimpleScriptLockedOutputWithRefScriptsTxBody pf)) (keysForMultisig pf)
            ]
            -- Note we did not add a script witness for simpleScript
        ]
    ]

spendSimpleScriptOutWithRefScriptUTxO :: forall era. PostShelley era => Proof era -> UTxO era
spendSimpleScriptOutWithRefScriptUTxO pf =
  expectedUTxO (initUTxO pf) (ExpectSuccess (spendSimpleScriptLockedOutputWithRefScriptsTxBody pf) (outEx1 pf)) 20

spendSimpleScriptOutWithRefScriptUTxOState ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
spendSimpleScriptOutWithRefScriptUTxOState pf = smartUTxOState (spendSimpleScriptOutWithRefScriptUTxO pf) (Coin 0) (Coin 5) def

-- ========================================================================================
--  Example 13: Invalid - TxOut too large for the included ADA, using a large inline datum
-- ========================================================================================

largeDatum :: Data era
largeDatum = Data (Plutus.B . BS.pack $ replicate 1500 0)

largeOutput :: forall era. Scriptic era => Proof era -> Core.TxOut era
largeOutput pf =
  newTxOut
    pf
    [ Address (plainAddr pf),
      Amount (inject $ Coin 1135),
      Datum . Babbage.Datum . dataToBinaryData $ largeDatum @era
    ]

largeOutputTxBody :: Scriptic era => Proof era -> Core.TxBody era
largeOutputTxBody pf =
  newTxBody
    pf
    [ Inputs' [somePlainInput],
      Outputs' [largeOutput pf],
      Txfee (Coin 5)
    ]

largeOutputTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
largeOutputTx pf =
  newTx
    pf
    [ Body (largeOutputTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (largeOutputTxBody pf)) (someKeys pf)]
        ]
    ]

-- ====================================================================================
--  Example 14: Invalid - Malformed plutus script witness
-- ====================================================================================

malformedScriptWitTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Core.Tx era
malformedScriptWitTx pf =
  newTx
    pf
    [ Body txb,
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated txb) (someKeys pf)],
          ScriptWits' [malformedScript pf "malfoy"],
          RdmrWits validatingRedeemersEx1
        ]
    ]
  where
    txb = malformedScriptWitTxBody pf

outEx14 :: Era era => Proof era -> Core.TxOut era
outEx14 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 5000)]

malformedScriptWitTxBody ::
  forall era.
  (Scriptic era) =>
  Proof era ->
  Core.TxBody era
malformedScriptWitTxBody pf =
  newTxBody
    pf
    [ Inputs' [malformedScriptTxIn],
      Collateral' [collateralInput11],
      Outputs' [outEx14 pf],
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 mempty)
    ]

-- ====================================================================================
--
-- ====================================================================================

testU ::
  forall era.
  ( GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    HasCallStack
  ) =>
  Proof era ->
  Core.Tx era ->
  Either [PredicateFailure (Core.EraRule "UTXOW" era)] (State (Core.EraRule "UTXOW" era)) ->
  Assertion
testU pf tx expect = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf) tx expect

genericBabbageFeatures ::
  forall era.
  ( AlonzoBased era (PredicateFailure (EraRule "UTXOW" era)),
    BabbageBased era (PredicateFailure (EraRule "UTXOW" era)),
    State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  TestTree
genericBabbageFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $
            testU
              pf
              (trustMeP pf True $ inlineDatumTx pf inlineDatumTxBodyEven)
              (Right $ utxoStEx1 pf),
          testCase "inline datum failing script" $
            testU
              pf
              (trustMeP pf False $ inlineDatumTx pf inlineDatumTxBodyOdd)
              (Right $ utxoStEx1invalid pf),
          testCase "reference script" $
            testU
              pf
              (trustMeP pf True $ referenceScriptTx pf)
              (Right $ utxoStEx2 pf),
          testCase "inline datum and ref script" $
            testU
              pf
              (trustMeP pf True $ inlineDatumAndRefScriptTx pf)
              (Right $ utxoStEx3 pf),
          testCase "reference input with data hash, no data witness" $
            testU
              pf
              (trustMeP pf True $ refInputWithDataHashNoWitTx pf)
              (Right $ utxoStEx5 pf),
          testCase "reference input with data hash, with data witness" $
            testU
              pf
              (trustMeP pf True $ refInputWithDataHashWithWitTx pf)
              (Right $ utxoStEx6 pf),
          testCase "reference script to authorize delegation certificate" $
            testU
              pf
              (trustMeP pf True $ refScriptForDelegCertTx pf)
              (Right $ utxoStEx7 pf),
          testCase "use a collateral output" $
            testU
              pf
              (trustMeP pf False $ collateralOutputTx pf)
              (Right $ utxoStEx8 pf),
          testCase "not validating scripts not required" $
            testU
              pf
              (trustMeP pf True $ refScriptWasInOutputTx pf)
              (Right . refScriptWasInOutputState $ pf),
          testCase "spend simple script output with reference script" $
            testU
              pf
              (trustMeP pf True $ spendSimpleScriptLockedOutputWithRefScriptsTx pf)
              (Right . spendSimpleScriptOutWithRefScriptUTxOState $ pf)
        ],
      testGroup
        "invalid transactions"
        [ testCase "incorrect collateral total" $
            testU
              pf
              (trustMeP pf True $ incorrectCollateralTotalTx pf)
              (Left [fromUtxoB @era (IncorrectTotalCollateralField (Coin 5) (Coin 6))]),
          testCase "malformed reference script" $
            testU
              pf
              (trustMeP pf True $ malformedScriptRefTx pf)
              ( Left
                  [ fromUtxowB @era $
                      MalformedReferenceScripts $
                        Set.singleton
                          (hashScript @era $ malformedScript pf "rs")
                  ]
              ),
          testCase "malformed script witness" $
            testU
              pf
              (trustMeP pf True $ malformedScriptWitTx pf)
              ( Left
                  [ fromUtxowB @era $
                      MalformedScriptWitnesses $
                        Set.singleton (hashScript @era $ malformedScript pf "malfoy")
                  ]
              ),
          testCase "inline datum and ref script and redundant script witness" $
            testU
              pf
              (trustMeP pf True $ inlineDatumAndRefScriptAndWitScriptTx pf)
              ( Left
                  [ fromUtxow @era
                      ( Shelley.ExtraneousScriptWitnessesUTXOW
                          (Set.singleton $ hashScript @era (alwaysAlt 3 pf))
                      )
                  ]
              ),
          testCase "inline datum with redundant datum witness" $
            testU
              pf
              (trustMeP pf True $ inlineDatumRedundantDatumTx pf)
              ( Left
                  [ fromPredFail @era
                      ( NonOutputSupplimentaryDatums
                          (Set.singleton $ hashData @era datumExampleSixtyFiveBytes)
                          mempty
                      )
                  ]
              ),
          testCase "inline datum with Plutus V1" $
            testU
              pf
              (trustMeP pf True $ inlineDatumV1Tx pf)
              ( Left
                  [ fromUtxos @era
                      ( CollectErrors
                          [ BadTranslation $
                              InlineDatumsNotSupported (TxOutFromInput inlineDatumInputV1)
                          ]
                      )
                  ]
              ),
          testCase "min-utxo value with output too large" $
            testU
              pf
              (trustMeP pf True $ largeOutputTx pf)
              (Left [fromUtxoB @era $ BabbageOutputTooSmallUTxO [(largeOutput pf, Coin 8915)]])
        ]
    ]

babbageFeatures :: TestTree
babbageFeatures =
  testGroup
    "Babbage Features"
    [ genericBabbageFeatures (Babbage Mock)
    ]
