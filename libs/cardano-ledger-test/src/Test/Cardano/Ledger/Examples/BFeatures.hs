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

module Test.Cardano.Ledger.Examples.BFeatures where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (Data (..), dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..), Script (PlutusScript))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
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
import Cardano.Ledger.Shelley.TxBody (DCert (..), DelegCert (..))
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn (..), txid)
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (pack)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import GHC.Stack
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( freeCostModelV1,
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

mkGenesisTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => Integer -> TxIn crypto
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (Crypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 1 1 1 1 1)

keysForMultisig :: forall era. Era era => Proof era -> KeyPair 'Witness (Crypto era)
keysForMultisig _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 99)

keyHashForMultisig :: forall era. Era era => Proof era -> KeyHash 'Witness (Crypto era)
keyHashForMultisig pf = hashKey . vKey $ keysForMultisig pf

simpleScript :: forall era. (Scriptic era) => Proof era -> Core.Script era
simpleScript pf = allOf [require @era (keyHashForMultisig pf)] pf

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

plainAddr :: forall era. Era era => Proof era -> Addr (Crypto era)
plainAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

scriptAddr :: forall era. (Scriptic era) => Proof era -> Core.Script era -> Addr (Crypto era)
scriptAddr _pf s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

datumExampleEven :: Data era
datumExampleEven = Data (Plutus.I 2)

validatingRedeemersDatumEven :: Era era => Redeemers era
validatingRedeemersDatumEven =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (datumExampleEven, ExUnits 5000 5000)

-- We intentionally use a ByteString with length greater than 64 to serve as
-- as reminder that our protection against contiguous data over 64 Bytes on
-- the wire is done during deserialization using the Plutus library.
sixtyFiveBytes :: BS.ByteString
sixtyFiveBytes = BS.pack [1 .. 65]

datumExampleSixtyFiveBytes :: Data era
datumExampleSixtyFiveBytes = Data (Plutus.B sixtyFiveBytes)

txDatsExample2 :: Era era => TxDats era
txDatsExample2 = TxDats $ keyBy hashData [datumExampleSixtyFiveBytes]

outEx1 :: Scriptic era => Proof era -> Core.TxOut era
outEx1 pf = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 4995)]

collateralOutput :: Scriptic era => Proof era -> Core.TxOut era
collateralOutput pf =
  newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 2115)]

-- =========================================================================
-- Spend a EUTxO with an inline datum.
-- =========================================================================

inlineDatumTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
inlineDatumTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 1,
          newTxOut
            pf
            [ Address (scriptAddr pf (evenData3ArgsScript pf)),
              Amount (inject $ Coin 5000),
              Datum (Babbage.Datum . dataToBinaryData $ datumExampleEven @era)
            ]
        ),
      collateral = [(mkGenesisTxIn 11, collateralOutput pf)],
      refInputs = [],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersDatumEven mempty)
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields =
        [ ScriptWits' [evenData3ArgsScript pf],
          RdmrWits validatingRedeemersDatumEven
        ],
      ttxOut = outEx1 pf
    }

-- =========================================================================
-- Use a reference script.
-- =========================================================================

referenceScriptTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
referenceScriptTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 5,
          newTxOut
            pf
            [ Address (scriptAddr pf (alwaysAlt 3 pf)),
              Amount (inject $ Coin 5000),
              DHash' [hashData $ datumExampleSixtyFiveBytes @era]
            ]
        ),
      collateral = [(mkGenesisTxIn 11, collateralOutput pf)],
      refInputs =
        [ ( mkGenesisTxIn 9,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 5000),
                RefScript (SJust $ alwaysAlt 3 pf)
              ]
          )
        ],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersDatumEven txDatsExample2)
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields =
        [ DataWits' [datumExampleSixtyFiveBytes],
          RdmrWits validatingRedeemersDatumEven
        ],
      ttxOut = outEx1 pf
    }

-- =========================================================================
-- Spend a EUTxO with an inline datum, using a reference script.
-- Notice that the reference input is not consumed.
-- =========================================================================

inlineDatumAndRefScriptTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
inlineDatumAndRefScriptTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 1,
          newTxOut
            pf
            [ Address (scriptAddr pf (evenData3ArgsScript pf)),
              Amount (inject $ Coin 5000),
              Datum (Babbage.Datum . dataToBinaryData $ datumExampleEven @era)
            ]
        ),
      collateral = [(mkGenesisTxIn 11, collateralOutput pf)],
      refInputs =
        [ ( mkGenesisTxIn 2,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 5000),
                RefScript (SJust $ evenData3ArgsScript pf)
              ]
          )
        ],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersDatumEven mempty)
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields = [RdmrWits validatingRedeemersDatumEven],
      ttxOut = outEx1 pf
    }

-- =========================================================================
-- Spend a EUTxO with an inline datum, using a reference script,
-- and also redundantly supply the script witness.
-- =========================================================================

inlineDatumAndRefScriptAndWitScriptTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
inlineDatumAndRefScriptAndWitScriptTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 1,
          newTxOut
            pf
            [ Address (scriptAddr pf (evenData3ArgsScript pf)),
              Amount (inject $ Coin 5000),
              Datum (Babbage.Datum . dataToBinaryData $ datumExampleEven @era)
            ]
        ),
      collateral = [(mkGenesisTxIn 11, collateralOutput pf)],
      refInputs =
        [ ( mkGenesisTxIn 2,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 5000),
                RefScript (SJust $ evenData3ArgsScript pf)
              ]
          )
        ],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersDatumEven mempty)
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields =
        [ ScriptWits' [alwaysAlt 3 pf], -- This is redundant with the reference script
          RdmrWits validatingRedeemersDatumEven
        ],
      ttxOut = outEx1 pf
    }

-- ====================================================================================
-- Use a reference input with a data hash in the correspending output and
-- without supplying the correspending data witness.
-- ====================================================================================

refInputWithDataHashNoWitTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
refInputWithDataHashNoWitTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 4,
          newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]
        ),
      collateral = [],
      refInputs =
        [ ( mkGenesisTxIn 3,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 10),
                DHash' [hashData $ datumExampleSixtyFiveBytes @era]
              ]
          )
        ],
      txBodyFields = [Txfee (Coin 5)],
      keysForAddrWits = [someKeys pf],
      otherWitsFields = [],
      ttxOut = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]
    }

-- =======================================================================================
-- Use a reference input with a data hash in the correspending output and
-- supplying the correspending data witness.
-- =======================================================================================
refInputWithDataHashWithWitTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
refInputWithDataHashWithWitTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 4,
          newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]
        ),
      collateral = [],
      refInputs =
        [ ( mkGenesisTxIn 3,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 10),
                DHash' [hashData $ datumExampleSixtyFiveBytes @era]
              ]
          )
        ],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) txDatsExample2)
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields = [DataWits' [datumExampleSixtyFiveBytes]],
      ttxOut = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]
    }

-- ====================================================================================
-- Use a reference script for authorizing a delegation certificate
-- ====================================================================================

refScriptForDelegCertTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
refScriptForDelegCertTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 4,
          newTxOut pf [Address $ plainAddr pf, Amount (inject $ Coin 1140)]
        ),
      collateral = [(mkGenesisTxIn 11, collateralOutput pf)],
      refInputs =
        [ ( mkGenesisTxIn 6,
            newTxOut
              pf
              [ Address (plainAddr pf),
                Amount (inject $ Coin 5000),
                RefScript (SJust $ alwaysAlt 2 pf)
              ]
          )
        ],
      txBodyFields =
        [ Txfee (Coin 5),
          WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] redeemersEx7 mempty),
          Certs' [DCertDeleg (DeRegKey cred)]
        ],
      keysForAddrWits = [someKeys pf],
      otherWitsFields = [RdmrWits redeemersEx7],
      ttxOut = newTxOut pf [Address (plainAddr pf), Amount (inject $ Coin 1135)]
    }
  where
    cred = ScriptHashObj (hashScript @era $ alwaysAlt 2 pf)

redeemersEx7 :: Era era => Redeemers era
redeemersEx7 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (Data (Plutus.I 42), ExUnits 5000 5000)

-- ====================================================================================
-- Don't run reference scripts in output for validation
-- ====================================================================================

refScriptInOutputTestCaseData :: forall era. (Scriptic era) => Proof era -> TestCaseData era
refScriptInOutputTestCaseData pf =
  TestCaseData
    { input =
        ( mkGenesisTxIn 12,
          newTxOut
            pf
            [ Address (plainAddr pf),
              Amount (inject $ Coin 5000),
              RefScript (SJust $ simpleScript pf)
            ]
        ),
      collateral = [],
      refInputs = [],
      txBodyFields = [Txfee (Coin 5)],
      keysForAddrWits = [someKeys pf],
      otherWitsFields = [],
      ttxOut = outEx1 pf
    }

class BabbageBased era failure where
  fromUtxoB :: BabbageUtxoPred era -> failure
  fromUtxowB :: BabbageUtxowPred era -> failure

instance BabbageBased (BabbageEra c) (BabbageUtxowPred (BabbageEra c)) where
  fromUtxoB = UtxoFailure
  fromUtxowB = id

type InOut era = (TxIn (Crypto era), Core.TxOut era)

data TestCaseData era = TestCaseData
  { input :: InOut era,
    collateral :: [InOut era],
    refInputs :: [InOut era],
    ttxOut :: Core.TxOut era,
    txBodyFields :: [TxBodyField era],
    keysForAddrWits :: [KeyPair 'Payment (Crypto era)],
    otherWitsFields :: [WitnessesField era]
  }

testExpectSuccessValid ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    Scriptic era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  TestCaseData era ->
  Assertion
testExpectSuccessValid
  pf
  (TestCaseData input' collateral' refInputs' txOut' txBodyFields' keysForAddrWits' otherWitsFields') =
    let txBody' =
          newTxBody
            pf
            ( [ Inputs' [fst input'],
                Collateral' $ fst <$> collateral',
                RefInputs' $ fst <$> refInputs',
                Outputs' [txOut']
              ]
                ++ txBodyFields'
            )
        addrWits = makeWitnessVKey (hashAnnotated txBody') <$> keysForAddrWits'
        tx' =
          newTx
            pf
            ( Body txBody' :
              [ WitnessesI
                  (AddrWits' addrWits : otherWitsFields')
              ]
            )
        newTxIn = TxIn (txid txBody') minBound
        utxo = (UTxO . Map.fromList) $ [input'] ++ collateral' ++ refInputs'
        expectedUtxo = UTxO $ Map.insert newTxIn txOut' (Map.fromList (collateral' ++ refInputs'))
        expectedState = smartUTxOState expectedUtxo (Coin 0) (Coin 5) def
     in testUTXOW (UTXOW pf) utxo (pp pf) (trustMeP pf True tx') (Right expectedState)

genericBFeatures ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era
  ) =>
  Proof era ->
  TestTree
genericBFeatures pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "inline datum" $ testExpectSuccessValid pf (inlineDatumTestCaseData pf),
          testCase "reference script" $ testExpectSuccessValid pf (referenceScriptTestCaseData pf),
          testCase "inline datum and ref script" $ testExpectSuccessValid pf (inlineDatumAndRefScriptTestCaseData pf),
          testCase "reference input with data hash, no data witness" $ testExpectSuccessValid pf (refInputWithDataHashNoWitTestCaseData pf),
          testCase "reference input with data hash, with data witness" $ testExpectSuccessValid pf (refInputWithDataHashWithWitTestCaseData pf),
          testCase "reference script to authorize delegation certificate" $ testExpectSuccessValid pf (refScriptForDelegCertTestCaseData pf),
          testCase "not validating scripts not required" $ testExpectSuccessValid pf (refScriptInOutputTestCaseData pf)
        ],
      testGroup "invalid transactions" []
    ]

bFeatures :: TestTree
bFeatures =
  testGroup
    "B Features"
    [genericBFeatures (Babbage Mock)]
