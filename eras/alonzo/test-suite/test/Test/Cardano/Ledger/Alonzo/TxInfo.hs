module Test.Cardano.Ledger.Alonzo.TxInfo where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (TranslationError (..), txInfo)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTx (Tx), EraTxBody (TxBody), EraTxOut (TxOut))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap (aliceByronAddr)
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePHK)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

byronAddr :: Addr StandardCrypto
byronAddr = AddrBootstrap (BootstrapAddress aliceByronAddr)

shelleyAddr :: Addr StandardCrypto
shelleyAddr = Addr Testnet alicePHK StakeRefNull

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

-- This input is only a "Byron input" in the sense
-- that we attach it to a Byron output in the UTxO created below.
byronInput :: TxIn StandardCrypto
byronInput = mkTxInPartial genesisId 0

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: TxIn StandardCrypto
shelleyInput = mkTxInPartial genesisId 1

byronOutput :: TxOut Alonzo
byronOutput = AlonzoTxOut byronAddr (Val.inject $ Coin 1) SNothing

shelleyOutput :: TxOut Alonzo
shelleyOutput = AlonzoTxOut shelleyAddr (Val.inject $ Coin 2) SNothing

utxo :: UTxO Alonzo
utxo = UTxO $ Map.fromList [(byronInput, byronOutput), (shelleyInput, shelleyOutput)]

txb :: TxIn StandardCrypto -> TxOut Alonzo -> TxBody Alonzo
txb i o =
  AlonzoTxBody
    (Set.singleton i) -- inputs
    mempty -- collateral
    (StrictSeq.singleton o) -- outputs
    mempty -- certs
    (Wdrl mempty) -- withdrawals
    (Coin 2) -- txfee
    (ValidityInterval SNothing SNothing) -- validity interval
    SNothing -- updates
    mempty -- required signers
    mempty -- mint
    SNothing -- script integrity hash
    SNothing -- auxiliary data hash
    SNothing -- network ID

txEx :: TxIn StandardCrypto -> TxOut Alonzo -> Tx Alonzo
txEx i o = AlonzoTx (txb i o) mempty (IsValid True) SNothing

silentlyIgnore :: Tx Alonzo -> Assertion
silentlyIgnore tx =
  case ctx of
    Right _ -> pure ()
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
  where
    ctx = txInfo PlutusV1 ei ss utxo tx

expectTranslationError :: Language -> Tx Alonzo -> TranslationError StandardCrypto -> Assertion
expectTranslationError lang tx expected =
  case ctx of
    Right _ -> error "This translation was expected to fail, but it succeeded."
    Left e -> e @?= expected
  where
    ctx = txInfo lang ei ss utxo tx

txInfoTests :: TestTree
txInfoTests =
  testGroup
    "txInfo translation"
    [ testGroup
        "Plutus V1"
        [ testCase "silently ignore byron txout" $
            silentlyIgnore (txEx shelleyInput byronOutput),
          testCase "silently ignore byron txin" $
            silentlyIgnore (txEx byronInput shelleyOutput)
        ],
      testGroup
        "Plutus V2"
        [ testCase "translation error for V2 in Alonzo" $
            expectTranslationError
              PlutusV2
              (txEx shelleyInput shelleyOutput)
              (LanguageNotSupported PlutusV2)
        ]
    ]
