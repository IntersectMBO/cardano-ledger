module Test.Cardano.Ledger.Alonzo.TxInfo where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (TranslationError (..), txInfo)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Compact.SplitMap as SplitMap
import Data.Default.Class (def)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Cardano.Ledger.EraBuffet (StandardCrypto)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap (aliceByronAddr)
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePHK)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

byronAddr :: Addr StandardCrypto
byronAddr = AddrBootstrap (BootstrapAddress aliceByronAddr)

shelleyAddr :: Addr StandardCrypto
shelleyAddr = Addr Testnet alicePHK StakeRefNull

ei :: EpochInfo Identity
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

type A = AlonzoEra StandardCrypto

-- This input is only a "Byron input" in the sense
-- that we attach it to a Byron output in the UTxO created below.
byronInput :: TxIn StandardCrypto
byronInput = mkTxInPartial genesisId 0

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: TxIn StandardCrypto
shelleyInput = mkTxInPartial genesisId 1

-- This input is only unknown in the sense
-- that it is not present in the UTxO created below.
unknownInput :: TxIn StandardCrypto
unknownInput = mkTxInPartial genesisId 2

byronOutput :: TxOut A
byronOutput = TxOut byronAddr (Val.inject $ Coin 1) SNothing

shelleyOutput :: TxOut A
shelleyOutput = TxOut shelleyAddr (Val.inject $ Coin 2) SNothing

utxo :: UTxO A
utxo = UTxO $ SplitMap.fromList [(byronInput, byronOutput), (shelleyInput, shelleyOutput)]

txb :: TxIn StandardCrypto -> TxOut A -> TxBody A
txb i o =
  TxBody
    (Set.singleton i) -- inputs
    mempty -- collateral
    (StrictSeq.singleton o) --outputs
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

txEx :: TxIn StandardCrypto -> TxOut A -> ValidatedTx A
txEx i o = ValidatedTx (txb i o) mempty (IsValid True) SNothing

silentlyIgnore :: ValidatedTx A -> Assertion
silentlyIgnore tx =
  case ctx of
    Right _ -> pure ()
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
  where
    ctx = runIdentity $ txInfo def PlutusV1 ei ss utxo tx

expectTranslationError :: Language -> ValidatedTx A -> TranslationError -> Assertion
expectTranslationError lang tx expected =
  case ctx of
    Right _ -> error "This translation was expected to fail, but it succeeded."
    Left e -> e @?= expected
  where
    ctx = runIdentity $ txInfo def lang ei ss utxo tx

txInfoTests :: TestTree
txInfoTests =
  testGroup
    "txInfo translation"
    [ testGroup
        "Plutus V1"
        [ testCase "silently ignore byron txout" $
            silentlyIgnore (txEx shelleyInput byronOutput),
          testCase "silently ignore byron txin" $
            silentlyIgnore (txEx byronInput shelleyOutput),
          testCase "silently ignore unknown txin (logic error)" $
            silentlyIgnore (txEx unknownInput shelleyOutput)
        ],
      testGroup
        "Plutus V2"
        [ testCase "translation error for V2 in Alonzo" $
            expectTranslationError
              PlutusV2
              (txEx shelleyInput shelleyOutput)
              LanguageNotSupported
        ]
    ]
