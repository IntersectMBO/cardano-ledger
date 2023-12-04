{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.TxInfo (
  tests,
) where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Core hiding (TranslationError)
import Cardano.Ledger.Alonzo.Plutus.Context (toPlutusTxInfo)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (transVITime)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..), natVersion)
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
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
    (Withdrawals mempty) -- withdrawals
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
  case toPlutusTxInfo SPlutusV1 def ei ss utxo tx of
    Right _ -> pure ()
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e

-- | The test checks that the old implementation of 'transVITime' stays intentionally incorrect,
-- by returning close upper bound of the validaty interval.
transVITimeUpperBoundIsClosed :: Assertion
transVITimeUpperBoundIsClosed = do
  let interval = ValidityInterval SNothing (SJust (SlotNo 40))
  case transVITime (def :: PParams Alonzo) ei ss interval of
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
    Right t -> t @?= (PV1.Interval (PV1.LowerBound PV1.NegInf True) (PV1.UpperBound (PV1.Finite (PV1.POSIXTime 40000)) True))

-- | The test checks that since protocol version 9 'transVITime' works correctly,
-- by returning open upper bound of the validaty interval.
transVITimeUpperBoundIsOpen :: Assertion
transVITimeUpperBoundIsOpen = do
  let interval = ValidityInterval SNothing (SJust (SlotNo 40))
  case transVITime (def & ppProtocolVersionL .~ BT.ProtVer (natVersion @9) 0 :: PParams Alonzo) ei ss interval of
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
    Right t -> t @?= (PV1.Interval (PV1.LowerBound PV1.NegInf True) (PV1.UpperBound (PV1.Finite (PV1.POSIXTime 40000)) False))

tests :: TestTree
tests =
  testGroup
    "txInfo translation"
    [ testGroup
        "Plutus V1"
        [ testCase "silently ignore byron txout" $
            silentlyIgnore (txEx shelleyInput byronOutput)
        , testCase "silently ignore byron txin" $
            silentlyIgnore (txEx byronInput shelleyOutput)
        ]
    , testGroup
        "transVITime"
        [ testCase "validity interval's upper bound is close when protocol < 9" transVITimeUpperBoundIsClosed
        , testCase "validity interval's upper bound is open when protocol >= 9" transVITimeUpperBoundIsOpen
        ]
    ]
