module Test.Cardano.Ledger.Alonzo.TxInfo where

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (TranslationError (..), txInfo)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), txrdmrs)
import Cardano.Ledger.BaseTypes (Network (..), ProtVer (..), StrictMaybe (..))
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
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified PlutusTx as P (Data (..))
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

emptyRedeemers :: Redeemers A
emptyRedeemers = Redeemers mempty

-- This redeemer is unknown since 'txEx' does not mint any tokens
unknownRedeemer :: Redeemers A
unknownRedeemer = Redeemers $ Map.singleton (RdmrPtr Mint 0) (Data $ P.I 0, ExUnits 0 0)

txEx :: TxIn StandardCrypto -> TxOut A -> Redeemers A -> ValidatedTx A
txEx i o r = ValidatedTx (txb i o) (mempty {txrdmrs = r}) (IsValid True) SNothing

ppV6 :: PParams A
ppV6 = def {_protocolVersion = ProtVer 6 0}

ppV7 :: PParams A
ppV7 = def {_protocolVersion = ProtVer 7 0}

silentlyIgnore :: ValidatedTx A -> Assertion
silentlyIgnore tx =
  case ctx of
    Right _ -> pure ()
    Left e -> assertFailure $ "no translation error was expected, but got: " <> show e
  where
    ctx = runIdentity $ txInfo ppV6 PlutusV1 ei ss utxo tx

expectTranslationError :: Language -> ValidatedTx A -> TranslationError -> Assertion
expectTranslationError lang tx expected =
  case ctx of
    Right _ -> error "This translation was expected to fail, but it succeeded."
    Left e -> e @?= expected
  where
    ctx = runIdentity $ txInfo ppV7 lang ei ss utxo tx

txInfoTests :: TestTree
txInfoTests =
  testGroup
    "txInfo translation"
    [ testGroup
        "Plutus V1, Protocol V6"
        [ testCase "silently ignore byron txout" $
            silentlyIgnore (txEx shelleyInput byronOutput emptyRedeemers),
          testCase "silently ignore byron txin" $
            silentlyIgnore (txEx byronInput shelleyOutput emptyRedeemers),
          testCase "silently ignore unknown txin (logic error)" $
            silentlyIgnore (txEx unknownInput shelleyOutput emptyRedeemers)
        ],
      testGroup
        "Plutus V1, Protocol V7"
        [ testCase "translation error byron txin" $
            expectTranslationError
              PlutusV1
              (txEx byronInput shelleyOutput emptyRedeemers)
              ByronInputInContext,
          testCase "translation error byron txout" $
            expectTranslationError
              PlutusV1
              (txEx shelleyInput byronOutput emptyRedeemers)
              ByronOutputInContext,
          testCase "translation error unknown txin (logic error)" $
            expectTranslationError
              PlutusV1
              (txEx unknownInput shelleyOutput emptyRedeemers)
              TranslationLogicErrorInput
        ],
      testGroup
        "Plutus V2"
        [ testCase "translation error byron txin" $
            expectTranslationError
              PlutusV2
              (txEx byronInput shelleyOutput emptyRedeemers)
              ByronInputInContext,
          testCase "translation error byron txout" $
            expectTranslationError
              PlutusV2
              (txEx shelleyInput byronOutput emptyRedeemers)
              ByronOutputInContext,
          testCase "translation error unknown txin (logic error)" $
            expectTranslationError
              PlutusV2
              (txEx unknownInput shelleyOutput emptyRedeemers)
              TranslationLogicErrorInput,
          testCase "translation error unknown redeemer (logic error)" $
            expectTranslationError
              PlutusV2
              (txEx shelleyInput shelleyOutput unknownRedeemer)
              TranslationLogicErrorRedeemer
        ]
    ]
