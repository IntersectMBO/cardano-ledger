{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.TxInfoSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra, Tx (..))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  LedgerTxInfo (..),
  toPlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (transValidityInterval)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), TxBody (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkCredential, mkKeyPair)
import Test.Cardano.Ledger.Shelley.Examples (exampleByronAddress)

shelleyAddr :: Addr
shelleyAddr = Addr Testnet pk StakeRefNull
  where
    pk = mkCredential (mkKeyPair 0 :: KeyPair 'Payment)

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

-- This input is only a "Byron input" in the sense
-- that we attach it to a Byron output in the UTxO created below.
byronInput :: TxIn
byronInput = mkTxInPartial genesisId 0

-- This input is only a "Shelley input" in the sense
-- that we attach it to a Shelley output in the UTxO created below.
shelleyInput :: TxIn
shelleyInput = mkTxInPartial genesisId 1

byronOutput :: TxOut AlonzoEra
byronOutput = AlonzoTxOut exampleByronAddress (Val.inject $ Coin 1) SNothing

shelleyOutput :: TxOut AlonzoEra
shelleyOutput = AlonzoTxOut shelleyAddr (Val.inject $ Coin 2) SNothing

utxo :: UTxO AlonzoEra
utxo = UTxO $ Map.fromList [(byronInput, byronOutput), (shelleyInput, shelleyOutput)]

txb :: TxIn -> TxOut AlonzoEra -> TxBody AlonzoEra
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

txEx :: TxIn -> TxOut AlonzoEra -> Tx AlonzoEra
txEx i o = MkAlonzoTx $ AlonzoTx (txb i o) mempty (IsValid True) SNothing

silentlyIgnore :: Tx AlonzoEra -> Expectation
silentlyIgnore tx =
  let lti =
        LedgerTxInfo
          { ltiProtVer = BT.ProtVer (eraProtVerLow @AlonzoEra) 0
          , ltiEpochInfo = ei
          , ltiSystemStart = ss
          , ltiUTxO = utxo
          , ltiTx = tx
          }
   in case toPlutusTxInfo SPlutusV1 lti of
        Right _ -> pure ()
        Left e -> expectationFailure $ "no translation error was expected, but got: " <> show e

-- | The test checks that the old implementation of 'transVITime' stays intentionally incorrect,
-- by returning close upper bound of the validaty interval.
transVITimeUpperBoundIsClosed :: Expectation
transVITimeUpperBoundIsClosed = do
  let interval = ValidityInterval SNothing (SJust (SlotNo 40))
  case transValidityInterval (Proxy @AlonzoEra) ei ss interval of
    Left (e :: ContextError AlonzoEra) ->
      expectationFailure $ "no translation error was expected, but got: " <> show e
    Right t ->
      t
        `shouldBe` PV1.Interval
          (PV1.LowerBound PV1.NegInf True)
          (PV1.UpperBound (PV1.Finite (PV1.POSIXTime 40000)) True)

spec :: Spec
spec = describe "txInfo translation" $ do
  -- TODO: convert to Imp: https://github.com/IntersectMBO/cardano-ledger/issues/5210
  describe "Plutus V1" $ do
    it "silently ignore byron txout" $
      silentlyIgnore (txEx shelleyInput byronOutput)
    it "silently ignore byron txin" $
      silentlyIgnore (txEx byronInput shelleyOutput)
  describe "transVITime" $ do
    it "validity interval's upper bound is closed when protocol < 9" $
      transVITimeUpperBoundIsClosed

genesisId :: TxId
genesisId = TxId (unsafeMakeSafeHash (mkDummyHash (0 :: Int)))
