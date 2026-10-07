{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.ReceivingActivationSpec (spec) where

import Cardano.Ledger.BaseTypes (Network (..), ProtVer (..))
import Cardano.Ledger.Binary (decCBOR, decodeFullDecoder, natVersion, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra (ApplyTxError (..), DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.API.Mempool (applyTxWithFullValidation, mkMempoolEnv)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.State
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.Examples (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum)

-- This exercises ledger translation and restoration, not a node hard-fork
-- schedule, block replay, wallet workflow or release approval.
spec :: Spec
spec = withImpInit @(LedgerSpec ConwayEra)
  $ describe "Receiving ledger activation boundary"
  $ it
    "carries populated Conway state and a signed pending transaction through translation, then restores and spends a protected output"
  $ do
    payment <- freshKeyHash @Payment
    stake <- freshKeyHash @Staking
    _ <- registerStakeCredential (KeyHashObj stake)
    let ordinary = Addr Testnet (KeyHashObj payment) (StakeRefBase (KeyHashObj stake))
        protected = AddrProtected Testnet (KeyHashObj payment) (StakeRefBase (KeyHashObj stake))
    input <- sendCoinTo ordinary (Coin 20_000_000)
    native <- impAddNativeScript (RequireSignature (asWitness payment))
    _ <- produceScript native
    _ <- produceScript (hashPlutusScript (alwaysSucceedsNoDatum SPlutusV3))
    pendingTx <-
      fixupTx $
        mkBasicTx $
          mkBasicTxBody
            & inputsTxBodyL
              .~ [input]
            & outputsTxBodyL
              .~ [mkCoinTxOut ordinary (Coin 5_000_000)]
    source <- getsNES id
    globals <- gets (^. impGlobalsL)
    slot <- gets (^. impCurSlotNoG)
    let sourceLedger = source ^. nesEsL . esLStateL
    _ <-
      expectRight $
        applyTxWithFullValidation globals (mkMempoolEnv source slot) sourceLedger pendingTx

    migratedTx <- expectRight $ runExcept $ translateEra @DijkstraEra exampleDijkstraGenesis pendingTx
    let version = natVersion @12
        migrated =
          translateEra' @DijkstraEra exampleDijkstraGenesis source
            & nesEsL
              . curPParamsEpochStateL
              . ppProtocolVersionL
              .~ ProtVer version 0
        migratedLedger = migrated ^. nesEsL . esLStateL
        environment = mkMempoolEnv migrated slot
    -- Translation does not promise whole-transaction byte identity. Check
    -- this ordinary fixture's actual memoized body bytes before asserting
    -- that its original signatures survive, then validate those witnesses.
    originalBytes (migratedTx ^. bodyTxL) `shouldBe` originalBytes (pendingTx ^. bodyTxL)
    txIdTx migratedTx `shouldBe` txIdTx pendingTx
    serialize version (migrated ^. utxoL) `shouldBe` serialize version (source ^. utxoL)
    -- Pool VRF bookkeeping is deliberately populated by the existing era
    -- translation. Compare the ordinary account/delegation state directly.
    serialize version (migratedLedger ^. lsCertStateL . certDStateL)
      `shouldBe` serialize version (sourceLedger ^. lsCertStateL . certDStateL)
    serialize version (migrated ^. instantStakeG) `shouldBe` serialize version (source ^. instantStakeG)
    _ <- expectRight $ applyTxWithFullValidation globals environment migratedLedger migratedTx

    let protectedUnsigned =
          migratedTx
            & bodyTxL
              . outputsTxBodyL
              %~ \outputs -> case outputs of
                SSeq.Empty -> SSeq.Empty
                out SSeq.:<| rest -> (out & addrTxOutL .~ protected) SSeq.:<| rest
    case applyTxWithFullValidation globals environment migratedLedger protectedUnsigned of
      Left (DijkstraApplyTxError failures) ->
        assertBool "Changing protection did not reject the stale signature" $
          any isInvalidSignature failures
      Right _ -> assertFailure "Changing protection preserved an obsolete signature"
    pairs <- mapM (getKeyPair . witVKeyHash) (Set.toList (pendingTx ^. witsTxL . addrTxWitsL))
    let protectedTx =
          protectedUnsigned
            & witsTxL
              . addrTxWitsL
              .~ mkWitnessesVKey (hashAnnotated (protectedUnsigned ^. bodyTxL)) pairs
        preactivation = migrated & nesEsL . curPParamsEpochStateL . ppProtocolVersionL .~ ProtVer (natVersion @11) 0
    case applyTxWithFullValidation globals (mkMempoolEnv preactivation slot) migratedLedger protectedTx of
      Left (DijkstraApplyTxError failures) ->
        assertBool "Preactivation did not reject protected output admission" $
          Dijkstra.LedgerFailure
            ( injectFailure @"LEDGER" @Dijkstra.DijkstraUtxoPredFailure @DijkstraEra
                (Dijkstra.UnsupportedOutputAddresses (NES.singleton 0))
            )
            `elem` failures
      Right _ -> assertFailure "Protected transaction was admitted before activation"
    (createdLedger, _) <-
      expectRight $ applyTxWithFullValidation globals environment migratedLedger protectedTx
    let created = migrated & nesEsL . esLStateL .~ createdLedger
        protectedInput = txInAt 0 protectedTx
    Map.lookup protectedInput (unUTxO (created ^. utxoL))
      `shouldBe` SSeq.lookup 0 (protectedTx ^. bodyTxL . outputsTxBodyL)
    assertBool "Replaying the protected transaction was accepted" $
      isLeft $
        applyTxWithFullValidation globals environment createdLedger protectedTx

    restoredResult <- liftIO $
      withSystemTempFile "receiving-activation-nes.cbor" $ \path handle -> do
        BSL.hPut handle (serialize version created)
        hClose handle
        bytes <- BSL.readFile path
        pure $ decodeFullDecoder version "complete Dijkstra NewEpochState" decCBOR bytes
    restored <- expectRight restoredResult
    serialize version restored `shouldBe` serialize version created
    pair <- getKeyPair (asWitness payment)
    let Coin fee = pendingTx ^. bodyTxL . feeTxBodyL <> Coin 100_000
        spendingBody =
          mkBasicTxBody @DijkstraEra @TopTx
            & inputsTxBodyL
              .~ [protectedInput]
            & outputsTxBodyL
              .~ [mkCoinTxOut ordinary (Coin (5_000_000 - fee))]
            & feeTxBodyL
              .~ Coin fee
        spending =
          mkBasicTx spendingBody
            & witsTxL
              . addrTxWitsL
              .~ mkWitnessesVKey (hashAnnotated spendingBody) [pair]
    (spentLedger, _) <-
      expectRight $
        applyTxWithFullValidation
          globals
          (mkMempoolEnv restored slot)
          (restored ^. nesEsL . esLStateL)
          spending
    Map.member protectedInput (unUTxO (spentLedger ^. utxoG)) `shouldBe` False
  where
    isInvalidSignature :: Dijkstra.DijkstraMempoolPredFailure DijkstraEra -> Bool
    isInvalidSignature (Dijkstra.LedgerFailure (Dijkstra.DijkstraUtxowFailure (Dijkstra.InvalidWitnessesUTXOW _))) = True
    isInvalidSignature _ = False
