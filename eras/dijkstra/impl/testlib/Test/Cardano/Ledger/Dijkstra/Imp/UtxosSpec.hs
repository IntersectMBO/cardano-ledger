{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..), TxIx (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (Constitution (..), GovAction (..), Voter (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core (
  DijkstraEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  KeyRole (..),
  txIdTx,
 )
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.TxIn (TxIn (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Common (SpecWith, describe, it)
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  LedgerSpec,
  enactConstitution,
  freshKeyAddrNoPtr_,
  genRegTxCert,
  getCommitteeMembers,
  mkProposal,
  mkTokenMintingTx,
  registerAccountAddress,
  registerCommitteeHotKeys,
  registerInitialCommittee,
  registerStakeCredential,
  setupSingleDRep,
  submitGovAction,
  submitProposal_,
  submitTxAnn,
  submitTxAnn_,
  submitYesVote_,
 )
import Test.Cardano.Ledger.Imp.Common (arbitrary, choose, mkAddr, void)
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus" $ do
    describe "purposeIsWellformedNoDatum" $ do
      it "Passes with spending purpose" $ do
        val <- Coin <$> choose (2_000_000, 8_000_000)
        sCred <- arbitrary @(Credential Staking)
        let
          plutusScript = purposeIsWellformedNoDatum SPlutusV4
          sh = hashPlutusScript plutusScript
          addr = mkAddr (ScriptHashObj @Payment sh) sCred
          txOut = mkBasicTxOut addr (inject val)
        tx <-
          submitTxAnn "Produce script-locked output" $
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ SSeq.singleton txOut
        submitTxAnn_ "Use locked output" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton (TxIn (txIdTx tx) (TxIx 0))
      it "Passes with minting purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        tx <- mkTokenMintingTx sh
        -- PlutusV4 rejects stake pointers in outputs, so replace the generated
        -- output address with one that can't contain a pointer
        addr <- freshKeyAddrNoPtr_
        submitTxAnn_ "Mint tokens with script policy" $
          tx & bodyTxL . outputsTxBodyL %~ fmap (addrTxOutL .~ addr)
      it "Passes with certifying purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        txCert <- genRegTxCert $ ScriptHashObj sh
        submitTxAnn_ "Register script staking credential" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ SSeq.singleton txCert
      it "Passes with withdrawing purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        account <- registerStakeCredential $ ScriptHashObj sh
        submitTxAnn_ "Withdraw from script-controlled account" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . withdrawalsTxBodyL .~ Withdrawals (Map.singleton account mempty)
      it "Passes with voting purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        coldCred : _ <- Set.toList <$> getCommitteeMembers
        hotCred :| _ <- registerCommitteeHotKeys (pure $ ScriptHashObj sh) (coldCred :| [])
        gaId <- submitGovAction InfoAction
        submitYesVote_ (CommitteeVoter hotCred) gaId
      it "Passes with proposing purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        committeeMembers <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        anchor <- arbitrary
        void $ enactConstitution SNothing (Constitution anchor (SJust sh)) dRep committeeMembers
        account <- registerAccountAddress
        proposal <- mkProposal $ TreasuryWithdrawals (Map.singleton account (Coin 1000)) (SJust sh)
        submitProposal_ proposal
      it "Passes with guarding purpose" $ do
        let sh = hashPlutusScript $ purposeIsWellformedNoDatum SPlutusV4
        submitTxAnn_ "Submit tx with a Plutus guard" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj sh)
