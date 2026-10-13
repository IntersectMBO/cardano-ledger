{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Translation.TranslatableGen (
  genTranslatableTxWithNestedTxs,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (SupportedLanguage (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Control.Monad ((<=<))
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen (genTxOut)
import Test.Cardano.Ledger.Common (Gen, arbitrary, oneof, vectorOf)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

-- TODO Plan for ttisRedeemerHashes:

-- Why redeemers are cleared today:
-- ltiScriptHashesUsed only contains purposes whose
-- script is both needed and provided
-- (resolveNeededPlutusScriptsWithPurpose in
-- mkTestLedgerTxInfo). A random redeemer therefore
-- fails with ScriptHashNotFoundForPurpose. Since
-- you can't produce a script for a given hash,
-- choose the scripts first and then make the
-- transactions need them.

-- Generator (TranslatableGen.hs):
-- 1. Use a small pool of distinct PlutusV4 scripts
--    so hashes collide often across the batch, e.g.
--    alwaysSucceedsNoDatum,
--    alwaysSucceedsWithDatum and
--    evenRedeemerNoDatum at SPlutusV4.
-- 2. In fixTx, pick a sublist of the pool and add
--    each script to witsTxL . scriptTxWitsL. Make
--    the body need it:
--    - add ScriptHashObj h to guardsTxBodyL (purely
--      additive, giving a GuardingPurpose);
--    - sometimes also mint under PolicyID h, so one
--      hash serves two purposes in one
--      transaction.

--    This changes the body, and therefore the TxId.
--    The existing OMap.fromFoldable fixedSubTxs
--    rebuild already handles that.
-- 3. After the UTxO is built, run a new
--    addRedeemers utxo tx step:
--    - compute AlonzoScriptsNeeded needed =
--      getScriptsNeeded utxo (tx ^. bodyTxL);
--    - keep the entries whose hash is in the pool,
--      and take a sublistOf;
--    - set rdmrsTxWitsL to Redeemers of
--      hoistPlutusPurpose toAsIx p ↦ (arbitrary,
--      arbitrary).

--    Redeemers don't affect txIdTx, so the OMap
--    keys stay valid; rebuild it once more anyway.
-- 4. Delete the "clear redeemers" bullet and update
--    the generator's Haddock.

-- Reference (translate-then-combine). This is
-- independent of production's txInfoRedeemers →
-- ScriptPurpose → scriptHashFromScriptPurpose path
-- and of ltiScriptHashesUsed:
-- ttisRedeemerHashes =
--   Set.toList . foldMapBatch $ \tx ->
--     let AlonzoScriptsNeeded needed =
-- getScriptsNeeded utxo (tx ^. bodyTxL)
--         withRedeemer = Map.keysSet (tx ^. witsTxL
-- . rdmrsTxWitsL . unRedeemersL)
--      in Set.fromList
-- withRedeemer]
-- The existing exact-list shouldBe then pins both
-- the deduplication and the ascending Plutus order.

-- Extras:
-- - A tiny separate property for production's claim
--   that the Plutus ScriptHash ordering equals the
--   ledger's: \(hs :: Set ScriptHash) -> map
--   transScriptHash (Set.toList hs) === Set.toList
--   (Set.map transScriptHash hs).
-- - Add the three redeemer covers from the table
--   above.
-- - Mutation check: replacing Set.toList .
--   Set.fromList in TxInfo.hs:915 with plain
--   concatenation must turn the test red.

-- Risk to check: getScriptsNeeded for a SubTx body
-- has to report GuardingPurpose the same way it
-- does for TopTx (Dijkstra/UTxO.hs:196). If it
-- doesn't, use only mint purposes in
-- sub-transactions.

-- | Generate a top-level transaction together with its sub-transactions (if any) and
-- a 'UTxO' that makes the whole batch translatable (by functions like 'transGuardingTopTxInfo').
--
-- Some properties of the generated transactions:
--   * every input/reference input across the batch resolves in the 'UTxO'
--   * no output anywhere in the batch carries a 'StakeRefPtr' or a Byron address,
--   * inputs never overlap with reference inputs within the same transaction,
--   * sub-transactions often vote again, differently, on the governance actions the top-level
--     transaction votes on, so that which vote wins across the batch is exercised.
--
-- TODO For withdrawals, direct deposits, required top-level guards, datums and
-- mints, keys should overlap accross transactions.
genTranslatableTxWithNestedTxs :: Gen (Tx TopTx DijkstraEra, UTxO DijkstraEra)
genTranslatableTxWithNestedTxs = do
  topTx <- arbitrary
  let subTxs = OMap.elems $ topTx ^. bodyTxL . subTransactionsTxBodyL
  fixedSubTxs <- mapM (makeTxTranslatable <=< revote (topTx ^. bodyTxL . votingProceduresTxBodyL)) subTxs
  fixedTopTx <- makeTxTranslatable $ topTx & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable fixedSubTxs
  let allInputs = Set.toList $ fixedTopTx ^. bodyTxL . allInputsTxBodyF
  outputs <- vectorOf (length allInputs) $ genTxOut @DijkstraEra $ SupportedLanguage SPlutusV4
  pure (fixedTopTx, UTxO (Map.fromList $ allInputs `zip` outputs))
  where
    -- Fix the generated transaction by:
    --   * replacing a transaction's outputs with freshly generated ones (so they never carry a 'StakeRefPtr' or a Byron address)
    --   * making its reference inputs disjoint from its inputs,
    --   * clear its redeemers: arbitrary redeemers almost never point at a real script need, so we get `ScriptHashNotFoundForPurpose` during the translation.
    makeTxTranslatable :: Tx l DijkstraEra -> Gen (Tx l DijkstraEra)
    makeTxTranslatable tx = do
      let txBody = tx ^. bodyTxL
      newTxOutputs <-
        vectorOf (length (txBody ^. outputsTxBodyL)) $ genTxOut @DijkstraEra $ SupportedLanguage SPlutusV4
      let fixedTxBody =
            txBody
              & outputsTxBodyL .~ StrictSeq.fromList newTxOutputs
              & referenceInputsTxBodyL
                .~ Set.difference (txBody ^. referenceInputsTxBodyL) (txBody ^. inputsTxBodyL)
      pure $
        tx
          & bodyTxL .~ fixedTxBody
          & witsTxL . rdmrsTxWitsL .~ Redeemers Map.empty
    -- Half of the time, make the sub-transaction vote again on every governance action the
    -- top-level transaction votes on, with freshly generated votes. Independently generated
    -- votes essentially never share a voter and a governance action.
    revote :: VotingProcedures DijkstraEra -> Tx SubTx DijkstraEra -> Gen (Tx SubTx DijkstraEra)
    revote topTxVotes subTx =
      oneof
        [ pure subTx
        , do
            reVoted <-
              VotingProcedures <$> traverse (traverse (const arbitrary)) (unVotingProcedures topTxVotes)
            pure $ subTx & bodyTxL . votingProceduresTxBodyL %~ (<> reVoted)
        ]
