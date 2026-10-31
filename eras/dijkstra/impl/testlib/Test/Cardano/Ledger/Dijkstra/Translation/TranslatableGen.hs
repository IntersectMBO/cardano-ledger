{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Translation.TranslatableGen (
  genTranslatableTxWithNestedTxs,
) where

import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo, SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (toAsIx)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (TxDats), unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.State (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Mary.Value (AssetName, PolicyID (..), multiAssetFromList)
import Cardano.Ledger.Plutus (hashPlutusScript)
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (forM, (>=>))
import Control.Monad.Trans.Fail.String (errorFail)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen (genTxOut)
import Test.Cardano.Ledger.Common (
  Arbitrary,
  Gen,
  arbitrary,
  frequency,
  oneof,
  scale,
  sublistOf,
  vectorOf,
 )
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  alwaysSucceedsWithDatum,
  evenRedeemerNoDatum,
  evenRedeemerWithDatum,
  purposeIsWellformedNoDatum,
  redeemerSameAsDatum,
 )

-- | Generate a top-level transaction together with its sub-transactions (if any) and
-- a 'UTxO' that makes the whole batch translatable (by functions like 'transGuardingTopTxInfo').
--
-- Some properties of the generated transactions:
--   * every input/reference input across the batch resolves in the 'UTxO',
--   * no output anywhere in the batch carries a 'StakeRefPtr' or a Byron address,
--   * inputs never overlap with reference inputs within the same transaction,
--   * reference inputs, datums, withdrawal and direct deposit account addresses, guard and
--     required top-level guard credentials, and minted asset ids are often shared across the
--     top transaction field and nested transactions, so that their aggregation is exercised; an
--     asset id is sometimes minted in one transaction and burned in another.
--   * sub-transactions often vote again, differently, on some of the @(voter,
--     governance action)@ pairs of the top-level transaction or of other
--     sub-transactions, so that which vote wins across the batch is exercised. It
--     also exercises votes one a new governance action for a known voter, so that
--     votes are merged per voter.
--   * sometimes no transaction of the batch has an upper bound (or, alternatively, a lower
--     bound) in its validity interval, so that a bound absent from the whole batch is exercised,
--   * every redeemer points at a script that its transaction both needs and provides, from a
--     small pool of PlutusV4 scripts, so that the same script is often executed in more than
--     one transaction, and sometimes for two purposes (guarding and minting) in one transaction.
--     Only scripts for guarding and minting purposes are executed; the
--     translation of the other purposes is left to the unit tests of
--     'transPlutusPurposeV4'.
genTranslatableTxWithNestedTxs ::
  forall era.
  ( DijkstraEraTxBody era
  , AlonzoEraTxWits era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusTxInfo PlutusV4 era
  , Arbitrary (Tx TopTx era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) =>
  Gen (Tx TopTx era, UTxO era)
genTranslatableTxWithNestedTxs = do
  -- Sub-transactions are generated at the same size as the top-level transaction, and there
  -- can be up to 4 of them, so scale down to keep the generation of the batch fast.
  topTx <- scale (`div` 4) arbitrary
  -- Pools of values for the different tx fields. A subset of those values is
  -- added to the fields in the nested transactions so that the same value often
  -- appears in more than one transaction.
  sharedAccountAddresses <- arbitrary
  sharedGuardCreds <- arbitrary
  sharedDats <- arbitrary
  sharedAssetIds <- arbitrary
  sharedReferenceInputs <- arbitrary
  -- Bounds that are absent from every transaction of the batch. Each bound is otherwise
  -- independently present or absent in every transaction, so a bound is essentially never
  -- absent from the whole batch.
  maybeClearBounds <-
    frequency
      [ (3, pure id)
      , (1, pure $ invalidHereAfterL .~ SNothing)
      , (1, pure $ invalidBeforeL .~ SNothing)
      ]
  let subTxs = OMap.elems $ topTx ^. bodyTxL . subTransactionsTxBodyL
      knownVotes =
        topTx ^. bodyTxL . votingProceduresTxBodyL
          <> foldMap (\tx -> tx ^. bodyTxL . votingProceduresTxBodyL) subTxs
      -- Steps applied to top or sub transactions, in order.
      -- 'makeTxTranslatable' must come last.
      fixTx :: Tx l era -> Gen (Tx l era)
      fixTx =
        pure . (bodyTxL . vldtTxBodyL %~ maybeClearBounds)
          >=> addKnownReferenceInputs sharedReferenceInputs
          >=> addKnownMints sharedAssetIds
          >=> addKnownDats sharedDats
          >=> addKnownGuards sharedGuardCreds
          >=> addKnownRequiredTopLevelGuards sharedGuardCreds
          >=> addKnownDirectDeposits sharedAccountAddresses
          >=> addKnownWithdrawals sharedAccountAddresses
          >=> addPredefinedScripts
          >=> makeTxTranslatable
  fixedSubTxs <- forM subTxs (revote knownVotes >=> fixTx)
  fixedTopTx <-
    fixTx $ topTx & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable fixedSubTxs
  let allInputs = Set.toList $ fixedTopTx ^. bodyTxL . allInputsTxBodyF
  outputs <- vectorOf (length allInputs) $ genTxOut @era $ SupportedLanguage SPlutusV4
  let utxo = UTxO (Map.fromList $ allInputs `zip` outputs)
  subTxsWithRedeemers <-
    forM (OMap.elems $ fixedTopTx ^. bodyTxL . subTransactionsTxBodyL) $
      addRedeemers utxo (Map.keysSet predefinedScripts)
  topTxWithRedeemers <-
    addRedeemers utxo (Map.keysSet predefinedScripts) $
      fixedTopTx & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable subTxsWithRedeemers
  pure (topTxWithRedeemers, utxo)
  where
    -- A small pool of distinct scripts, so that their hashes are often shared across the batch
    predefinedScripts :: Map.Map ScriptHash (Script era)
    predefinedScripts =
      Map.fromList
        [ (hashPlutusScript plutus, fromPlutusScript $ errorFail $ mkPlutusScript plutus)
        | plutus <-
            [ alwaysSucceedsNoDatum SPlutusV4
            , alwaysSucceedsWithDatum SPlutusV4
            , evenRedeemerNoDatum SPlutusV4
            , evenRedeemerWithDatum SPlutusV4
            , redeemerSameAsDatum SPlutusV4
            , purposeIsWellformedNoDatum SPlutusV4
            ]
        ]
    -- Fix the generated transaction by:
    --   * replacing its outputs with freshly generated ones, so they never carry a 'StakeRefPtr' or
    --     a Byron address,
    --   * making its reference inputs disjoint from its inputs.
    makeTxTranslatable :: Tx l era -> Gen (Tx l era)
    makeTxTranslatable tx = do
      let txBody = tx ^. bodyTxL
      newTxOutputs <-
        vectorOf (length (txBody ^. outputsTxBodyL)) $ genTxOut @era $ SupportedLanguage SPlutusV4
      let fixedTxBody =
            txBody
              & outputsTxBodyL .~ StrictSeq.fromList newTxOutputs
              & referenceInputsTxBodyL
                .~ Set.difference (txBody ^. referenceInputsTxBodyL) (txBody ^. inputsTxBodyL)
      pure $ tx & bodyTxL .~ fixedTxBody
    -- Provide a sublist of the predefined scripts, and make the transaction need each of them as a
    -- guard and sometimes also as a minting policy, so that a script can be executed for two
    -- purposes in one transaction.
    addPredefinedScripts :: Tx l era -> Gen (Tx l era)
    addPredefinedScripts tx = do
      scriptSubset <- Map.fromList <$> sublistOf (Map.toList predefinedScripts)
      mintingHashes <- sublistOf (Map.keys scriptSubset)
      addedMintValues <-
        multiAssetFromList
          <$> traverse (\h -> (PolicyID h,,) <$> arbitrary <*> arbitrary) mintingHashes
      pure $
        tx
          & witsTxL . scriptTxWitsL %~ (<> scriptSubset)
          & bodyTxL . guardsTxBodyL %~ (<> OSet.fromList (ScriptHashObj <$> Map.keys scriptSubset))
          & bodyTxL . mintTxBodyL %~ (<> addedMintValues)
    -- Replace the redeemers of the transaction with redeemers for a sublist of
    -- its needed purposes whose script is one of the predefined scripts.
    -- Arbitrary generated redeemers almost never point at a script that is both
    -- needed and provided, which fails the translation with
    -- 'ScriptHashNotFoundForPurpose'.
    addRedeemers :: UTxO era -> Set.Set ScriptHash -> Tx l era -> Gen (Tx l era)
    addRedeemers utxo scriptHashes tx = do
      let AlonzoScriptsNeeded needed = getScriptsNeeded utxo (tx ^. bodyTxL)
      purposes <-
        sublistOf
          [hoistPlutusPurpose toAsIx purpose | (purpose, sh) <- needed, sh `Set.member` scriptHashes]
      redeemers <- traverse (\purpose -> (purpose,) <$> arbitrary) purposes
      pure $ tx & witsTxL . rdmrsTxWitsL .~ Redeemers (Map.fromList redeemers)
    -- Half of the time, make the sub-transaction vote (with freshly generated votes) on:
    --   * a sublist of the (voter, governance action) pairs voted on by the top-level
    --     transaction or by any sub-transaction, so that which vote wins is exercised, between
    --     the top-level transaction and a sub-transaction, and between two sub-transactions;
    --   * a new governance action for a sublist of the known voters, so that votes are merged
    --     per voter, and not only per @(voter, governance action)@ pair.
    -- Independently generated votes essentially never share a voter.
    revote :: VotingProcedures era -> Tx SubTx era -> Gen (Tx SubTx era)
    revote knownVotes subTx =
      oneof
        [ pure subTx
        , do
            revotedPairs <-
              sublistOf
                [ (voter, gaid)
                | (voter, gaids) <- Map.toList (unVotingProcedures knownVotes)
                , gaid <- Map.keys gaids
                ]
            newGovActionPairs <-
              sublistOf (Map.keys $ unVotingProcedures knownVotes)
                >>= traverse (\voter -> (voter,) <$> arbitrary)
            votes <- traverse (\key -> (key,) <$> arbitrary) (revotedPairs <> newGovActionPairs)
            let addedVotes =
                  VotingProcedures $
                    Map.fromListWith
                      Map.union
                      [(voter, Map.singleton gaid vp) | ((voter, gaid), vp) <- votes]
            pure $ subTx & bodyTxL . votingProceduresTxBodyL %~ (<> addedVotes)
        ]

    addKnownWithdrawals :: [AccountAddress] -> Tx l era -> Gen (Tx l era)
    addKnownWithdrawals accountAddressPool tx = do
      aas <- sublistOf accountAddressPool
      addedWithdrawals <-
        Withdrawals . Map.fromList
          <$> traverse (\aa -> (aa,) <$> arbitrary) aas
      pure $ tx & bodyTxL . withdrawalsTxBodyL %~ (<> addedWithdrawals)

    addKnownDirectDeposits :: [AccountAddress] -> Tx l era -> Gen (Tx l era)
    addKnownDirectDeposits accountAddressPool tx = do
      aas <- sublistOf accountAddressPool
      addedDirectDeposits <-
        DirectDeposits . Map.fromList
          <$> traverse (\aa -> (aa,) <$> arbitrary) aas
      pure $ tx & bodyTxL . directDepositsTxBodyL %~ (<> addedDirectDeposits)

    addKnownGuards :: [Credential Guard] -> Tx l era -> Gen (Tx l era)
    addKnownGuards guardCredPool tx = do
      creds <- sublistOf guardCredPool
      let addedGuards = OSet.fromList creds
      pure $ tx & bodyTxL . guardsTxBodyL %~ (<> addedGuards)

    addKnownRequiredTopLevelGuards :: [Credential Guard] -> Tx l era -> Gen (Tx l era)
    addKnownRequiredTopLevelGuards guardCredPool tx = do
      creds <- sublistOf guardCredPool
      addedRequiredTopLevelGuards <-
        Map.fromList
          <$> traverse (\cred -> (cred,) <$> arbitrary) creds
      pure $ tx & bodyTxL . requiredTopLevelGuardsTxBodyL %~ (<> addedRequiredTopLevelGuards)

    addKnownReferenceInputs :: [TxIn] -> Tx l era -> Gen (Tx l era)
    addKnownReferenceInputs referenceInputPool tx = do
      addedReferenceInputs <- Set.fromList <$> sublistOf referenceInputPool
      pure $ tx & bodyTxL . referenceInputsTxBodyL %~ (<> addedReferenceInputs)

    addKnownDats :: TxDats era -> Tx l era -> Gen (Tx l era)
    addKnownDats datPool tx = do
      addedDats <- TxDats . Map.fromList <$> sublistOf (Map.toList $ unTxDats datPool)
      pure $ tx & witsTxL . datsTxWitsL %~ (<> addedDats)

    addKnownMints :: [(PolicyID, AssetName)] -> Tx l era -> Gen (Tx l era)
    addKnownMints assetIdPool tx = do
      mas <- sublistOf assetIdPool
      addedMints <- multiAssetFromList <$> traverse (\(pid, an) -> (pid,an,) <$> arbitrary) mas
      pure $ tx & bodyTxL . mintTxBodyL %~ (<> addedMints)
