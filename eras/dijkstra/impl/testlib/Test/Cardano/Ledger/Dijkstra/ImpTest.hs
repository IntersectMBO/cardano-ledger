{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest (
  module Test.Cardano.Ledger.Conway.ImpTest,
  DijkstraEraImp,
  impDijkstraSatisfyNativeScript,
  fixupSubTransactions,
  balanceSubTransactions,
) where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Governance (ConwayEraGov (..), committeeMembersL)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra (ApplyTxError, DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraNativeScript,
  evalDijkstraNativeScript,
  pattern RequireGuard,
 )
import Cardano.Ledger.Dijkstra.UTxO
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.API (mkStAnnTx)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.Val
import Control.Monad.State (gets)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Dijkstra.Era
import Test.Cardano.Ledger.Dijkstra.Examples (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Imp.Common

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure exampleDijkstraGenesis

  initNewEpochState = defaultInitNewEpochState $ \nes ->
    nes
      & nesEsL . epochStateGovStateL . committeeGovStateL %~ fmap updateCommitteeExpiry
    where
      updateCommitteeExpiry =
        committeeMembersL
          %~ fmap (const $ addEpochInterval (impEraStartEpochNo @DijkstraEra) (EpochInterval 15))

  impSatisfyNativeScript = impDijkstraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = dijkstraFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess
  modifyImpInitProtVer = conwayModifyImpInitProtVer
  genRegTxCert = dijkstraGenRegTxCert
  genUnRegTxCert = dijkstraGenUnRegTxCert
  delegStakeTxCert = conwayDelegStakeTxCert

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3
      <> plutusTestScripts SPlutusV4

instance BabbageEraImp DijkstraEra

instance ConwayEraImp DijkstraEra

class
  ( ConwayEraImp era
  , DijkstraEraTest era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  , InjectRuleFailure "LEDGER" EntitiesPredFailure era
  , InjectRuleFailure "LEDGER" SubEntitiesPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraUtxowPredFailure era
  , InjectRuleFailure "MEMPOOL" DijkstraMempoolPredFailure era
  , InjectRuleFailure "MEMPOOL" DijkstraUtxoPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraSubUtxoPredFailure era
  , Inject (NonEmpty (Conway.PredicateFailure (EraRule "MEMPOOL" era))) (ApplyTxError era)
  ) =>
  DijkstraEraImp era

instance DijkstraEraImp DijkstraEra

-- Partial implementation used for checking predicate failures
instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure @"ENTITIES"

instance InjectRuleFailure "ENTITIES" Shelley.ShelleyDelegPredFailure DijkstraEra where
  injectFailure = CertsFailure . injectFailure @"CERTS"

instance InjectRuleFailure "CERTS" Shelley.ShelleyDelegPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure . injectFailure

instance InjectRuleFailure "CERT" Shelley.ShelleyDelegPredFailure DijkstraEra where
  injectFailure = Conway.DelegFailure . injectFailure

instance InjectRuleFailure "DELEG" Shelley.ShelleyDelegPredFailure DijkstraEra where
  injectFailure (Shelley.DelegAccountAlreadyRegistered c) = Conway.DelegAccountAlreadyRegistered c
  injectFailure (Shelley.StakeKeyNotRegisteredDELEG c) = Conway.StakeKeyNotRegisteredDELEG c
  injectFailure (Shelley.StakeKeyNonZeroAccountBalanceDELEG c) = Conway.StakeKeyHasNonZeroAccountBalanceDELEG c
  injectFailure _ = error "Cannot inject ShelleyDelegPredFailure into DijkstraEra"

instance InjectRuleFailure "LEDGER" DijkstraSubUtxoPredFailure DijkstraEra where
  injectFailure =
    injectFailure @"LEDGER" @DijkstraSubLedgersPredFailure
      . SubLedgerFailure
      . SubUtxowFailure
      . SubUtxoFailure

impDijkstraSatisfyNativeScript ::
  ( DijkstraEraImp era
  , NativeScript era ~ DijkstraNativeScript era
  ) =>
  Set.Set (KeyHash Witness) ->
  TxBody l era ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash Witness) (KeyPair Witness)))
impDijkstraSatisfyNativeScript providedVKeyHashes txBody script = do
  let vi = txBody ^. vldtTxBodyL
  let guards = txBody ^. guardsTxBodyL
  case script of
    RequireSignature keyHash -> impSatisfySignature keyHash providedVKeyHashes
    RequireAllOf ss -> impSatisfyMNativeScripts providedVKeyHashes txBody (length ss) ss
    RequireAnyOf ss -> do
      m <- frequency [(9, pure 1), (1, choose (1, length ss))]
      impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    RequireMOf m ss -> impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    lock@(RequireTimeStart _)
      | evalDijkstraNativeScript mempty vi guards lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    lock@(RequireTimeExpire _)
      | evalDijkstraNativeScript mempty vi guards lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    -- TODO: actual satisfy the native scripts by updating the transaction's guards
    ns@(RequireGuard _)
      | evalDijkstraNativeScript mempty vi guards ns -> pure $ Just mempty
      | otherwise -> pure Nothing
    _ -> error "Impossible: All NativeScripts should have been accounted for"

dijkstraGenRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential Staking ->
  ImpTestM era (TxCert era)
dijkstraGenRegTxCert stakingCredential =
  RegDepositTxCert stakingCredential
    <$> getsNES (nesEsL . curPParamsEpochStateL . ppKeyDepositL)

dijkstraGenUnRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential Staking ->
  ImpTestM era (TxCert era)
dijkstraGenUnRegTxCert stakingCredential = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  deposit <- case lookupAccountState stakingCredential accounts of
    Nothing -> getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
    Just accountState -> pure (fromCompact (accountState ^. depositAccountStateL))
  pure $ UnRegDepositTxCert stakingCredential deposit

dijkstraFixupTx ::
  ( HasCallStack
  , DijkstraEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
dijkstraFixupTx tx = do
  isLegacy <- detectLegacyMode tx
  fixedUp <- fixupSubTransactions tx
  balancedInLegacy <- if isLegacy then balanceSubTransactions fixedUp else pure fixedUp
  babbageFixupTx balancedInLegacy

detectLegacyMode ::
  DijkstraEraImp era =>
  Tx TopTx era ->
  ImpTestM era Bool
detectLegacyMode tx = do
  Globals {systemStart, epochInfo} <- gets (^. impGlobalsL)
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  let stAnnTx = mkStAnnTx epochInfo systemStart pp utxo mempty tx
  pure $ stAnnTx ^. plutusLegacyModeStAnnTxG

fixupSubTransactions ::
  ( HasCallStack
  , DijkstraEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
fixupSubTransactions tx = impAnn "fixupSubTransactions" $ do
  fixedup <-
    traverse
      fixupSubTransaction
      (OMap.elems (tx ^. bodyTxL . subTransactionsTxBodyL))
  pure $ tx & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable fixedup
  where
    fixupSubTransaction =
      addSubTxIn
        >=> addNativeScriptTxWits
        >=> fixupAuxDataHash
        >=> fixupTxOuts
        >=> updateAddrTxWits
    addSubTxIn subTx
      | not (Set.null (subTx ^. bodyTxL . inputsTxBodyL)) = pure subTx
      | otherwise = do
          addr <- freshKeyAddr_
          -- restore default fixup behaviour temporarily,
          -- to make sure it isn't affected by any higher-level fixup modifications
          newTxIn <- withFixup fixupTx $ sendCoinTo addr (Coin 1_000_000)
          pure $ subTx & bodyTxL . inputsTxBodyL .~ Set.singleton newTxIn

balanceSubTransactions ::
  DijkstraEraImp era =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
balanceSubTransactions topTx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  pools <- Map.keysSet <$> getsNES (nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL)
  utxo <- getUTxO
  let
    subTransactions = topTx ^. bodyTxL . subTransactionsTxBodyL
    subsCerts = foldMap' (^. bodyTxL . certsTxBodyL) subTransactions
    subsConsumed = coin $ foldMap' (dijkstraConsumed pp utxo . (^. bodyTxL)) subTransactions
    subsProduced =
      foldMap' (coin . localProducedValue pp . (^. bodyTxL)) subTransactions
        <> getTotalDepositsTxCerts pp (`Set.member` pools) subsCerts
  balancer <- mkBalancerSubTx subsConsumed subsProduced
  case balancer of
    Nothing -> pure topTx
    Just b -> pure $ topTx & bodyTxL . subTransactionsTxBodyL %~ (OMap.|> b)

mkBalancerSubTx ::
  DijkstraEraImp era =>
  -- | Cumulated consumed value by all sub-transactions
  Coin ->
  -- | Cumulated produced value by all sub-transactions
  Coin ->
  ImpTestM era (Maybe (Tx SubTx era))
mkBalancerSubTx consumed produced = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  case consumed `compare` produced of
    EQ -> pure Nothing
    ord -> do
      addr <- freshKeyAddr_
      let
        (surplus, shortfall) = case ord of
          GT -> (consumed <-> produced, mempty)
          LT -> (mempty, produced <-> consumed)
        -- a buffer to make both the input UTxO and the change output satisfy minCoin. It's added on both sides, so it cancels out.
        minChangeCoin = ensureMinCoinTxOut pp (mkBasicTxOut addr mempty) ^. coinTxOutL
        inputCoin = minChangeCoin <> shortfall
        changeCoin = minChangeCoin <> surplus
        changeOut = mkBasicTxOut addr (inject changeCoin)
      newTxIn <- withFixup fixupTx $ sendCoinTo addr inputCoin
      pure . Just $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ [newTxIn]
          & bodyTxL . outputsTxBodyL .~ [changeOut]
